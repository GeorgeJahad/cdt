;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; Contributors:
;; Travis Vachon

(ns cdt.break
  (:require [clojure.string :as str]
            [cdt.utils :as cdtu]
            [cdt.events :as cdte])
  (:import java.io.File
           java.util.regex.Pattern
           com.sun.jdi.request.EventRequest
           com.sun.jdi.event.BreakpointEvent))

(defn- create-bp [l]
  (doto (.createBreakpointRequest
         (.eventRequestManager (cdtu/vm)) l)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)))

(defn print-bps []
  (doseq [[n k] (keep-indexed vector (keys @cdte/bp-list))]
    (println n k)))

(defn- check-ns-loaded [sym]
  (let [ns (second (re-find  #"(.*)[:/]" (str sym)))
        class-regex (re-pattern (str (Compiler/munge ns) "((\\$)|$)"))]
    (when-not (seq (cdtu/find-classes class-regex))
      (throw (IllegalStateException.
              (str "Namespace "
                   ns " not loaded; bp can not be set until it is."))))))

(defn- create-thread-bp [thread location]
  (let [bp (create-bp location)]
    (cdte/set-thread-filter bp thread)
    (.setEnabled bp true)
    bp))

(defn- create-thread-bps [locations thread-list groups-to-skip]
  (into {} (for [t thread-list :when (cdte/valid-thread? t groups-to-skip)]
             (let [bps (doall (map (partial create-thread-bp t) locations))]
               (when (seq bps)
                 [t bps])))))

(defn- create-bps
  ([locations]
   (let [bps (map create-bp locations)]
     (when (seq bps)
       (doseq [bp bps]
         (.setEnabled bp true))
       {:all bps
        :locations locations})))
  ([locations thread-list groups-to-skip add-new-threads?]
   (let [bps (create-thread-bps locations
                                thread-list groups-to-skip)]
     (when (seq bps)
       {:add-new-threads? add-new-threads?
        :locations locations
        :groups-to-skip groups-to-skip
        :thread-specific bps}))))

(defn delete-bp-fn [sym]
  (doseq [bps (cdte/sym-event-seq sym cdte/bp-list) bp bps]
    (.setEnabled bp false)
    (.deleteEventRequest (.eventRequestManager (cdtu/vm)) bp))
  (swap! cdte/bp-list dissoc sym))

(defn- merge-with-exception [sym]
  (fn [m1 m2]
    (merge-with
     (fn [a b] (delete-bp-fn sym) b)
     m1 m2)))

(defn- set-bp-locations [sym locations thread-args]
  (check-ns-loaded sym)
  (when-let [bps (apply create-bps locations thread-args)]
    (println (cdtu/cdt-display-msg (str "bp set on " (seq locations))))
    (swap! cdte/bp-list
           (merge-with-exception sym) {sym bps})))

(defn- gen-class-pattern [sym]
  (let [s (cdtu/munge-sym sym)]
    (re-pattern (str "^" s "$"))))

(defn- get-methods [sym]
  (for [c (cdtu/find-classes (gen-class-pattern sym))
        m (cdtu/regex-filter #"(invoke|doInvoke)" (.methods c))] m))

(defn set-bp-sym [sym thread-args]
  (let [methods (get-methods sym)]
    (when-not (set-bp-locations sym (map #(.location %) methods) thread-args)
      (println "no methods found for" sym))))


(defn- java-fname? [fname]
  (boolean (.endsWith fname ".java")))

(defn- append-dollar [fname s]
  (if (java-fname? fname)
    s
    (re-pattern (str s "\\$"))))

(defn- fix-class [c]
  (str/replace c File/separator "."))

(defn- get-file-class [fname path]
  (let [path-literal-regex-str (str (Pattern/quote (str path File/separator)) "(.*)(.clj|.java)")]
    (second (re-find (re-pattern path-literal-regex-str) 
            fname))))

(defn- remove-suffix [fname]
  (first (.split fname "\\.")))

(defn- get-jar-class [fname path]
  (when-let [short-name (cdtu/get-jar fname path)]
    (remove-suffix short-name)))

(defn- get-class-from-jar-or-file [fname path]
  (if (re-find #"\.jar" path)
    (get-jar-class fname path)
    (get-file-class fname path)))

(defn- get-basename [fname]
  (if-let [basename (second (.split fname "\\.jar:"))]
    (remove-suffix basename)
    (->> (cdtu/gen-paths)
         (map (partial get-class-from-jar-or-file fname))
         (remove nil?)
         first)))

(defn- get-class* [fname]
  (->> (get-basename fname)
       fix-class
       re-pattern))

(defn- get-class [fname]
  (try
    (get-class* fname)
    (catch Exception e
      (println fname (cdtu/source-not-found))
      (let [path (pr-str (doall (cdtu/gen-paths)))]
        (throw (Exception. (str fname " PATH: "  path " - " (cdtu/source-not-found))))))))

(defn- get-locations [line class]
  (try
    (.locationsOfLine class line)
    (catch Exception e 
           (if (or (= (type (.getCause e))
                      com.sun.jdi.AbsentInformationException)
                   (= (type e) com.sun.jdi.AbsentInformationException))
             []
             (throw e)))))

(defn line-bp [fname line & thread-args]
  (cdtu/check-unexpected-exception
   (let [c (get-class fname)
         sym (symbol (str c ":" line))
         classes (filter #(re-find (append-dollar fname c) (.name %))
                         (.allClasses (cdtu/vm)))
         locations (mapcat (partial get-locations line) classes)]
     (when-not (set-bp-locations sym locations thread-args)
       (println (cdtu/cdt-display-msg
                 (str "No breakpoints found at line: " line)))))))

(defn- thread-event-seq [list thread]
  (mapcat #(get (:thread-specific (val %)) thread) @list))

(defn enable-all-breakpoints [thread type]
  (doseq [bp (thread-event-seq cdte/bp-list thread)]
    (.setEnabled bp type)))

(defn delete-all-breakpoints []
  ;; (println @cdte/bp-list)
  (doseq [bps @cdte/bp-list]
    (delete-bp-fn (key bps))))

(defmethod cdte/make-thread-event cdte/bp-list
  [list thread sym]
  (doall (map (partial create-thread-bp thread)
              (:locations (@cdte/bp-list sym)))))

(defmacro set-bp
  [sym & thread-args]
  `(set-bp-sym '~sym ~thread-args))

(defmacro delete-bp
  [sym]
  `(delete-bp-fn '~sym))

(defn delelete-breakpoints-in-file
  [path]
  (doseq [bps @cdte/bp-list
          :let [bp-pattern (-> (first bps)
                               (str/replace #"\." "/")
                               (str/replace #":\d+$" "\\.(clj|cljs|CLJ|CLJS)")
                               re-pattern)]]
    (when (re-find bp-pattern path)
      (delete-bp-fn (key bps)))))
