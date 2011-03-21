(ns cdt.break
  (:use cdt.utils
        cdt.events)
  (:require [clojure.string :as str])
  (:import java.io.File
           com.sun.jdi.request.EventRequest
           com.sun.jdi.event.BreakpointEvent))

(defn- create-bp [l]
  (doto (.createBreakpointRequest
         (.eventRequestManager (vm)) l)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)))

(defn print-bps []
  (doseq [[n k] (keep-indexed vector (keys @bp-list))]
    (println n k)))

(defn- check-ns-loaded [sym]
  (let [ns (second (re-find  #"(.*)[:/]" (str sym)))
        class-regex (re-pattern (str (Compiler/munge ns) "((\\$)|$)"))]
    (when-not (seq (find-classes class-regex))
      (throw (IllegalStateException.
              (str "Namespace "
                   ns " not loaded; bp can not be set until it is."))))))

(defn- create-thread-bp [thread location]
  (let [bp (create-bp location)]
    (set-thread-filter bp thread)
    (.setEnabled bp true)
    bp))

(defn- create-thread-bps [locations thread-list groups-to-skip]
  (into {} (for [t thread-list :when (valid-thread? t groups-to-skip)]
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
  (doseq [bps (sym-event-seq sym bp-list) bp bps]
    (.setEnabled bp false)
    (.deleteEventRequest (.eventRequestManager (vm)) bp))
  (swap! bp-list dissoc sym))

(defn- merge-with-exception [sym]
  (fn [m1 m2]
    (merge-with
     (fn [a b] (delete-bp-fn sym) b)
     m1 m2)))

(defn- set-bp-locations [sym locations thread-args]
  (check-ns-loaded sym)
  (when-let [bps (apply create-bps locations thread-args)]
    (println (cdt-display-msg (str "bp set on " (seq locations))))
    (swap! bp-list
           (merge-with-exception sym) {sym bps})))

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
  (second (re-find (re-pattern
                    (str path File/separator "(.*)(.clj|.java)")) fname)))

(defn- remove-suffix [fname]
  (first (.split fname "\\.")))

(defn- get-jar-class [fname path]
  (when-let [short-name (get-jar fname path)]
    (remove-suffix short-name)))

(defn- get-class-from-jar-or-file [fname path]
  (if (re-find #"\.jar" path)
    (get-jar-class fname path)
    (get-file-class fname path)))

(defn- get-basename [fname]
  (if-let [basename (second (.split fname "\\.jar:"))]
    (remove-suffix basename)
    (->> (gen-paths)
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
      (println fname (source-not-found))
      (throw (Exception. (str fname " " (source-not-found)))))))

(defn- get-locations [line class]
  (try
    (.locationsOfLine class line)
    (catch com.sun.jdi.AbsentInformationException _ [])))

(defn line-bp [fname line & thread-args]
  (check-unexpected-exception
   (let [c (get-class fname)
         sym (symbol (str c ":" line))
         classes (filter #(re-find (append-dollar fname c) (.name %))
                         (.allClasses (vm)))
         locations (mapcat (partial get-locations line) classes)]
     (when-not (set-bp-locations sym locations thread-args)
       (println (cdt-display-msg
                 (str "No breakpoints found at line: " line)))))))

(defn- thread-event-seq [list thread]
  (mapcat #(get (:thread-specific (val %)) thread) @list))

(defmacro delete-bp
  [sym]
  `(delete-bp-fn '~sym))

(defn enable-all-breakpoints [thread type]
  (doseq [bp (thread-event-seq bp-list thread)]
    (.setEnabled bp type)))

(defn delete-all-breakpoints []
  (doseq [bps @bp-list]
    (delete-bp-fn (key bps))))

(defmethod make-thread-event bp-list
  [list thread sym]
  (doall (map (partial create-thread-bp thread)
                   (:locations (@bp-list sym)))))

