(ns com.georgejahad.cdt
  (:require [clojure.contrib.str-utils2 :as str2])
  (:use clojure.contrib.pprint)
  (:import java.util.ArrayList))

;; This handles the fact that tools.jar is a global dependency that
;; can't really be in a repo:
(with-out-str (add-classpath (format "file://%s/../lib/tools.jar"
                                     (System/getProperty "java.home"))))
(import com.sun.jdi.Bootstrap
        com.sun.jdi.request.EventRequest)

(use 'alex-and-georges.debug-repl)
(defn regex-filter [regex seq]
  (filter #(re-find regex (.name %)) seq))

(def conn
     (memoize
      (fn [] (first (regex-filter #"SocketAttach"
                                  (.allConnectors
                                   (Bootstrap/virtualMachineManager)))))))

(defonce vm-data (atom nil))

(defn vm [] @vm-data)

(defn cdt-attach [port]
  (let [args (.defaultArguments (conn))]
    (.setValue (.get args "port") port)
    (reset! vm-data (.attach (conn) args))))

(defn find-classes [class-regex]
  (regex-filter class-regex (.allClasses (vm))))

(defn find-methods [class method-regex]
  (regex-filter method-regex (.methods class)))

(def rt (memoize (fn [] (first (find-classes #"clojure.lang.RT")))))

(def co (memoize (fn [] (first (find-classes #"clojure.lang.Compiler")))))

(def rstring (memoize (fn [] (first (find-methods  (rt) #"readString")))))

(def ev (memoize (fn [] (first (find-methods (co) #"eval")))))

(defonce current-thread (atom nil))

(defn set-current-thread [thread]
  (reset! current-thread thread))

(defn ct [] @current-thread)

(defn list-threads []
  (.allThreads (vm)))

(defn print-threads []
  (pprint (seq (list-threads))))

(defrecord BpSpec [sym methods bps])

(defonce bp-list (atom {}))

(defn merge-with-exception [short-name]
  (partial merge-with
           #(throw (IllegalArgumentException.
                    (str "bp-list already contains a " short-name)))))
(defn create-bp [m]
  (doto (.createBreakpointRequest
         (.eventRequestManager (vm)) (.location m))
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled true)))

(defn gen-class-pattern [sym]
  (let [s (str2/replace (str sym) "/" "\\$")]
    (re-pattern (str "^" s "__\\d*$"))))

(defn get-methods [sym]
  (for [c (find-classes (gen-class-pattern sym))
        m (regex-filter #"(invoke|doInvoke)" (.methods c))] m))

(defn set-bp-fn [sym short-name]
  (let [methods (get-methods sym)
        k (keyword short-name)
        bps (map create-bp methods)]
    (swap! bp-list (merge-with-exception k) {k (BpSpec. sym methods bps)})))

(defmacro set-bp
  ([sym]
     (let [short-name (symbol (second (seq (.split (str sym) "/"))))]
       `(set-bp ~sym ~short-name)))
  ([sym short-name]
     `(set-bp-fn '~sym '~short-name)))

(defn delete-bp [short-name]
  (debug-repl)
  (doseq [bp (:bps (short-name @bp-list))]
    (.deleteEventRequest (.eventRequestManager (vm)) bp))
  (swap! bp-list dissoc short-name))
