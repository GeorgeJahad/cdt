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

(def va (memoize (fn [] (first (find-classes #"clojure.lang.Var")))))

(def rstring (memoize (fn [] (first (find-methods (rt) #"readString")))))

(def as (memoize (fn [] (first (find-methods (rt) #"assoc")))))

(def ev (memoize (fn [] (first (find-methods (co) #"eval")))))

(def ge (memoize (fn [] (first (find-methods (va) #"get")))))

(def sroot (memoize (fn [] (first (find-methods (va) #"swapRoot")))))

(defonce current-thread (atom nil))

(defn set-current-thread [thread]
  (reset! current-thread thread))

(defn ct [] @current-thread)

(defonce current-frame (atom 0))

(defn set-current-frame [frame]
  (reset! current-frame frame))

(defn cf [] @current-frame)

(defn list-threads []
  (.allThreads (vm)))

(defn print-threads []
  (pprint (seq (list-threads))))

(defn print-frames
  ([] (print-frames (ct)))
  ([thread] (pprint (seq (.frames thread)))))

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
    (re-pattern (str "^" s "$"))))

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
  (doseq [bp (:bps (short-name @bp-list))]
    (.setEnabled bp false)
    (.deleteEventRequest (.eventRequestManager (vm)) bp))
  (swap! bp-list dissoc short-name))

(defonce catch-list (atom {}))

(defn set-catch [class type]
  (let [caught (boolean (#{:all :caught} type))
        uncaught (boolean (#{:all :uncaught} type))
        pattern (re-pattern (second (.split (str class) " " )))
        ref-type (first (find-classes pattern))
        catch-request
        (doto (.createExceptionRequest (.eventRequestManager (vm))
                                       ref-type caught uncaught)
          (.setEnabled true))]
    (swap! catch-list assoc class catch-request)))

(defn delete-catch [class]
  (let [catch-request (@catch-list class)]
    (.setEnabled catch-request false)
    (.deleteEventRequest (.eventRequestManager (vm)) catch-request)
    (swap! catch-list dissoc class)))

(defn remote-create-str [form]
  (.mirrorOf (vm) (str form)))

(defn make-arg-list [ & args]
  (ArrayList. (or args [])))

(defn remote-invoke [class-fn method-fn arglist thread frame]
  (.invokeMethod (class-fn) thread (method-fn) arglist frame))

(def remote-eval (partial remote-invoke co ev))

(def remote-read-string (partial remote-invoke rt rstring))

(def remote-assoc (partial remote-invoke rt as))

(defn remote-get [v]
  (remote-invoke (constantly v) ge (make-arg-list) (ct) (cf)))

(defn remote-swap-root [v arg-list]
  (remote-invoke (constantly v) sroot arg-list (ct) (cf)))

(declare  reval-ret* reval-ret-str reval-ret-obj)

(defn add-local-to-map [m l]
  (remote-assoc
   (make-arg-list m
                  (remote-create-str (.name (key l))) (val l)) (ct) (cf)))

(defn add-locals-to-map [v]
  (let [frame (.frame (ct) (cf))
        locals (.getValues frame (.visibleVariables frame))
        new-map (reduce add-local-to-map (remote-get v) locals)]
    (remote-swap-root v (make-arg-list new-map))
    locals))

(defn gen-local-bindings [sym locals]
  (into [] (mapcat
            (fn [l]
              (let [local-name (.name (key l))]
                `[~(symbol local-name)
                  ((var-get (ns-resolve '~'user '~sym)) ~local-name)]))
            locals)))

(defn gen-form-with-locals [form]
  (let [sym (symbol (read-string (str (reval-ret-str `(gensym "cdt-") false))))
        _ (reval-ret-str '(ns user) false)
        v (reval-ret-obj `(def ~sym {}) false)
        locals (add-locals-to-map v)
        ]
    `(let ~(gen-local-bindings sym locals) ~form)))

(defn gen-form [form]
  )

(defn gen-form [form return-str?]
  (if return-str?
    `(with-out-str (pr (eval '~form)))
    `(eval '~form)))

(defn reval-ret*
  [return-str? form locals]
  (let [form (if-not locals form
                     (gen-form-with-locals form (ct) (cf)))]
    (-> (remote-create-str (gen-form form return-str?))
        make-arg-list
        (remote-read-string (ct) (cf))
        make-arg-list
        (remote-eval (ct) (cf)))))

(def reval-ret-str (partial reval-ret* true))
(def reval-ret-obj (partial reval-ret* false))

(defmacro reval-ret
  ([form]
     `(reval-ret ~form false))
  ([form locals]
     `(reval-ret-str '~form ~locals)))

(defmacro reval
  ([form]
     `(reval ~form false))
  ([form locals]
     `(println (str (reval-ret-str '~form ~locals)))))
