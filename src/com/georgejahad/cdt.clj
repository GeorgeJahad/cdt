;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns com.georgejahad.cdt
  (:require [clojure.contrib.str-utils2 :as str2])
  (:use [clojure.contrib.seq-utils :only [indexed]]
        [clojure.contrib.repl-utils :only
         [start-handling-break add-break-thread!]])
  (:import java.util.ArrayList
           clojure.lang.Compiler))

(declare reval-ret* reval-ret-str reval-ret-obj
         disable-stepping show-data init-step-list print-frame)

;; This handles the fact that tools.jar is a global dependency that
;; can't really be in a repo:
(with-out-str (add-classpath (format "file://%s/../lib/tools.jar"
                                     (System/getProperty "java.home"))))
(with-out-str (add-classpath (format "file://%s/../lib/sa-jdi.jar"
                                     (System/getProperty "java.home"))))
(import com.sun.jdi.Bootstrap
        com.sun.jdi.request.EventRequest
        com.sun.jdi.event.BreakpointEvent
        com.sun.jdi.event.ExceptionEvent
        com.sun.jdi.request.StepRequest
        com.sun.jdi.event.StepEvent
        com.sun.jdi.event.LocatableEvent
        com.sun.jdi.IncompatibleThreadStateException)

(defn regex-filter [regex seq]
  (filter #(re-find regex (.name %)) seq))

(defn get-connectors [regex]
  (regex-filter regex (.allConnectors
                       (Bootstrap/virtualMachineManager))))

(defonce conn-data (atom nil))

(defn conn [] @conn-data)

(defn get-socket-connectors []
  (regex-filter #"SocketAttach" (.allConnectors
                                 (Bootstrap/virtualMachineManager))))

(defonce vm-data (atom nil))

(defn vm [] @vm-data)

(defn cont []
  (.resume (vm)))

(defn list-threads []
  (.allThreads (vm)))

(defonce current-thread (atom nil))

(defn set-current-thread [thread-num]
  (reset! current-thread (nth (list-threads) thread-num)))

(def sct set-current-thread)

(defn ct [] @current-thread)

(defonce current-frame (atom 0))

(defn set-current-frame [frame]
  (reset! current-frame frame))

(def scf set-current-frame)

(defn cf [] @current-frame)

(defonce source-path (atom ""))

(defn set-source-path [path]
  (reset! source-path path))

(defn get-source []
  (let [file (.sourcePath (.location (.frame (ct) (cf))))
        paths (.split @source-path ":")]
    (first (filter #(.exists (java.io.File. %))
                   (for [p paths] (str p "/" file))))))

(defn print-current-location []
  (try
   (let [line (.lineNumber (.location (.frame (ct) (cf))))]
     (if-let [path (get-source)]
       (println "CDT location is" (format "%s:%d" path line))
       (println "Source not found")))
   (catch Exception _ (println "Source not found")))
  (print-frame))

(defn up []
  (let [max (dec (count (.frames (ct))))]
    (if (< (cf) max)
      (scf (inc (cf)))
      (println "already at top of stack")))
  (print-current-location))

(defn down []
  (if (> (cf) 0)
    (scf (dec (cf)))
    (println "already at bottom of stack"))
  (print-current-location))

(defn handle-exception [e]
  (println "\n\nException" e
           (.catchLocation e) "hit\n\n")
  #_(.exec (Runtime/getRuntime) "/tmp/g3"))

(defn handle-event [e]
  (Thread/yield)
  (condp #(instance? %1 %2) e
    BreakpointEvent (println "\n\nBreakpoint" e "hit\n\n")
    ExceptionEvent (handle-exception e)
    StepEvent  (println "\n\nStep" e "hit\n\n")
    :default (println "other event hit")))

(defn get-thread [#^LocatableEvent e]
  (.thread e))

(defn finish-set [s]
  (let [e (first (iterator-seq (.eventIterator s)))]
    (set-current-frame 0)
    (reset! current-thread (get-thread e))
    (disable-stepping)
    (print-current-location)))

(defn handle-events []
  (println "starting event handler")
  (let [q (.eventQueue (vm))]
    (while true
      (try
       (let [s (.remove q)]
         (doseq [i (iterator-seq (.eventIterator s))]
           (handle-event i))
         (finish-set s))
       (catch Exception e
         (do
           (println "exception in event handler" e)
           (Thread/sleep 500)))))))

(defonce event-handler (atom nil))

(defn start-event-handler []
  (reset! event-handler (Thread. handle-events))
  (.start @event-handler))

(defn finish-attach []
  (init-step-list))

(defn cdt-attach-core []
  (reset! conn-data (first (get-connectors #"SADebugServerAttachingConnector")))
  (let [args (.defaultArguments (conn))]
    (println args)
    (.setValue (.get args "debugServerName") "localhost")
    (reset! vm-data (.attach (conn) args))
    (finish-attach)))

(defn cdt-attach
  ([port] (cdt-attach "localhost" port))
  ([hostname port]
     (reset! conn-data (first (get-connectors #"SocketAttach")))
     (let [args (.defaultArguments (conn))]
       (.setValue (.get args "port") port)
       (.setValue (.get args "hostname") hostname)
       (reset! vm-data (.attach (conn) args))
       (start-event-handler)
       (finish-attach))))

(defn find-classes [class-regex]
  (regex-filter class-regex (.allClasses (vm))))

(defn find-methods [class method-regex]
  (regex-filter method-regex (.methods class)))

(def rt (memoize #(first (find-classes #"clojure.lang.RT"))))

(def co (memoize #(first (find-classes #"clojure.lang.Compiler"))))

(def va (memoize #(first (find-classes #"clojure.lang.Var"))))

(def rstring (memoize #(first (find-methods (rt) #"readString"))))

(def as (memoize #(first (find-methods (rt) #"assoc"))))

(def ev (memoize #(first (find-methods (co) #"eval"))))

(def ge (memoize #(first (find-methods (va) #"get"))))

(def sroot (memoize #(first (find-methods (va) #"swapRoot"))))

(defn print-threads []
  (doseq [[n t] (indexed (seq (list-threads)))]
    (println n (.name t))))

(defrecord BpSpec [methods bps])

(defonce step-list (atom nil))

(defn create-step [width depth]
  (doto (.createStepRequest
         (.eventRequestManager (vm)) (ct)
         width depth)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled false)))

(defn init-step-list []
  (reset! step-list
          {:stepi (create-step StepRequest/STEP_MIN StepRequest/STEP_INTO)
           :into  (create-step StepRequest/STEP_LINE StepRequest/STEP_INTO)
           :over  (create-step StepRequest/STEP_LINE StepRequest/STEP_OVER)
           :finish  (create-step StepRequest/STEP_LINE StepRequest/STEP_OUT)}))

(defn do-step [type]
  (fn []
    (.setEnabled (@step-list type) true)
    (cont)))

(def stepi (do-step :stepi))
(def step (do-step :into))
(def step-over (do-step :over))
(def finish (do-step :finish))

(defn disable-stepping []
  (doseq [s (vals @step-list)]
    (.setEnabled s false)))

(defonce bp-list (atom {}))

(defn merge-with-exception [short-name]
  (partial merge-with
           #(throw (IllegalArgumentException.
                    (str "bp-list already contains a " short-name)))))

(defn create-bp [l]
  (doto (.createBreakpointRequest
         (.eventRequestManager (vm)) l)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled true)))

(defn munge-sym [sym]
  (let [[ns sym] (.split (str sym) "/")]
    (str (Compiler/munge ns) "\\$" (Compiler/munge sym))))

(defn gen-class-pattern [sym]
  (let [s (munge-sym sym)]
    (re-pattern (str "^" s "$"))))

(defn get-methods [sym]
  (for [c (find-classes (gen-class-pattern sym))
        m (regex-filter #"(invoke|doInvoke)" (.methods c))] m))


(defn set-bp-locations [sym locations] (debug-repl)
  (let [bps (doall (map create-bp locations))]
    (if (seq bps)
      (do
        (println "bp set on" locations)
        (swap! bp-list
               (merge-with-exception sym) {sym (BpSpec. locations bps)}))
      false)))

(defn set-bp-sym [sym]
  (let [methods (get-methods sym)]
    (when-not (set-bp-locations sym (map #(.location %) methods))
      (println "no methods found for" sym))))

(defmacro set-bp
  [sym]
  `(set-bp-sym '~sym))

(defn fix-class [c]
  (str2/replace c "/" "."))

(defn get-class [fname]
  (->> (.split @source-path ":")
       (map #(re-find (re-pattern (str % "/(.*)(.clj|.java)")) fname))
       (remove nil?)
       first
       second
       fix-class
       re-pattern))

(defn line-bp [fname line]
  (let [c (get-class fname)
        sym (symbol (str c ":" line))
        classes (filter #(re-find c (.name %)) (.allClasses (vm)))
        locations (mapcat #(.locationsOfLine % line) classes)] (debug-repl)
    (when-not (set-bp-locations sym locations)
      (println "no breakpoints found at line" line))))

(defn delete-bp-fn [sym]
  (doseq [bp (:bps (@bp-list sym))]
    (.setEnabled bp false)
    (.deleteEventRequest (.eventRequestManager (vm)) bp))
  (swap! bp-list dissoc sym))

(defmacro delete-bp
  [sym]
  `(delete-bp-fn '~sym))

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
    (swap! catch-list assoc class catch-request)
    (println "catch set on" class)))

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

(defn get-file-name [frame]
  (let [sp (try (.sourcePath (.location frame))
                (catch Exception e "source not found"))]
    (last  (.split sp "/"))))

(defn clojure-frame? [frame fields]
  (let [names (map #(.name %) fields)]
    (or (.endsWith (get-file-name frame) ".clj")
        (some #{"__meta"} names))))

(def default-regex
     #"(^const__\d*$|^__meta$|^__var__callsite__\d*$|^__site__\d*__$|^__thunk__\d*__$)")

(defn remove-default-fields [fields]
  (seq (remove #(re-find default-regex (.name %)) fields)))

(defn gen-closure-field-list
  ([] (gen-closure-field-list (cf)))
  ([f] (let [frame (.frame (ct) f)]
         (when-let [obj (.thisObject frame)]
           (let [fields (.fields (.referenceType obj))]
             (if (clojure-frame? frame fields)
               (remove-default-fields fields)))))))

(defn fix-values [values]
  (into {} (for [[k v] values] [(.name k) v])))

(defn gen-closure-map
  ([] (gen-closure-map (cf)))
  ([f] (when-let [obj (.thisObject (.frame (ct) f))]
         (let [this-map {"this" obj}]
           (if-let [fields (gen-closure-field-list f)]
             (merge this-map
                    (fix-values (.getValues obj fields)))
             this-map)))))

(defn convert-type [type val]
  (reval-ret-obj (list 'new type (str val)) false))

(defn gen-conversion [t]
  (let [c (Class/forName (str "com.sun.tools.jdi." t "ValueImpl"))
        ctor (if (= t 'Char) 'Character t)]
    [c (partial convert-type ctor)]))

(defmacro gen-conversion-map [types]
  `(into {} (map gen-conversion '~types)))

(def conversion-map
     (gen-conversion-map
      [Boolean Integer Byte Char Double Float Integer Long Short]))

(defn convert-primitives [p]
  (if-let [f (conversion-map (type p))]
    (f p)
    p))


(defn add-local-to-map [m l]
  (let [val (convert-primitives (val l))]
    (remote-assoc
     (make-arg-list m
                    (remote-create-str (key l)) val) (ct) (cf))))

(def cdt-sym (atom nil))

(defn get-cdt-sym []
  (or @cdt-sym
      (reset! cdt-sym
              (symbol (read-string
                       (str (reval-ret-str `(gensym "cdt-") false)))))))

(defn gen-locals-and-closures
  ([] (gen-locals-and-closures (cf)))
  ([f] (let [frame (.frame (ct) f)
             locals (fix-values (.getValues frame (.visibleVariables frame)))]
         (merge locals (gen-closure-map f)))))

(defn add-locals-to-map []
  (let [locals-and-closures (gen-locals-and-closures)
        sym (get-cdt-sym)
        v (reval-ret-obj `(intern '~'user '~sym {}) false)
        new-map (reduce add-local-to-map (remote-get v) locals-and-closures)]
    (remote-swap-root v (make-arg-list new-map))
    locals-and-closures))

(defn gen-local-bindings [sym locals]
  (into [] (mapcat
            (fn [l]
              (let [local-name (key l)]
                `[~(symbol local-name)
                  ((var-get (ns-resolve '~'user '~sym)) ~local-name)]))
            locals)))

(defn gen-form-with-locals [form]
  (let [locals (add-locals-to-map)]
    `(let ~(gen-local-bindings (get-cdt-sym) locals) ~form)))

(defn gen-form [form return-str?]
  (let [form (if return-str?
               `(with-out-str (pr (eval '~form)))
               `(eval '~form))]
    `(try ~form
          (catch Throwable t#
            (with-out-str (pr (str "remote exception: " t#)))))))

(defn gen-remote-form-and-eval [form]
  (-> (remote-create-str form)
      make-arg-list
      (remote-read-string (ct) (cf))
      make-arg-list
      (remote-eval (ct) (cf))))

(defn reval-ret*
  [return-str? form locals?]
  (try
   (let [form (if-not locals? form (gen-form-with-locals form))]
     (gen-remote-form-and-eval (gen-form form return-str?)))
   (catch IncompatibleThreadStateException e
     (throw
      (IncompatibleThreadStateException.
       "reval can only be run after stopping at an breakpoint or exception")))))

(def reval-ret-str (partial reval-ret* true))
(def reval-ret-obj (partial reval-ret* false))

(defn fixup-string-reference-impl [sri]
  ;; remove the extra quotes caused by the stringReferenceImpl
  (apply str (butlast (drop 1 (seq (str sri))))))

(defn local-names
  ([] (local-names (cf)))
  ([f] (->> (gen-locals-and-closures f)
            keys
            (map symbol)
            (into []))))

(defn locals []
  (dorun (map #(println %1 %2)
              (local-names)
              (read-string (fixup-string-reference-impl
                            (reval-ret-str (local-names) true))))))

(defn print-frame
  ([] (print-frame (cf) (.frame (ct) (cf))))
  ([i f]
     (let [l (.location f)
           ln (try (str (local-names i)) (catch Exception e "[]"))
           fname (get-file-name f)
           c (.name (.declaringType (.method l)))]
       (printf "%3d %s %s %s %s:%d\n" i c (.name (.method l))
               ln fname (.lineNumber l)))))

(defn print-frames
  ([] (print-frames (ct)))
  ([thread]
     (doseq [[i f] (indexed (.frames thread))]
       (print-frame i f))))

(defmacro reval
  ([form]
     `(reval ~form true))
  ([form locals?]
     `(read-string (fixup-string-reference-impl
                    (reval-ret-str '~form ~locals?)))))

(defmacro reval-println
  ([form]
     `(reval-println ~form true))
  ([form locals?]
     `(println (str (reval-ret-str '~form ~locals?)))))

(start-handling-break)
(add-break-thread!)

(defn class-test [string data]
  (= (str (class data)) (str "class sun.jvm.hotspot.jdi." string)))

(def field? (partial class-test "FieldImpl"))
(def array-ref? (partial class-test "ArrayReferenceImpl"))
(def string-ref? (partial class-test "StringReferenceImpl"))

(defn seqable [data]
  (try
   (.allFields (.referenceType data))
   (catch Exception _ false)))

(use 'alex-and-georges.debug-repl)
(import sun.jvm.hotspot.jdi.FieldImpl)

(defn handle-field [prev [depth limit data]]
  (show-data  (.getValue prev data) [(inc depth) limit (.getValue prev data)]))

(def gbug (atom nil))
(defn show-data [prev [depth limit data]]
  #_  (println data)
  (when @gbug
    (debug-repl))
  (cond
    (> depth limit)
    (do
      (println "depth " depth "reached")
      [depth limit data])
    (field? data)
    (do
      #_      (println "field found ")
      (handle-field prev [depth limit data]))
    (array-ref? data)
    (map #(show-data data [(inc depth) limit %])
         (.getValues data))
    (string-ref? data)
    [depth limit data]
    :else
    (if-let [fields (seqable data)]
      (map #(show-data data [(inc depth) limit %])
           fields)
      [depth limit data])))

