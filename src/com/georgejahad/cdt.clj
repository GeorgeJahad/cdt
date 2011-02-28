;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns com.georgejahad.cdt
  (:require [clojure.string :as str])
  (:use [clojure.contrib.repl-utils :only
         [start-handling-break add-break-thread!]]
        [alex-and-georges.debug-repl])
  (:import java.util.ArrayList
           clojure.lang.Compiler
           java.lang.management.ManagementFactory
           java.io.File))

(declare reval-ret* reval-ret-str reval-ret-obj
         disable-stepping show-data update-step-list print-frame
         unmunge delete-bp-fn remote-create-str step-list)

;; add-classpath is ugly, but handles the fact that tools.jar and
;; sa-jdi.jar are platform dependencies that I can't easily put in a
;; repo:
#_(with-out-str (add-classpath (format "file:///%s/../lib/tools.jar"
                                     (System/getProperty "java.home"))))
#_(with-out-str (add-classpath (format "file:///%s/../lib/sa-jdi.jar"
                                     (System/getProperty "java.home"))))
(import com.sun.jdi.Bootstrap
        com.sun.jdi.ClassType
        com.sun.jdi.request.EventRequest
        com.sun.jdi.event.BreakpointEvent
        com.sun.jdi.event.ExceptionEvent
        com.sun.jdi.request.StepRequest
        com.sun.jdi.event.StepEvent
        com.sun.jdi.event.MethodEntryEvent
        com.sun.jdi.event.LocatableEvent
        com.sun.jdi.event.ThreadStartEvent
        com.sun.jdi.IncompatibleThreadStateException
        com.sun.jdi.ObjectCollectedException)

(defonce CDT-DISPLAY-MSG (atom false))

(defn cdt-display-msg [s]
  (condp = @CDT-DISPLAY-MSG
      true (str "CDT Display Message: " s),
      false s,
      (@CDT-DISPLAY-MSG s)))

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

(defn continue-vm []
  (.resume (vm)))

(defn continue-thread [threadx]
  (.resume threadx))

(defn list-threads []
  (.allThreads (vm)))

(defonce current-thread (atom nil))

(defn set-current-thread [threadx]
  (reset! current-thread threadx)
  (update-step-list threadx))

(defn clear-current-thread []
  (println "gbj clearing")
  (reset! current-thread nil))

(defn set-current-thread-num [thread-num]
  (set-current-thread (nth (list-threads) thread-num)))

(def sct set-current-thread-num)

(defn ct [] @current-thread)

(defonce current-frame (atom 0))

(defn set-current-frame [framex]
  (reset! current-frame framex))

(def scf set-current-frame)

(defn cf [] @current-frame)

(defn status-report [threadx]
  (let [s (if (and threadx (.isSuspended threadx))
            " "
            " not ")]
    (println (cdt-display-msg (str "Status of current thread is " s " suspended.")))))

(defonce source-path (atom ""))
(defonce prefix-path (atom ""))

(defn remove-trailing-slashes [s]
  (str/replace s (str File/separator File/pathSeparator)
               File/pathSeparator)
  (str/replace s (re-pattern (str File/separator "$")) ""))

(defn set-source-path [path]
  (reset! source-path (remove-trailing-slashes path)))

(defn set-prefix-path [path]
  (reset! prefix-path (remove-trailing-slashes path)))

(defn get-frame [threadx framex]
  (.frame threadx framex))

(defn gen-paths []
  (remove #{""}
          (map remove-trailing-slashes
               (concat
                (.split @prefix-path File/pathSeparator)
                (.classPath (vm))
                (.split @source-path File/pathSeparator)))))

(defn get-source-path [threadx framex]
  (.sourcePath (.location (get-frame threadx framex))))

(defn get-jar-entries [path]
  (map str (enumeration-seq (.entries (java.util.jar.JarFile. path)))))

(defn remove-suffix [fname]
  (first (.split fname "\\.")))

(defn get-jar [fname path]
  (->> (get-jar-entries path)
       (filter #(re-find (re-pattern (str "^" % "$")) fname))
       first))

(defn get-file-source [fname path]
  (let [full-path  (str path File/separator fname)]
    (when (.exists (File. full-path))
      {:name full-path})))

(defn get-jar-source [fname path]
  (when-let [short-name (get-jar fname path)]
    {:name short-name
     :jar path}))

(defn get-source-from-jar-or-file [fname path]
  (if (re-find #"\.jar" path)
    (get-jar-source fname path)
    (get-file-source fname path)))

(defn get-source [threadx framex]
  (let [file (get-source-path threadx framex)]
    (if (= (first file) File/separatorChar)
      {:name file}
      (->> (gen-paths)
           (map (partial get-source-from-jar-or-file file))
           (remove nil?)
           first))))

(defmacro check-unexpected-exception [& body]
  `(try
     ~@body
     (catch Exception e#
       (println (cdt-display-msg (str "Unexpected exception generated: " e#)))
       (throw e#))))

(defmacro check-incompatible-state [& body]
  `(try
     ~@body
     (catch IncompatibleThreadStateException e#
       (println (cdt-display-msg "command can only be run after stopping at a breakpoint or exception"))
       (remote-create-str "IncompatibleThreadStateException"))))

(defn source-not-found [] (cdt-display-msg "Source not found; check @source-path"))

(defn print-current-location [threadx framex]
  (try
    (check-incompatible-state
     (let [line (.lineNumber (.location (get-frame threadx framex)))]
       (if-let [{:keys [name jar]} (get-source threadx framex)]
         (let [s (format "%s:%d:%d:" name line framex)
               s (if jar (str s jar) s)]
           (println "CDT location is" s)
           (print-frame threadx framex))
         (println (source-not-found)))))
    (catch Exception _ (println (source-not-found)))))

(defn up [threadx framex]
  (let [max (dec (count (.frames threadx)))]
    (if (< framex max)
      (do
        (scf (inc framex))
        (print-current-location threadx framex))
      (println (cdt-display-msg "already at top of stack")))))

(defn down [threadx framex]
  (if (> framex 0)
    (do
      (scf (dec framex))
      (print-current-location threadx framex))
    (println (cdt-display-msg "already at bottom of stack"))))

(defonce exception-handler (atom nil))

(defonce breakpoint-handler (atom nil))

(defonce step-handler (atom nil))

(defonce method-entry-handler (atom nil))

(defonce thread-start-handler (atom nil))

(defn set-handler [h f]
  (reset! h f))

(defn default-exception-handler [e]
  (println "\n\nException" e
           (.catchLocation e) "hit\n\n"))

(defn default-step-handler [e]
  (println "\n\nStep" e "hit\n\n"))

(defn default-method-entry-handler [e]
  (println "\n\nMethod entry" e "hit\n\n"))

(defn default-breakpoint-handler [e]
  (println "\n\nBreakpoint" e "hit\n\n"))

(def new-thread (atom nil))

(defn default-thread-start-handler [e]
  (reset! new-thread (.thread e))
  (println "\n\nThread started" e "hit\n\n"))

(defn handle-event [e]
  (Thread/yield)
  (condp #(instance? %1 %2) e
    BreakpointEvent (@breakpoint-handler e)
    ExceptionEvent (@exception-handler e)
    StepEvent (@step-handler e)
    MethodEntryEvent (@method-entry-handler e)
    ThreadStartEvent (@thread-start-handler e)
    (println "other event hit")))

(defn setup-handlers []
  (set-handler exception-handler default-exception-handler)
  (set-handler breakpoint-handler default-breakpoint-handler)
  (set-handler step-handler default-step-handler)
  (set-handler method-entry-handler default-method-entry-handler)
  (set-handler thread-start-handler default-thread-start-handler))

(defn get-thread [#^LocatableEvent e]
  (.thread e))

(defn finish-set [s]
  (let [e (first (iterator-seq (.eventIterator s)))]
    (set-current-frame 0)
    (println "gbj-setting" (get-thread e))
    (set-current-thread (get-thread e))
    (disable-stepping)
    (print-current-location (ct) (cf))))

(defonce event-handler-exceptions (atom []))

(defmacro handle-event-exceptions [& body]
  `(try
     ~@body
     (catch Exception e#
       (println (cdt-display-msg (str "exception in event handler "
                                      e# ". You may need to restart CDT")))
       (swap! event-handler-exceptions conj e#)
       (Thread/sleep 5000))))

(defonce event-handler-done (atom false))

(defn handle-events []
  (println (cdt-display-msg "CDT ready"))
  (let [q (.eventQueue (vm))]
    (while (not @event-handler-done)
      (handle-event-exceptions
       (let [s (.remove q)]
         (doseq [i (iterator-seq (.eventIterator s))]
           (handle-event i))
         (finish-set s))))))

(defonce event-handler (atom nil))
(defn stop-event-handler []
  (reset! event-handler-done true))

(defn start-event-handler []
  (setup-handlers)
  (reset! event-handler (Thread. handle-events))
  (reset! event-handler-done false)
  (.start @event-handler))

(defn cdt-attach-core []
  (reset! conn-data (first (get-connectors #"SADebugServerAttachingConnector")))
  (let [args (.defaultArguments (conn))]
    (println args)
    (.setValue (.get args "debugServerName") "localhost")
    (reset! vm-data (.attach (conn) args))))

(defn cdt-attach
  ([port] (cdt-attach "localhost" port))
  ([hostname port]
     (reset! conn-data (first (get-connectors #"SocketAttach")))
     (let [args (.defaultArguments (conn))]
       (.setValue (.get args "port") port)
       (.setValue (.get args "hostname") hostname)
       (reset! vm-data (.attach (conn) args))
       (start-event-handler))))

(defn get-pid []
  (first (.split (.getName
                  (ManagementFactory/getRuntimeMXBean)) "@")))

(defn cdt-attach-pid
  ([] (cdt-attach-pid (get-pid)))
  ([pid]
     (reset! conn-data (first (get-connectors #"ProcessAttach")))
     (let [args (.defaultArguments (conn))]
       (.setValue (.get args "pid") pid)
       (reset! vm-data (.attach (conn) args))
       (start-event-handler))))

(defn cdt-detach []
  (.dispose (vm))
  (reset! CDT-DISPLAY-MSG false)
  (reset! step-list {})
  (stop-event-handler))

(defn find-classes [class-regex]
  (regex-filter class-regex (.allClasses (vm))))

(defn find-methods [class method-regex]
  (regex-filter method-regex (.methods class)))

(def rt (memoize #(first (find-classes #"clojure.lang.RT$"))))

(def co (memoize #(first (find-classes #"clojure.lang.Compiler$"))))

(def va (memoize #(first (find-classes #"clojure.lang.Var$"))))

(def rstring (memoize #(first (find-methods (rt) #"readString$"))))

(def as (memoize #(first (find-methods (rt) #"assoc$"))))

(def cj (memoize #(first (find-methods (rt) #"conj$"))))

(def ev (memoize #(first (find-methods (co) #"eval$"))))

(def ge (memoize #(first (find-methods (va) #"get$"))))

(def sroot (memoize #(first (find-methods (va) #"swapRoot$"))))

(defn print-threads []
  (doseq [[n t] (keep-indexed vector (seq (list-threads)))]
    (println n (.name t))))

(defrecord BpSpec [methods bps])

(defonce step-list (atom {}))

(defn create-step [threadx width depth]
  (doto (.createStepRequest
         (.eventRequestManager (vm)) threadx
         width depth)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled false)))

(defn update-step-list [threadx]
  (if-not (@step-list threadx)
    (swap! step-list assoc
           threadx
           {:stepi (create-step threadx
                                StepRequest/STEP_MIN StepRequest/STEP_INTO)
            :into  (create-step threadx
                                StepRequest/STEP_LINE StepRequest/STEP_INTO)
            :over  (create-step threadx
                                StepRequest/STEP_LINE StepRequest/STEP_OVER)
            :finish (create-step threadx 
                                 StepRequest/STEP_LINE StepRequest/STEP_OUT)})))

(defn do-step [threadx type]
  (fn []
    (.setEnabled ((@step-list threadx) type) true)
    (continue-thread threadx)))

(defn stepi [threadx]
  (do-step threadx :stepi))

(defn step [threadx]
  (do-step threadx :into))

(defn step-over [threadx]
  (do-step threadx :over))

(defn finish [threadx]
  (do-step threadx :finish))

(defn disable-stepping []
  (doseq [t (vals @step-list) s (vals t)]
    (.setEnabled s false)))

(defonce bp-list (atom {}))

(defn merge-with-exception [sym]
  (fn [m1 m2]
    (merge-with
     (fn [a b] (delete-bp-fn sym) b)
     m1 m2)))

(defn create-thread-start-break []
  (doto (.createThreadStartRequest
         (.eventRequestManager (vm)))
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled true)))

(defn create-bp [l]
  (doto (.createBreakpointRequest
         (.eventRequestManager (vm)) l)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled true)))

(defn munge-sym [sym]
  (let [[ns sym] (.split (str sym) "/")]
    (str (Compiler/munge ns) "\\$" (Compiler/munge (str sym)))))

(defn gen-class-pattern [sym]
  (let [s (munge-sym sym)]
    (re-pattern (str "^" s))))

(defn get-methods [sym]
  (for [c (find-classes (gen-class-pattern sym))
        m (regex-filter #"(invoke|doInvoke)" (.methods c))] m))

(defn print-bps []
  (doseq [[n k] (keep-indexed vector (keys @bp-list))]
    (println n k)))

(defn check-ns-loaded [sym]
  (let [ns (second (re-find  #"(.*)[:/]" (str sym)))
        class-regex (re-pattern (str (Compiler/munge ns) "((\\$)|$)"))]
    (when-not (seq (find-classes class-regex))
      (throw (IllegalStateException.
              (str "Namespace "
                   ns " not loaded; bp can not be set until it is."))))))

(defn set-bp-locations [sym locations]
  (check-ns-loaded sym)
  (let [bps (doall (map create-bp locations))]
    (if (seq bps)
      (do
        (println (cdt-display-msg (str "bp set on " (seq locations))))
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

(defn java-fname? [fname]
  (boolean (.endsWith fname ".java")))

(defn append-dollar [fname s]
  (if (java-fname? fname)
    s
    (re-pattern (str s "\\$"))))

(defn fix-class [c]
  (str/replace c File/separator "."))

(defn get-file-class [fname path]
  (second (re-find (re-pattern
                    (str path File/separator "(.*)(.clj|.java)")) fname)))

(defn get-jar-class [fname path]
  (when-let [short-name (get-jar fname path)]
    (remove-suffix short-name)))

(defn get-class-from-jar-or-file [fname path]
  (if (re-find #"\.jar" path)
    (get-jar-class fname path)
    (get-file-class fname path)))

(defn get-basename [fname]
  (if-let [basename (second (.split fname "\\.jar:"))]
    (remove-suffix basename)
    (->> (gen-paths)
         (map (partial get-class-from-jar-or-file fname))
         (remove nil?)
         first)))

(defn get-class* [fname]
  (->> (get-basename fname)
       fix-class
       re-pattern))

(defn get-class [fname]
  (try
    (get-class* fname)
    (catch Exception e
      (println fname (source-not-found))
      (throw (Exception. (str fname " " (source-not-found)))))))

(defn current-type [threadx framex]
  (-> (get-frame threadx framex)
      .location
      .declaringType))

(defn get-ns [threadx framex]
  (-> (current-type threadx framex)
      .name
      (.split  "\\$")
      first
      unmunge
      symbol))

(defn get-locations [line class]
  (try
    (.locationsOfLine class line)
    (catch com.sun.jdi.AbsentInformationException _ [])))

(defn line-bp [fname line]
  (check-unexpected-exception
   (let [c (get-class fname)
         sym (symbol (str c ":" line))
         classes (filter #(re-find (append-dollar fname c) (.name %))
                         (.allClasses (vm)))
         locations (mapcat (partial get-locations line) classes)]
     (when-not (set-bp-locations sym locations)
       (println (cdt-display-msg (str "No breakpoints found at line: " line)))))))

(defn delete-bp-fn [sym]
  (doseq [bp (:bps (@bp-list sym))]
    (.setEnabled bp false)
    (.deleteEventRequest (.eventRequestManager (vm)) bp))
  (swap! bp-list dissoc sym))

(defmacro delete-bp
  [sym]
  `(delete-bp-fn '~sym))

(defn enable-all-breakpoints [type]
  (doseq [bps @bp-list bp (:bps (val bps))]
    (.setEnabled bp type)))

(defn delete-all-breakpoints []
  (doseq [bps @bp-list]
    (delete-bp-fn (key bps))))

(defonce catch-list (atom {}))

(defn set-catch [class type]
  (let [caught (boolean (#{:all :caught} type))
        uncaught (boolean (#{:all :uncaught} type))
        pattern (re-pattern (str (second (.split (str class) " " )) "$"))
        ref-type (first (find-classes pattern))
        catch-request
        (doto (.createExceptionRequest (.eventRequestManager (vm))
                                       ref-type caught uncaught)
          (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
          (.setEnabled true))]
    (swap! catch-list assoc class catch-request)
    (println "catch set on" class)))

(defn delete-catch [class]
  (let [catch-request (@catch-list class)]
    (.setEnabled catch-request false)
    (.deleteEventRequest (.eventRequestManager (vm)) catch-request)
    (swap! catch-list dissoc class)))

(defn create-disabled-str [form]
  (let [s (.mirrorOf (vm) (str form))]
    ;; NEED TO RE-ENABLE SOMEWHERE!
    (try
      (.disableCollection s)
      s
      (catch ObjectCollectedException e
        (println "object collected " form)
        nil))))

(defn remote-create-str [form]
  (if-let [s (first (remove nil?
                            (take 10 (repeatedly #(create-disabled-str form)))))]
    s
    (throw (IllegalStateException. "object collected 10 times"))))

(defn make-arg-list [ & args]
  (ArrayList. (or args [])))

;; INVOKE_SINGLE_THREADED is apparently somewhat dangerous, but allows
;;  self-targeting.
(def invoke-options (atom ClassType/INVOKE_SINGLE_THREADED))

(defn remote-invoke [class-fn method-fn arglist threadx]
  (.invokeMethod (class-fn) threadx (method-fn) arglist @invoke-options))

(def remote-eval (partial remote-invoke co ev))

(def remote-read-string (partial remote-invoke rt rstring))

(def remote-assoc (partial remote-invoke rt as))

(defn remote-get [threadx v]
  (remote-invoke (constantly v) ge (make-arg-list) threadx))

(defn remote-swap-root [threadx v arg-list]
  (remote-invoke (constantly v) sroot arg-list threadx))

(def remote-conj (partial remote-invoke rt cj))

(defn get-file-name [framey]
  (let [sp (try (.sourcePath (.location framey))
                (catch Exception e "source not found"))]
    (last  (.split sp File/separator))))

(defn get-source-name [threadx framex]
  (try (-> (get-frame threadx framex)
           .location
           .sourceName) (catch Exception e nil)))

(defn clojure-frame? [threadx framex]
  (if-let [name (get-source-name threadx framex)]
    (.endsWith name ".clj")
    (do
      (println "source name unavailable")
      (-> (get-frame threadx framex) .location .method .name (.endsWith "nvoke")))))

(def default-regex
     #"(^const__\d*$|^__meta$|^__var__callsite__\d*$|^__site__\d*__$|^__thunk__\d*__$)")

(defn remove-default-fields [fields]
  (seq (remove #(re-find default-regex (.name %)) fields)))

(defn gen-closure-field-list
  ([threadx framex]
     (let [framey (.frame threadx framex)]
       (when-let [obj (.thisObject framey)]
         (let [fields (.fields (.referenceType obj))]
           (if (clojure-frame? threadx framex)
             (remove-default-fields fields)
             fields #_(.allFields (.declaringType (.location framey)))))))))

(def unmunge-seq
     (reverse (sort-by second compare clojure.lang.Compiler/CHAR_MAP)))

(defn unmunge [n]
  (reduce (fn[n [k v]] (str/replace n v (str k))) n unmunge-seq))

(defn fix-values [values]
  (into {} (for [[k v] values] [(unmunge (.name k)) v])))

(defn gen-closure-map
  ([threadx framex]
     (when-let [obj (.thisObject (.frame threadx framex))]
       (let [this-map {"this" obj}]
         (if-let [fields (gen-closure-field-list threadx framex)]
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

(defn add-local-to-map [threadx m l]
  (let [val (convert-primitives (val l))]
    (remote-assoc
     (make-arg-list m
                    (remote-create-str (key l)) val) threadx)))

(def cdt-sym (atom nil))

(defn get-cdt-sym [threadx framex]
  (or @cdt-sym
      (reset! cdt-sym
              (symbol (read-string
                       (str (reval-ret-str threadx framex `(gensym "cdt-") false)))))))

(defn gen-locals-and-closures
  ([threadx framex]
     (let [framey (.frame threadx framex)
           locals #_(fix-values (.getValues framey (-> framey .location .method .variables))) (fix-values (.getValues framey (.visibleVariables framey)))]
       (merge locals (gen-closure-map threadx framex)))))

(defn add-locals-to-map [threadx framex]
  (let [locals-and-closures (gen-locals-and-closures threadx framex)
        sym (get-cdt-sym threadx framex)
        v (reval-ret-obj threadx framex `(intern '~'user '~sym {}) false)
        new-map (reduce (partial add-local-to-map threadx)
                        (remote-get threadx v) locals-and-closures)]
    (remote-swap-root threadx v (make-arg-list new-map))
    locals-and-closures))

(defn gen-local-bindings [sym locals]
  (into [] (mapcat
            (fn [l]
              (let [local-name (key l)]
                `[~(symbol local-name)
                  ((var-get (ns-resolve '~'user '~sym)) ~local-name)]))
            locals)))

(defn gen-form-with-locals [threadx framex form]
  (let [locals (add-locals-to-map threadx framex)]
    `(let ~(gen-local-bindings (get-cdt-sym threadx framex) locals) ~form)))

(defn setup-namespace [threadx framex form]
  (if-not (clojure-frame? threadx framex)
    form
    `(binding [*ns* (find-ns '~(get-ns threadx framex))]
       ~form)))

(defn gen-form [threadx framex form return-str?]
  (let [form (if return-str?
               `(with-out-str (pr (eval '~form)))
               `(eval '~form))]
    (setup-namespace threadx framex
     `(try ~form
           (catch Throwable t#
             (with-out-str (pr (str "remote exception: " t#))))))))

(defn gen-remote-form-and-eval [threadx form]
  (-> (remote-create-str form)
      make-arg-list
      (remote-read-string threadx)
      make-arg-list
      (remote-eval threadx)))

(defn reval-ret*
  [threadx framex return-str? form locals?]
  (check-incompatible-state
   (let [form (if-not locals? form (gen-form-with-locals threadx framex form))]
     (gen-remote-form-and-eval threadx (gen-form threadx framex form return-str?)))))

(defn reval-ret-str
  [threadx framex form locals?]
  (reval-ret* threadx framex true form locals?))

(defn reval-ret-obj
  [threadx framex form locals?]
  (reval-ret* threadx framex false form locals?))

(defn fixup-string-reference-impl [sri]
  ;; remove the extra quotes caused by the stringReferenceImpl
  (apply str (butlast (drop 1 (seq (str sri))))))

(defn local-names
  ([threadx framex]
     (->> (gen-locals-and-closures threadx framex)
          keys
          (map symbol)
          sort
          (into []))))

(defn locals [threadx framex]
  (dorun (map #(println %1 %2)
              (local-names threadx framex)
              (read-string (fixup-string-reference-impl
                            (reval-ret-str threadx framex
                                           (local-names threadx framex) true))))))

(defn print-frame
  ([threadx framex]
     (let [f (get-frame threadx framex)
           l (.location f)
           ln (try (str (local-names threadx framex)) (catch Exception e "[]"))
           fname (get-file-name f)
           c (.name (.declaringType (.method l)))]
       (printf "%3d %s %s %s %s:%d\n" framex c (.name (.method l))
               ln fname (.lineNumber l)))))

(defn print-frames
  ([threadx]
     (doseq [framex (range (count (.frames threadx)))]
       (print-frame threadx framex))))

(defn get-frame-string
  ([threadx framex]
     (let [f (get-frame threadx framex)
           l (.location f)
           ln (try (str (local-names threadx framex)) (catch Exception e "[]"))
           fname (get-file-name f)
           c (.name (.declaringType (.method l)))]
       (format "%s %s %s %s:%d" c (.name (.method l))
               ln fname (.lineNumber l)))))

(defn get-frames
  ([threadx]
     (for [framex (range (count (.frames threadx)))]
       (get-frame-string threadx framex))))

(defmacro with-breakpoints-disabled [& body]
  `(try
     (enable-all-breakpoints false)
     ~@body
     (finally
      (enable-all-breakpoints true))))

(defn safe-reval [threadx framex form locals? convert-fn]
  (check-unexpected-exception
   (with-breakpoints-disabled
     (let [s (reval-ret-str threadx framex form locals?)]
       (try
         (convert-fn (fixup-string-reference-impl s))
         (catch Exception e (println-str (str s))))))))

(defmacro reval
  ([threadx framex form]
     `(reval ~threadx ~framex ~form true))
  ([threadx framex form locals?]
     `(safe-reval ~threadx ~framex '~form true read-string)))

(defn string-nil [x]
  (if (nil? x) "nil" x))

(defn reval-display [threadx framex form]
  (-> form
      (safe-reval threadx framex true read-string)
      string-nil cdt-display-msg println))

(defn gen-class-regex [c]
  (re-pattern (str (.getName c) "$")))

(defn add-obj-to-vec [threadx v obj]
  (remote-conj
   (make-arg-list v obj) threadx))

(defn get-instances [classes]
  (let [regexes (map gen-class-regex classes)]
    (mapcat #(.instances % 0) (mapcat find-classes regexes))))

(defn create-var-from-objs [threadx framex ns sym coll-form add-fn objs]
  (let [v (reval-ret-obj threadx framex `(intern  '~ns '~sym ~coll-form) false)
        new-vec (reduce add-fn (remote-get threadx v) objs)]
    (remote-swap-root threadx v (make-arg-list new-vec))
    v))

(defn create-instance-seq [threadx framex ns sym & classes]
  (let [instances (get-instances classes)]
    (create-var-from-objs threadx framex ns sym '[] add-obj-to-vec instances)))

(defn is-contained? [ls container]
  #_(if (= (type container) clojure.lang.LazySeq)
      (let [val ])))

(defn is-head [s ls]
  (if (some (partial is-contained? ls) s)
    nil
    ls))

(defn get-heads [s]
  (remove nil? (map (partial is-head s) s)))

(defn create-head-seq [threadx framex ns sym]
  (let [s (get-instances [clojure.lang.LazySeq])
        h (get-heads s)]
    (create-var-from-objs threadx framex ns sym '[] add-obj-to-vec h)))

(start-handling-break)
(add-break-thread!)
