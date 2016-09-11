;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; Contributors:
;; Travis Vachon

(ns cdt.reval
  (:require [cdt.utils :as cdtu]
            [cdt.break :as cdtb]
            [clojure.edn :as edn])
  (:import java.util.ArrayList
           java.io.File
           com.sun.jdi.IncompatibleThreadStateException
           com.sun.jdi.ObjectCollectedException
           com.sun.jdi.ClassType))

(declare reval-ret-obj reval-ret-str)

(def rt (memoize #(first (cdtu/find-classes #"clojure.lang.RT$"))))

(def co (memoize #(first (cdtu/find-classes #"clojure.lang.Compiler$"))))

(def va (memoize #(first (cdtu/find-classes #"clojure.lang.Var$"))))

(def rstring (memoize #(first (cdtu/find-methods (rt) #"readString$"))))

(def as (memoize #(first (cdtu/find-methods (rt) #"assoc$"))))

(def cj (memoize #(first (cdtu/find-methods (rt) #"conj$"))))

(def ev (memoize #(first (cdtu/find-methods (co) #"eval$"))))

(def ge (memoize #(first (cdtu/find-methods (va) #"get$"))))

(def sroot (memoize #(first (cdtu/find-methods (va) #"swapRoot$"))))

(defn create-disabled-str [form]
  (let [s (.mirrorOf (cdtu/vm) (str form))]
    ;; NEED TO RE-ENABLE SOMEWHERE!
    (try
      (.disableCollection s)
      s
      (catch ObjectCollectedException e
        (println "object collected " form)
        nil))))

;; I think this is trying to catch it before it gets GC'ed and gives up if it
;; fails ten times - crazy - jnorton
(defn remote-create-str [form]
  (if-let [s (first (remove nil?
                            (take 10 (repeatedly
                                      #(create-disabled-str form)))))]
    s
    (throw (IllegalStateException. "object collected 10 times"))))

(defn make-arg-list [ & args]
  (ArrayList. (or args [])))

;; INVOKE_SINGLE_THREADED is apparently somewhat dangerous, but allows
;;  self-targeting.
(def invoke-options (atom ClassType/INVOKE_SINGLE_THREADED))

(defn remote-invoke [class-fn method-fn arglist thread]
  (.invokeMethod (class-fn) thread (method-fn) arglist @invoke-options))

(def remote-eval (partial remote-invoke co ev))

(def remote-read-string (partial remote-invoke rt rstring))

(def remote-assoc (partial remote-invoke rt as))

(defn remote-get [thread v]
  (remote-invoke (constantly v) ge (make-arg-list) thread))

(defn remote-swap-root [thread v arg-list]
  (remote-invoke (constantly v) sroot arg-list thread))

(def remote-conj (partial remote-invoke rt cj))

(defn get-file-name [frame]
  (let [sp (try (.sourcePath (.location frame))
                (catch Exception e "source not found"))]
    (last  (.split sp File/separator))))

(defn get-source-name [thread frame-num]
  (try (-> (cdtu/get-frame thread frame-num)
           .location
           .sourceName) (catch Exception e nil)))

(defn clojure-frame? [thread frame-num]
  (if-let [name (get-source-name thread frame-num)]
    (.endsWith name ".clj")
    (do
      (println "source name unavailable")
      (-> (cdtu/get-frame thread frame-num) .location
          .method .name (.endsWith "nvoke")))))

(def default-regex
     #"(^const__\d*$|^__meta$|^__var__callsite__\d*$|^__site__\d*__$|^__thunk__\d*__$)")

(defn remove-default-fields [fields]
  (seq (remove #(re-find default-regex (.name %)) fields)))

(defn gen-closure-field-list
  ([thread frame-num]
   (let [frame (.frame thread frame-num)]
     (when-let [obj (.thisObject frame)]
       (let [fields (.fields (.referenceType obj))]
         (if (clojure-frame? thread frame-num)
           (remove-default-fields fields)
           fields #_(.allFields (.declaringType (.location frame)))))))))

(defn fix-values [values]
  (into {} (for [[k v] values] [(cdtu/unmunge (.name k)) v])))

(defn gen-closure-map
  ([thread frame-num]
   (when-let [obj (.thisObject (.frame thread frame-num))]
     (let [this-map {"this" obj}]
       (if-let [fields (gen-closure-field-list thread frame-num)]
         (merge this-map
                (fix-values (.getValues obj fields)))
         this-map)))))

(defn convert-type [type thread frame-num val]
  (reval-ret-obj thread frame-num (list 'new type (str val)) false))

(defn gen-conversion [t]
  (let [c (Class/forName (str "com.sun.tools.jdi." t "ValueImpl"))
        ctor (if (= t 'Char) 'Character t)]
    [c (partial convert-type ctor)]))

(defmacro gen-conversion-map [types]
  `(into {} (map gen-conversion '~types)))

(def conversion-map
     (gen-conversion-map
      [Boolean Integer Byte Char Double Float Integer Long Short]))

(defn convert-primitives [thread frame-num p]
  (if-let [f (conversion-map (type p))]
    (f thread frame-num p)
    p))

(defn add-local-to-map [thread frame-num m l]
  (let [val (convert-primitives thread frame-num (val l))]
    (remote-assoc
     (make-arg-list m
                    (remote-create-str (key l)) val) thread)))

(def cdt-sym (atom nil))

(defn get-cdt-sym [thread frame-num]
  (or @cdt-sym
      (reset! cdt-sym
              (symbol (read-string
                       (str (reval-ret-str thread frame-num
                                           `(gensym "cdt-") false)))))))

(defn gen-locals-and-closures
  ([thread frame-num]
   (println "THREAD: " thread)
   (let [frame (.frame thread frame-num)
         _ (println frame)
         locals (fix-values (.getValues frame (.visibleVariables frame)))]
     (merge locals (gen-closure-map thread frame-num)))))

(defn add-locals-to-map [thread frame-num]
  (let [locals-and-closures (gen-locals-and-closures thread frame-num)
        sym (get-cdt-sym thread frame-num)
        v (reval-ret-obj thread frame-num `(intern '~'user '~sym {}) false)
        new-map (reduce (partial add-local-to-map thread frame-num)
                        (remote-get thread v) locals-and-closures)]
    (remote-swap-root thread v (make-arg-list new-map))
    locals-and-closures))

(defn gen-local-bindings [sym locals]
  (into [] (mapcat
            (fn [l]
              (let [local-name (key l)]
                `[~(symbol local-name)
                  ((var-get (ns-resolve '~'user '~sym)) ~local-name)]))
            locals)))

(defn gen-form-with-locals [thread frame-num form]
  (let [locals (add-locals-to-map thread frame-num)]
    `(let ~(gen-local-bindings (get-cdt-sym thread frame-num) locals) ~form)))

(defn- current-type [thread frame-num]
  (-> (cdtu/get-frame thread frame-num)
      .location
      .declaringType))

(defn get-ns [thread frame-num]
  (-> (current-type thread frame-num)
      .name
      (.split  "\\$")
      first
      cdtu/unmunge
      symbol))

(defn setup-namespace [thread frame-num form]
  (if-not (clojure-frame? thread frame-num)
    form
    `(binding [*ns* (find-ns '~(get-ns thread frame-num))]
       ~form)))

(defn gen-form [thread frame-num form return-str?]
  (let [form (if return-str?
               `(with-out-str (pr (eval '~form)))
               `(eval '~form))]
    (setup-namespace thread frame-num
     `(try ~form
           (catch Throwable t#
             (with-out-str (pr (str "remote exception: " t#))))))))

(defn gen-remote-form-and-eval [thread form]
  (-> (remote-create-str form)
      make-arg-list
      (remote-read-string thread)
      make-arg-list
      (remote-eval thread)))

(defmacro check-incompatible-state [& body]
  `(try
     ~@body
     (catch IncompatibleThreadStateException e#
       (println (cdtu/cdt-display-msg
                 (str "command can only be run after "
                      "stopping at a breakpoint or exception")))
       (remote-create-str "IncompatibleThreadStateException"))))

(defn reval-ret*
  [thread frame-num return-str? form locals?]
  (check-incompatible-state
   (let [form (if-not locals? form
                      (gen-form-with-locals thread frame-num form))]
     (gen-remote-form-and-eval thread
                               (gen-form thread frame-num form return-str?)))))

(defn reval-ret-str
  [thread frame-num form locals?]
  (reval-ret* thread frame-num true form locals?))

(defn reval-ret-obj
  [thread frame-num form locals?]
  (reval-ret* thread frame-num false form locals?))

(defn fixup-string-reference-impl [sri]
  ;; remove the extra quotes caused by the stringReferenceImpl
  (apply str (butlast (drop 1 (seq (str sri))))))

(defn local-names
  ([thread frame-num]
   (->> (gen-locals-and-closures thread frame-num)
        keys
        (map symbol)
        sort
        (into []))))

(defmulti data-reader
 "Reader used to handle objects and records."
 (fn [tag value] tag))

(defmethod data-reader "object"
  [tag value]
  (str "[object " value "]"))

(defmethod data-reader "record"
  [tag value]
  (let [key (keyword tag)]
    {key value}))

(defmethod data-reader :default
  [tag value]
  value)

(defn- fix-locals-str
  "Escape objects and records to prevent read-string from trying to evaluate them."
  [locals-str]
  (println "LOCALS: " locals-str)
  (clojure.string/replace locals-str #"(#.*[}\]])", "\"$1\""))

(defn- value-for-local-str
  "Returns the value for the given local as string."
  [local-str]
  (if (re-matches #"#.*" local-str)
      local-str
      (read-string local-str))) 

(defn reader
  "Default data reader"
  [tag value]
  (println "CALLING DEFAULT READER FOR TAG " tag)
  [tag value])

(defn print-locals [thread frame-num]
  (dorun
   (map #(println %1 %2)
        (local-names thread frame-num)
        (read-string (fixup-string-reference-impl
                      (reval-ret-str thread frame-num
                                     (local-names thread frame-num) true))))))

;; Trick to handle tagged values (objects and Records) 
;; See https://github.com/clojure-cookbook/clojure-cookbook/blob/master/04_local-io/4-17_unknown-reader-literals.asciidoc
(defrecord TaggedValue [tag value])

(defmethod print-method TaggedValue [this ^java.io.Writer w]
   (.write w "#")
   (print-method (:tag this) w)
   (.write w " ")
   (print-method (:value this) w))

(defn- handle-unknown-tag [tag value]
  [tag value])

(defn read-preserving-unknown-tags [s]
  (edn/read-string {:default handle-unknown-tag} s))

(defn locals [thread frame-num]
  (println "NEW GET LOCALS")
  (binding [*default-data-reader-fn* data-reader]
    (let [frame (.frame thread frame-num)
          vars (.visibleVariables frame)
          _ (println "VARS FROM FRAME: " vars)
          args (set (map #(.name %) (filter #(.isArgument %) vars)))]
      (println "ARGS: " args)
      (reduce (fn [[arg-vars local-vars] var]
                  (let [cstr (fixup-string-reference-impl 
                               (reval-ret-str thread frame-num var true))
                        _ (println "CSTR: " cstr)
                        ; value (binding [*default-data-reader-fn* reader]
                        ;         (read-string cstr))
                        ; value (value-for-local-str cstr)
                        value (read-preserving-unknown-tags cstr)
                        ; value-map {:name var :value value}]
                        value-map {var value}]
                    (if (contains? args (str var))
                      [(conj arg-vars value-map) local-vars]
                      [arg-vars (conj local-vars value-map)])))
              [[] []]
              (local-names thread frame-num)))))



(defmacro with-breakpoints-disabled [thread & body]
  `(try
     (cdtb/enable-all-breakpoints ~thread false)
     ~@body
     (finally
      (cdtb/enable-all-breakpoints ~thread true))))

(defn safe-reval [thread frame-num form locals? convert-fn]
  (cdtu/check-unexpected-exception
   (with-breakpoints-disabled thread
     (let [s (reval-ret-str thread frame-num form locals?)]
       (try
         (convert-fn (fixup-string-reference-impl s))
         (catch Exception e (println-str (str s))))))))

(defmacro reval
  ([thread frame-num form]
   `(reval ~thread ~frame-num ~form true))
  ([thread frame-num form locals?]
   `(safe-reval ~thread ~frame-num '~form true read-string)))

(defn reval-display [thread frame-num form]
  (-> (safe-reval thread frame-num form true read-string)
      cdtu/string-nil cdtu/cdt-display-msg println))

(defn add-obj-to-vec [thread v obj]
  (remote-conj
   (make-arg-list v obj) thread))

(defn- gen-class-regex [c]
  (re-pattern (str (.getName c) "$")))

(defn get-instances [classes]
  (let [regexes (map gen-class-regex classes)]
    (mapcat #(.instances % 0) (mapcat cdtu/find-classes regexes))))

(defn create-var-from-objs [thread frame-num ns sym coll-form add-fn objs]
  (let [v (reval-ret-obj thread frame-num
                         `(intern  '~ns '~sym ~coll-form) false)
        new-vec (reduce add-fn (remote-get thread v) objs)]
    (remote-swap-root thread v (make-arg-list new-vec))
    v))

(defn create-instance-seq [thread frame-num ns sym & classes]
  (let [instances (get-instances classes)]
    (create-var-from-objs thread frame-num ns sym
                          '[] add-obj-to-vec instances)))

(defn is-contained? [ls container]
  #_(if (= (type container) clojure.lang.LazySeq)
      (let [val]))) 

(defn is-head [s ls]
  (if (some (partial is-contained? ls) s)
    nil
    ls))

(defn get-heads [s]
  (remove nil? (map (partial is-head s) s)))

(defn create-head-seq [thread frame-num ns sym]
  (let [s (get-instances [clojure.lang.LazySeq])
        h (get-heads s)]
    (create-var-from-objs thread frame-num ns sym '[] add-obj-to-vec h)))
