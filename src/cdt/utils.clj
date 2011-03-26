(ns cdt.utils
  (:require [clojure.string :as str])
  (:import java.io.File
           clojure.lang.Compiler))

;; add-classpath is ugly, but handles the fact that tools.jar and
;; sa-jdi.jar are platform dependencies that I can't easily put in a
;; repo:
(with-out-str (add-classpath (format "file:///%s/../lib/tools.jar"
                                     (System/getProperty "java.home"))))
(with-out-str (add-classpath (format "file:///%s/../lib/sa-jdi.jar"
                                     (System/getProperty "java.home"))))

(import  com.sun.jdi.Bootstrap)

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

(defn all-thread-groups []
  (loop [groups (into #{} (.topLevelThreadGroups (vm)))]
    (let [next-groups (into groups (mapcat #(.threadGroups %) groups))]
      (if (= groups next-groups)
        groups
        (recur next-groups)))))

(defn continue-vm []
  (.resume (vm)))

(defn continue-thread [thread]
  (.resume thread))

(defn list-threads []
  (.allThreads (vm)))

(defn get-thread-from-id [id]
  (first (filter #(= id (.uniqueID %)) (list-threads))))

(defonce current-frame (atom 0))

(defn set-current-frame [frame-num]
  (reset! current-frame frame-num))

(def scf set-current-frame)

(defn cf [] @current-frame)

(defonce CDT-DISPLAY-MSG (atom false))

(defn cdt-display-msg [s]
  (condp = @CDT-DISPLAY-MSG
      true (str "CDT Display Message: " s),
      false s,
      (@CDT-DISPLAY-MSG s)))

(defn status-report [thread]
  (let [s (if (and thread (.isSuspended thread))
            " "
            " not ")]
    (println (cdt-display-msg
              (str "Status of current thread is " s " suspended.")))))

(defonce source-path (atom ""))
(defonce prefix-path (atom ""))

(defn- remove-trailing-slashes [s]
  (str/replace s (str File/separator File/pathSeparator)
               File/pathSeparator)
  (str/replace s (re-pattern (str File/separator "$")) ""))

(defn set-source-path [path]
  (reset! source-path (remove-trailing-slashes path)))

(defn set-prefix-path [path]
  (reset! prefix-path (remove-trailing-slashes path)))

(defn get-frame [thread frame-num]
  (.frame thread frame-num))

(defn- get-source-path [thread frame-num]
  (.sourcePath (.location (get-frame thread frame-num))))

(defn- get-jar-entries [path]
  (map str (enumeration-seq (.entries (java.util.jar.JarFile. path)))))

(defn get-jar [fname path]
  (->> (get-jar-entries path)
       (filter #(re-find (re-pattern (str "^" % "$")) fname))
       first))

(defn- get-file-source [fname path]
  (let [full-path  (str path File/separator fname)]
    (when (.exists (File. full-path))
      {:name full-path})))

(defn- get-jar-source [fname path]
  (when-let [short-name (get-jar fname path)]
    {:name short-name
     :jar path}))

(defn- get-source-from-jar-or-file [fname path]
  (if (re-find #"\.jar" path)
    (get-jar-source fname path)
    (get-file-source fname path)))

(defn gen-paths []
  (remove #{""}
          (map remove-trailing-slashes
               (concat
                (.split @prefix-path File/pathSeparator)
                (.classPath (vm))
                (.split @source-path File/pathSeparator)))))

(defn get-source [thread frame-num]
  (let [file (get-source-path thread frame-num)]
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

(defn source-not-found [] (cdt-display-msg
                           "Source not found; check @source-path"))

(defn find-classes [class-regex]
  (regex-filter class-regex (.allClasses (vm))))

(defn find-methods [class method-regex]
  (regex-filter method-regex (.methods class)))

(defn print-threads []
  (doseq [[n t] (keep-indexed vector (seq (list-threads)))]
    (println n (.name t))))

(defn string-nil [x]
  (if (nil? x) "nil" x))

(defn munge-sym [sym]
  (let [[ns sym] (.split (str sym) "/")]
    (str (Compiler/munge ns) "\\$" (Compiler/munge (str sym)))))

(def unmunge-seq
     (reverse (sort-by second compare clojure.lang.Compiler/CHAR_MAP)))

(defn unmunge [n]
  (reduce (fn[n [k v]] (str/replace n v (str k))) n unmunge-seq))
