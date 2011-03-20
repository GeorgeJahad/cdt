;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns cdt.core
  (:use (cdt utils events reval)
        [alex-and-georges.debug-repl])
  (:import java.lang.management.ManagementFactory))

(defn cdt-attach
  ([port] (cdt-attach "localhost" port))
  ([hostname port]
     (reset! conn-data (first (get-connectors #"SocketAttach")))
     (let [args (.defaultArguments (conn))]
       (.setValue (.get args "port") port)
       (.setValue (.get args "hostname") hostname)
       (reset! vm-data (.attach (conn) args))
       (start-event-handler))))

(defn- get-pid []
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

;; still experimental
(defn cdt-attach-core []
  (reset! conn-data (first (get-connectors #"SADebugServerAttachingConnector")))
  (let [args (.defaultArguments (conn))]
    (println args)
    (.setValue (.get args "debugServerName") "localhost")
    (reset! vm-data (.attach (conn) args))))

;; still experimental
(defn cdt-detach []
  (.dispose (vm))
  (reset! CDT-DISPLAY-MSG false)
  (reset! step-list {})
  (stop-event-handler))

(defmacro set-bp
  [sym]
  `(cdt.break/set-bp-sym '~sym))

(defn print-frame
  ([thread frame-num]
     (let [f (get-frame thread frame-num)
           l (.location f)
           ln (try (str (local-names thread frame-num))
                   (catch Exception e "[]"))
           fname (get-file-name f)
           c (.name (.declaringType (.method l)))]
       (printf "%3d %s %s %s %s:%d\n" frame-num c (.name (.method l))
               ln fname (.lineNumber l)))))

(defn print-current-location [thread frame-num]
  (try
    (check-incompatible-state
     (let [line (.lineNumber (.location (get-frame thread frame-num)))]
       (if-let [{:keys [name jar]} (get-source thread frame-num)]
         (let [s (format "%s:%d:%d:" name line frame-num)
               s (if jar (str s jar) s)]
           (println "CDT location is" s)
           (print-frame thread frame-num))
         (println (source-not-found)))))
    (catch Exception _ (println (source-not-found)))))

(defn up [thread frame-num]
  (let [max (dec (count (.frames thread)))]
    (if (< frame-num max)
      (let [new-frame-num (inc frame-num)]
        (scf new-frame-num)
        (print-current-location thread new-frame-num))
      (println (cdt-display-msg "already at top of stack")))))

(defn down [thread frame-num]
  (if (> frame-num 0)
    (let [new-frame-num (dec frame-num)]
      (scf new-frame-num)
      (print-current-location thread new-frame-num))
    (println (cdt-display-msg "already at bottom of stack"))))

(defn print-frames
  ([thread]
     (doseq [frame-num (range (count (.frames thread)))]
       (print-frame thread frame-num))))

(defn- get-frame-string
  ([thread frame-num]
     (let [f (get-frame thread frame-num)
           l (.location f)
           ln (try (str (local-names thread frame-num))
                   (catch Exception e "[]"))
           fname (get-file-name f)
           c (.name (.declaringType (.method l)))]
       (format "%s %s %s %s:%d" c (.name (.method l))
               ln fname (.lineNumber l)))))

(defn get-frames
  ([thread]
     (for [frame-num (range (count (.frames thread)))]
       (get-frame-string thread frame-num))))

(defmacro reval
  ([thread frame-num form]
     `(reval ~thread ~frame-num ~form true))
  ([thread frame-num form locals?]
     `(safe-reval ~thread ~frame-num '~form true read-string)))

(defn reval-display [thread frame-num form]
  (-> (safe-reval thread frame-num form true read-string)
      string-nil cdt-display-msg println))
