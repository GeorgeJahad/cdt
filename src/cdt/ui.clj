;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; Contributors:
;; Travis Vachon

(ns cdt.ui
  (:require [cdt.utils :as cdtu]
            [cdt.events :as cdte]
            [cdt.break :as cdtb]
            [cdt.reval :as cdtr])
  (:import java.lang.management.ManagementFactory))

(def cdt-release "1.2.6b")

(defn cdt-attach
  ([port] (cdt-attach "localhost" port))
  ([hostname port]
   (reset! cdtu/conn-data (first (cdtu/get-connectors #"SocketAttach")))
   (let [args (.defaultArguments (cdtu/conn))]
     (.setValue (.get args "port") port)
     (.setValue (.get args "hostname") hostname)
     (reset! cdtu/vm-data (.attach (cdtu/conn) args))
     (cdte/start-event-handler))))

(defn- get-pid []
  (first (.split (.getName
                  (ManagementFactory/getRuntimeMXBean)) "@")))

(defn cdt-attach-pid
  ([] (cdt-attach-pid (get-pid)))
  ([pid]
   (reset! cdtu/conn-data (first (cdtu/get-connectors #"ProcessAttach")))
   (let [args (.defaultArguments (cdtu/conn))]
     (.setValue (.get args "pid") pid)
     (reset! cdtu/vm-data (.attach (cdtu/conn) args))
     (cdte/start-event-handler))))

;; still experimental
(defn cdt-attach-core []
  (reset! cdtu/conn-data (first (cdtu/get-connectors
                                 #"SADebugServerAttachingConnector")))
  (let [args (.defaultArguments (cdtu/conn))]
    (println args)
    (.setValue (.get args "debugServerName") "localhost")
    (reset! cdtu/vm-data (.attach (cdtu/conn) args))))

;; still experimental
(defn cdt-detach []
  (.dispose (cdtu/vm))
  (reset! cdtu/CDT-DISPLAY-MSG false)
  (reset! cdte/step-list {})
  (cdte/stop-event-handler))

(defn print-frame
  ([thread frame-num]
   (let [f (cdtu/get-frame thread frame-num)
         l (.location f)
         ln (try (str (cdtr/local-names thread frame-num))
                 (catch Exception e "[]"))
         fname (cdtr/get-file-name f)
         c (.name (.declaringType (.method l)))]
     (printf "%3d %s %s %s %s:%d\n" frame-num c (.name (.method l))
             ln fname (.lineNumber l)))))

(defn print-current-location [thread frame-num]
  (try
    (cdtr/check-incompatible-state
     (let [line (.lineNumber (.location (cdtu/get-frame thread frame-num)))]
       (if-let [{:keys [name jar]} (cdtu/get-source thread frame-num)]
         (let [s (format "%s:%d:%d:" name line frame-num)
               s (if jar (str s jar) s)]
           (println "CDT location is" s)
           (print-frame thread frame-num))
         (println (cdtu/source-not-found)))))
    (catch Exception _ (println (cdtu/source-not-found)))))

(defn up [thread frame-num]
  (let [max (dec (count (.frames thread)))]
    (if (< frame-num max)
      (let [new-frame-num (inc frame-num)]
        (cdtu/scf new-frame-num)
        (print-current-location thread new-frame-num))
      (println (cdtu/cdt-display-msg "already at top of stack")))))

(defn down [thread frame-num]
  (if (> frame-num 0)
    (let [new-frame-num (dec frame-num)]
      (cdtu/scf new-frame-num)
      (print-current-location thread new-frame-num))
    (println (cdtu/cdt-display-msg "already at bottom of stack"))))

(defn print-frames
  ([thread]
   (doseq [frame-num (range (count (.frames thread)))]
     (print-frame thread frame-num))))

(defn- get-frame-string
  ([thread frame-num]
   (let [f (cdtu/get-frame thread frame-num)
         l (.location f)
         ln (try (str (cdtr/local-names thread frame-num))
                 (catch Exception e "[]"))
         fname (cdtr/get-file-name f)
         c (.name (.declaringType (.method l)))]
     (format "%s %s %s %s:%d" c (.name (.method l))
             ln fname (.lineNumber l)))))

(defn get-frames
  ([thread]
   (for [frame-num (range (count (.frames thread)))]
     (get-frame-string thread frame-num))))

(defmacro bg [& body]
  `(.start (Thread.
            (fn[] (binding [*ns* ~*ns*] (eval '(do ~@body)))))))

(defmacro expose [& vars-to-expose]
  `(do
     ~@(for [var vars-to-expose]
         (let [var-name (symbol (second (.split (str var) "/")))]
           `(do (def ~var-name (var-get (var ~var)))
                (if (:macro (meta (var ~var)))
                  (.setMacro (var ~var-name))))))))

(expose cdtu/conn cdtu/vm cdtu/vm cdtu/continue-vm cdtu/list-threads cdtu/cf
        cdtu/print-threads cdtu/all-thread-groups cdtu/get-thread-from-id
        cdtu/cdt-display-msg cdtu/set-display-msg cdtu/continue-thread

        cdte/set-handler cdte/bp-list cdte/catch-list
        cdte/stepi cdte/step cdte/step-over cdte/finish cdte/ct cdte/set-catch
        cdte/delete-catch cdte/get-thread-from-event cdte/exception-event?
        cdte/exception-handler cdte/breakpoint-handler
        cdte/step-handler cdte/create-thread-start-request
        cdte/set-catch-exclusion-filter-strings cdte/event-handler-started?
        cdte/delete-all-catches cdte/print-catches

        cdtb/print-bps cdtb/line-bp cdtb/delete-all-breakpoints cdtb/set-bp
        cdtb/set-bp-sym cdtb/delete-bp cdtb/delelete-breakpoints-in-file

        cdtr/locals cdtr/safe-reval cdtr/reval cdtr/reval-display)

