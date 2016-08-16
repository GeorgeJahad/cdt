;; Copyright (c) George Jahad. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.
;; Contributors:
;; Travis Vachon

(ns cdt.events
  (:require [cdt.utils :as cdtu])
  (:import com.sun.jdi.request.EventRequest
           com.sun.jdi.event.BreakpointEvent
           com.sun.jdi.event.ExceptionEvent
           com.sun.jdi.request.StepRequest
           com.sun.jdi.event.StepEvent
           com.sun.jdi.event.MethodEntryEvent
           com.sun.jdi.event.LocatableEvent
           com.sun.jdi.event.ThreadStartEvent))

(defonce exception-handler (atom nil))

(defonce breakpoint-handler (atom nil))

(defonce step-handler (atom nil))

(defonce method-entry-handler (atom nil))

(defonce thread-start-handler (atom nil))

(defn set-handler [h f]
  (reset! h f))

(defn- default-exception-handler [e]
  (println "\n\nException" e
           (.catchLocation e) "hit\n\n"))

(defn- default-step-handler [e]
  (println "\n\nStep" e "hit\n\n"))

(defn- default-method-entry-handler [e]
  (println "\n\nMethod entry" e "hit\n\n"))

(defn- default-breakpoint-handler [e]
  (println "\n\nBreakpoint" e "hit\n\n"))

(def new-thread (atom nil))

(defn valid-thread? [thread groups-to-skip]
  (not-any? #(= % (.threadGroup thread)) groups-to-skip))

(defn- add-thread? [thread map-entry]
  (and (:add-new-threads? (val map-entry))
       (valid-thread? thread (:groups-to-skip (val map-entry)))))

(defn- merge-thread-specific [full-map thread-specific-map]
  (merge-with #(merge-with merge %1 %2)
              full-map thread-specific-map))

;; bp list struct
;; {sym {:all bps
;;       :add-new-threads? true
;;       :groups-to-skip []
;;       :locations locations
;;       :thread-specific
;;         {t1 bps
;;          t2 bps}}}

(defonce bp-list (atom {}))
(defonce catch-list (atom {}))

;; stolen from clojure.contrib.reflect; not using it from there because
;;  clojure 1.3 contrib is layed out differently and i haven't figured
;;  out the proper dependency management yet.
;; needed because of http://www.assembla.com/spaces/clojure/tickets/259
(defn call-method
  [klass method-name params obj & args]
  (-> klass (.getDeclaredMethod (name method-name)
                                (into-array Class params))
      (doto (.setAccessible true))
      (.invoke obj (into-array Object args))))

(defn set-thread-filter [event thread]
  (call-method
   com.sun.tools.jdi.EventRequestManagerImpl$ThreadVisibleEventRequestImpl
   'addThreadFilter [com.sun.jdi.ThreadReference] event thread))

(defn- create-catch-disabled
  [ref-type caught uncaught]
  (doto (.createExceptionRequest (.eventRequestManager (cdtu/vm))
                                 ref-type caught uncaught)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)))

(defn set-catch-exclusion-filter [catch s]
  (call-method
   com.sun.tools.jdi.EventRequestManagerImpl$ClassVisibleEventRequestImpl
   'addClassExclusionFilter [java.lang.String] catch s))

(def catch-exclusion-filter-strings (atom nil))
(defn set-catch-exclusion-filter-strings [& strings]
  (reset! catch-exclusion-filter-strings strings))

(defn- create-thread-catch [thread ref-type caught uncaught]
  (let [catch (create-catch-disabled ref-type caught uncaught)]
    (set-thread-filter catch thread)
    (doseq [s @catch-exclusion-filter-strings]
      (set-catch-exclusion-filter catch s))
    (.setEnabled catch true)
    catch))

(defmulti make-thread-event
  (fn [list thread sym] list))

(defmethod make-thread-event catch-list
  [list thread sym]
  (let [{:keys [ref-type caught uncaught]}
        (@catch-list sym)]
    (create-thread-catch thread ref-type caught uncaught)))

(defn- add-thread-event [list thread sym]
  (let [event (make-thread-event list thread sym)]
    (swap! list
           merge-thread-specific
           {sym
            {:thread-specific
             {thread event}}})))

(defn- add-thread-events [list thread]
  (doseq [map-entry @list :when (add-thread? thread map-entry)]
    (add-thread-event list thread (key map-entry))))

(defmulti get-thread-from-event (fn [e] (type e)))

(defmethod get-thread-from-event ThreadStartEvent [#^ThreadStartEvent e]
  (.thread e))

(defmethod get-thread-from-event LocatableEvent [#^LocatableEvent e]
  (.thread e))

(defn- default-thread-start-handler [e]
  (let [thread (get-thread-from-event e)]
    (reset! new-thread thread)
    (add-thread-events bp-list thread)
    (add-thread-events catch-list thread)))

(defn exception-event? [e]
  (instance? ExceptionEvent e))

(defn- handle-event [e]
  (Thread/yield)
  (condp #(instance? %1 %2) e
    BreakpointEvent (@breakpoint-handler e)
    ExceptionEvent (@exception-handler e)
    StepEvent (@step-handler e)
    MethodEntryEvent (@method-entry-handler e)
    ThreadStartEvent (@thread-start-handler e)
    (println "other event hit")))

(defn- setup-handlers []
  (set-handler exception-handler default-exception-handler)
  (set-handler breakpoint-handler default-breakpoint-handler)
  (set-handler step-handler default-step-handler)
  (set-handler method-entry-handler default-method-entry-handler)
  (set-handler thread-start-handler default-thread-start-handler))

(defonce step-list (atom {}))

(defn- create-step [thread width depth]
  (doto (.createStepRequest
         (.eventRequestManager (cdtu/vm)) thread
         width depth)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled false)))

(defn- update-step-list [thread]
  (if-not (@step-list thread)
    (swap! step-list assoc
           thread
           {:stepi (create-step thread
                                StepRequest/STEP_MIN StepRequest/STEP_INTO)
            :into  (create-step thread
                                StepRequest/STEP_LINE StepRequest/STEP_INTO)
            :over  (create-step thread
                                StepRequest/STEP_LINE StepRequest/STEP_OVER)
            :finish (create-step thread
                                 StepRequest/STEP_LINE StepRequest/STEP_OUT)})))

(defn- do-step [thread type]
  (.setEnabled ((@step-list thread) type) true)
  (cdtu/continue-thread thread))

(defn stepi [thread]
  (do-step thread :stepi))

(defn step [thread]
  (do-step thread :into))

(defn step-over [thread]
  (do-step thread :over))

(defn finish [thread]
  (do-step thread :finish))

(defn- disable-stepping []
  (doseq [t (vals @step-list) s (vals t)]
    (.setEnabled s false)))

(defonce current-thread (atom nil))

(defn- set-current-thread [thread]
  (println "SETTING CURRENT THREAD")
  (reset! current-thread thread)
  (update-step-list thread))

(defn clear-current-thread []
  (reset! current-thread nil))

(defn- set-current-thread-num [thread-num]
  (set-current-thread (nth (cdtu/list-threads) thread-num)))

(def sct set-current-thread-num)

(defn ct [] @current-thread)

(defn- stop-thread-after-event [e]
  (cdtu/set-current-frame 0)
  (set-current-thread (get-thread-from-event e))
  (disable-stepping)
  ;;GBJ FIX!
  (let [frame-num 0
        thread (get-thread-from-event e)
        line (.lineNumber (.location (cdtu/get-frame thread frame-num)))]
    (if-let [{:keys [name jar]} (cdtu/get-source thread frame-num)]
      (let [s (format "%s:%d:%d:" name line frame-num)
            s (if jar (str s jar) s)]
        (println "CDT location is" s))
      (println (cdtu/source-not-found))))
 #_  (print-current-location (ct) (cf)))

(defn- resume-thread-after-event [e]
  (cdtu/continue-thread (get-thread-from-event e)))

(defn- finish-set [s]
  (let [e (first (iterator-seq (.eventIterator s)))]
    (if (instance? ThreadStartEvent e)
      (resume-thread-after-event e)
      (stop-thread-after-event e))))

(defonce event-handler-exceptions (atom []))

(defmacro handle-event-exceptions [& body]
  `(try
     ~@body
     (catch Exception e#
       (println (cdtu/cdt-display-msg (str "exception in event handler "
                                       e# ". You may need to restart CDT")))
       (swap! event-handler-exceptions conj e#)
       ;; throttle exception messages a bit
       (Thread/sleep 500))))

(defonce event-handler-state (atom :not-started))
(defonce event-handler-promise (promise))

(defn event-handler-started? []
   @event-handler-promise)

(defn- handle-events []
  (deliver event-handler-promise true)
  (reset! event-handler-state :started)
  (println (cdtu/cdt-display-msg "CDT ready"))
  (let [q (.eventQueue (cdtu/vm))]
    (while (not (= @event-handler-state :stop))
      (handle-event-exceptions
       (let [s (.remove q)]
         (doseq [i (iterator-seq (.eventIterator s))]
           (handle-event i))
         (finish-set s))))))

(defonce event-handler (atom nil))
(defn stop-event-handler []
  (reset! event-handler-state :stop))

(defn start-event-handler []
  (setup-handlers)
  (reset! event-handler (Thread. handle-events "CDT Event Handler"))
  (reset! event-handler-state false)
  (.start @event-handler))

(defn create-thread-start-request []
  (doto (.createThreadStartRequest
         (.eventRequestManager (cdtu/vm)))
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)
    (.setEnabled true)))

(defn sym-event-seq [sym list]
  (remove nil?
          (conj (vals (:thread-specific (@list sym)))
                (:all (@list sym)))))

(defn- create-thread-catches
  [ref-type caught uncaught thread-list groups-to-skip]
  (into {} (for [t thread-list :when (valid-thread? t groups-to-skip)]
             [t (create-thread-catch t ref-type caught uncaught)])))

(defn- create-catch
  ([ref-type caught uncaught]
   {:all
    (doto (create-catch-disabled ref-type caught uncaught)
      (.setEnabled true))})
  ([ref-type caught uncaught thread-list groups-to-skip add-new-threads?]
   {:add-new-threads? add-new-threads?
    :groups-to-skip groups-to-skip
    :thread-specific
    (create-thread-catches ref-type caught uncaught
                           thread-list groups-to-skip)}))

(defn set-catch [class type & thread-args]
  (when (@catch-list class)
    (throw (IllegalArgumentException. (str "catch already exists for " class))))
  (let [caught (boolean (#{:all :caught} type))
        uncaught (boolean (#{:all :uncaught} type))
        _ (println "CAUGHT: " caught)
        _ (println "UNCAUGHT: " uncaught)
        pattern (re-pattern (str (second (.split (str class) " " )) "$"))
        ref-type (first (cdtu/find-classes pattern))]
    (when-not ref-type
      (throw (IllegalArgumentException.
              (str "No reference type found for " class))))
    (let [catch (apply create-catch ref-type
                       caught uncaught thread-args)
          catch (merge catch
                       {:ref-type ref-type
                        :caught caught
                        :uncaught uncaught})]
      (swap! catch-list assoc class catch)
      (println "catch set on" class))))

(defn delete-catch [class]
  (doseq [catch (sym-event-seq class catch-list)]
    (.setEnabled catch false)
    (.deleteEventRequest (.eventRequestManager (cdtu/vm)) catch))
  (swap! catch-list dissoc class))

(defn print-catches []
  (doseq [[n k] (keep-indexed vector (keys @catch-list))]
    (println n k)))

(defn delete-all-catches []
  (doseq [bps @catch-list]
    (delete-catch (key bps))))
