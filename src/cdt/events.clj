(ns cdt.events
  (:use cdt.utils
        ;; needed because of http://www.assembla.com/spaces/clojure/tickets/259
        [clojure.contrib.reflect :only [call-method]])
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

(defn set-thread-filter [event thread]
  (call-method
   com.sun.tools.jdi.EventRequestManagerImpl$ThreadVisibleEventRequestImpl
   'addThreadFilter [com.sun.jdi.ThreadReference] event thread))

(defn- create-catch-disabled
  [ref-type caught uncaught]
  (doto (.createExceptionRequest (.eventRequestManager (vm))
                                 ref-type caught uncaught)
    (.setSuspendPolicy EventRequest/SUSPEND_EVENT_THREAD)))

(defn- create-thread-catch [thread ref-type caught uncaught]
  (let [catch (create-catch-disabled ref-type caught uncaught)]
               (set-thread-filter catch thread)
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

(defmulti get-thread (fn [e] (type e)))

(defmethod get-thread ThreadStartEvent [#^ThreadStartEvent e]
  (.thread e))

(defmethod get-thread LocatableEvent [#^LocatableEvent e]
  (.thread e))

(defn- default-thread-start-handler [e]
  (let [thread (get-thread e)]
    (reset! new-thread thread)
    (add-thread-events bp-list thread)
    (add-thread-events catch-list thread)
    (println "\n\nThread started" e "hit\n\n")))

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

(defn get-thread-from-id [id]
  (first (filter #(= id (.uniqueID %)) (list-threads))))

(defonce step-list (atom {}))

(defn- create-step [thread width depth]
  (doto (.createStepRequest
         (.eventRequestManager (vm)) thread
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
  (continue-thread thread))

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
  (reset! current-thread thread)
  (update-step-list thread))

(defn clear-current-thread []
  (reset! current-thread nil))

(defn- set-current-thread-num [thread-num]
  (set-current-thread (nth (list-threads) thread-num)))

(def sct set-current-thread-num)

(defn ct [] @current-thread)

(defn- stop-thread-after-event [e]
  (set-current-frame 0)
  (set-current-thread (get-thread e))
  (disable-stepping)
  ;;GBJ FIX!
#_  (print-current-location (ct) (cf)))

(defn- resume-thread-after-event [e]
  (continue-thread (get-thread e)))

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
       (println (cdt-display-msg (str "exception in event handler "
                                      e# ". You may need to restart CDT")))
       (swap! event-handler-exceptions conj e#)
       (Thread/sleep 5000))))

(defonce event-handler-done (atom false))

(defn- handle-events []
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
  (reset! event-handler (Thread. handle-events "CDT Event Handler"))
  (reset! event-handler-done false)
  (.start @event-handler))

(defn create-thread-start-request []
  (doto (.createThreadStartRequest
         (.eventRequestManager (vm)))
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
  (let [caught (boolean (#{:all :caught} type))
        uncaught (boolean (#{:all :uncaught} type))
        pattern (re-pattern (str (second (.split (str class) " " )) "$"))
        ref-type (first (find-classes pattern))
        catch (apply create-catch ref-type
                     caught uncaught thread-args)
        catch (merge catch
                     {:ref-type ref-type
                      :caught caught
                      :uncaught uncaught})]
    (swap! catch-list assoc class catch)
    (println "catch set on" class)))

(defn delete-catch [class]
  (doseq [catch (sym-event-seq class catch-list)]
    (.setEnabled catch false)
    (.deleteEventRequest (.eventRequestManager (vm)) catch))
  (swap! catch-list dissoc class))
