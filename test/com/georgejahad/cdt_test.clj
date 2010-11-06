(ns com.georgejahad.cdt-test
  (:use [com.georgejahad.cdt] :reload-all)
  (:use [clojure.test])
  (:import java.util.concurrent.CountDownLatch
           java.util.concurrent.TimeUnit
           com.sun.jdi.ObjectCollectedException))

(defn ctest-closure []
  (let [a 1 b 2]
    (fn [a d] ([a b d]))))

(defn test-func [a b f]
  (pr-str [a b])
  (f))

(defonce attach (cdt-attach-pid))

(defn handler [latch]
  (fn [e] (.countDown latch)))

(defn reval-test* [form]
  (try
    (let [ret (safe-reval form true)]
      (when (= ret 'IncompatibleThreadStateException)
        (Thread/sleep 100))
      ret)
    (catch ObjectCollectedException e
      ObjectCollectedException)))

(defn reval-test [form]
  (if-let [ret (first (drop-while
                       #{ObjectCollectedException 'IncompatibleThreadStateException}
                       (take 5 (repeatedly #(reval-test* form)))))]
    ret
    (throw (IllegalStateException. "Intermittent connection to target"))))

(def test-frame-str-fmt
     "  0 com.georgejahad.cdt_test$test_func invoke [a b f this] cdt_test.clj:%d\n")

(defn test-func-str-format [offset]
  (format test-frame-str-fmt
          (+ offset (:line (meta #'test-func)))))

(def test-frame-str (test-func-str-format 1))

(def step-frame-str (test-func-str-format 2))

(deftest bp-tests
  (let [event-latch (CountDownLatch. 1)
        finish-latch (CountDownLatch. 1)
        step-latch (CountDownLatch. 1)
        a (agent nil)]
    (testing "set-bp causes a breakpoint event"
      (set-bp com.georgejahad.cdt-test/test-func)
      (set-handler breakpoint-handler (handler event-latch))
      (send-off a (fn [_] (test-func 3 "test" #(.countDown finish-latch))))
      (is (.await event-latch 2 TimeUnit/SECONDS)))
    (testing "reval is able to determine proper values"
      (is (= [3 "test"] (reval-test '[a b])))
      (is (= "\"#<Namespace com.georgejahad.cdt-test>\"\n" (reval-test '*ns*))))
    (testing "print-frame shows the frame"
      (is (= (with-out-str (print-frame)) test-frame-str)))
#_    (testing "step goes to next line"
      (set-handler step-handler (handler step-latch))
      (step-over)
      (is (= (with-out-str (print-frame)) step-frame-str)))
    (testing "cont allows function to finish"
      (cont)
      (is (.await finish-latch 2 TimeUnit/SECONDS)))
    (testing "delete-bp causes the function not to stop"
      (delete-bp com.georgejahad.cdt-test/test-func)
      (send-off a (fn [_] (test-func 3 "test" #(.countDown finish-latch))))
      (is (.await finish-latch 2 TimeUnit/SECONDS)))))

(deftest catch-tests
  (let [event-latch (CountDownLatch. 1)
        a (agent nil)
        b (agent nil)]
    (testing "set-catch causes a catch event"
      (set-catch java.lang.ClassCastException :all)
      (set-handler exception-handler (handler event-latch))
      (send-off a (fn [_] (* 1 {1 2})))
      (is (.await event-latch 2 TimeUnit/SECONDS)))
    (testing "reval is able to determine proper values"
      (is (= [1 {1 2}] (reval-test '[x y]))))
    (testing "delete-catch allows exception to be thrown"
      (delete-catch java.lang.ClassCastException)
      (send-off b (fn [_] (try (* 1 {1 2})
                               (catch ClassCastException e :caught-ex))))
      (await b)
      (is (= @b :caught-ex)))))
