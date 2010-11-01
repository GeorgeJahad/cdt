(ns com.georgejahad.cdt-test
  (:use [com.georgejahad.cdt] :reload-all)
  (:use [clojure.test])
  (:import java.util.concurrent.CountDownLatch
           java.util.concurrent.TimeUnit))

(defn ctest-closure []
  (let [a 1 b 2]
    (fn [a d] ([a b d]))))

(defn test-func [a b f]
  (pr-str [a b])
  (f))

(defonce attach (cdt-attach-pid))

(defn handler [latch]
  (fn [e] (.countDown latch)))

(defn init-debugger-test-harness []
  ;; bypass debug harness problems
  (try (reval "") (catch com.sun.jdi.ObjectCollectedException e)))

(def test-frame-str
      "  0 com.georgejahad.cdt_test$test_func invoke [a b f this] cdt_test.clj:12\n")

(deftest bp-tests
  (let [event-latch (CountDownLatch. 1)
        finish-latch (CountDownLatch. 1)
        a (agent nil)]
    (testing "set-bp causes a breakpoint event"
      (set-bp com.georgejahad.cdt-test/test-func)
      (set-handler breakpoint-handler (handler event-latch))
      (send-off a (fn [_] (test-func 3 "test" #(.countDown finish-latch))))
      (is (.await event-latch 2 TimeUnit/SECONDS)))
    (testing "reval is able to determine proper values"
      (init-debugger-test-harness)
      (is (= [3 "test"] (reval [a b])))
      (is (= "\"#<Namespace com.georgejahad.cdt-test>\"\n" (reval *ns*))))
    (testing "print-frame shows the frame"
      (is (= (with-out-str (print-frame)) test-frame-str)))
    (testing "cont allows function to finish"
      (cont)
      (is (.await finish-latch 2 TimeUnit/SECONDS)))
    (testing "delete-bp causes the function not to stop"
      (delete-bp com.georgejahad.cdt-test/test-func)
      (send-off a (fn [_] (test-func 3 "test" #(.countDown finish-latch))))
      (is (.await finish-latch 2 TimeUnit/SECONDS)))))

(deftest catch-tests
  (let [event-latch (CountDownLatch. 1)
        finish-latch (CountDownLatch. 1)
        a (agent nil)]
    (testing "set-catch causes a catch event"
      (set-catch java.lang.ClassCastException :all)
      (set-handler exception-handler (handler event-latch))
      (send-off a (fn [_] (* 1 {1 2})
                    (.countDown finish-latch)))
      (is (.await event-latch 2 TimeUnit/SECONDS)))
    (testing "reval is able to determine proper values"
      (init-debugger-test-harness)
      (is (= [1 {1 2}] (reval [x y]))))
    (testing "delete-catch allows exception to be thrown"
      (delete-catch java.lang.ClassCastException)
      (is (thrown? java.lang.ClassCastException (* 1 {1 2}))))))
