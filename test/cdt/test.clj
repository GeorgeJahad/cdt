(ns cdt.test
  (:use [cdt ui] :reload-all)
  (:use [clojure.test])
  (:import java.util.concurrent.CountDownLatch
           java.util.concurrent.TimeUnit
           com.sun.jdi.ObjectCollectedException))

(defn ctest-closure []
  (let [a 1 b 2]
    (fn [a d] ([a b d]))))

(defn test-func [a b f]
  (let [c (+ 1 2)
        d (+ c 4)
        e (+ d 5)]
    (pr-str [a b c d e])
    (f)))

(defonce attach (cdt-attach-pid))

(defn handler [latch]
  (fn [e] (.countDown latch)))

(defn reval-test [form]
  (safe-reval (ct) (cf) form true read-string))

(def test-frame-str-fmt
     "  0 cdt.test$test_func invoke %s test.clj:%d\n")

(defn test-func-str-format [var-string offset]
  (format test-frame-str-fmt var-string
          (+ offset (:line (meta #'test-func)))))

(def test-frame-str (test-func-str-format "[a b f this]" 1))

;; the next two defs will have to be corrected once this is fixed:
;; http://dev.clojure.org/jira/browse/CLJ-734
(def step-frame-str (test-func-str-format "[a b f this]" 2))

(def line-bp-frame-str (test-func-str-format "[a b f this]" 2))

(deftest bp-tests
  (let [event-latch (CountDownLatch. 1)
        finish-latch (CountDownLatch. 1)
        step-latch (CountDownLatch. 1)
        a (agent nil)]
    (testing "set-bp causes a breakpoint event"
      (set-bp cdt.test/test-func)
      (set-handler breakpoint-handler (handler event-latch))
      (send-off a (fn [_] (test-func 3 "test" #(.countDown finish-latch))))
      (is (.await event-latch 2 TimeUnit/SECONDS)))
    (testing "reval is able to determine proper values"
      (Thread/sleep 100)
      (is (= [3 "test"] (reval-test '[a b])))
      (is (= "\"#<Namespace cdt.test>\"\n" (reval-test '*ns*))))
    (testing "print-frame shows the frame"
      (is (= (with-out-str (print-frame (ct) (cf))) test-frame-str)))
    (testing "step goes to next line"
      (set-handler step-handler (handler step-latch))
      (step-over (ct))
      (Thread/sleep 100)
      (is (= (with-out-str (print-frame (ct) (cf))) step-frame-str)))
    ;;test failing because :file doesn't contain complete path
#_    (testing "line-bp causes breakpoint event"
      (let [file (:file (meta #'test-func))
            line (+ 2 (:line (meta #'test-func)))]
        (line-bp file line))
      (continue-vm)
      (Thread/sleep 100)
      (is (= 7 (reval-test 'd))))
    (is (= (with-out-str (print-frame (ct) (cf))) line-bp-frame-str))
    (testing "cont allows function to finish"
      (continue-vm)
      (is (.await finish-latch 2 TimeUnit/SECONDS)))
    (testing "delete-bp causes the function not to stop"
      (delete-bp cdt.test/test-func)
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
