(ns challenger-clojure.core-test
  (:require [clojure.test :refer :all]
            [clojure.test.check :refer :all]
            [challenger-clojure.core :refer :all]))

(deftest evaluate-rpn-test
  (testing "Check expression results"
    (are [express value] (= (evaluate-rpn express) value)
      [3 4 +] 7
      [5 1 2 + 4 * + 3 -] 14
      [1 2 3 + +] 6
      [1 2 "a"] nil)))

(deftest filter-and-sum-test
  (testing "Validate that only even values is sum"
    (are [express expected-value] (= (filter-and-sum express) expected-value)
      [1 2 3 4 5 6 7 8 9 10] 30
      [1 3 4 6 7 3 3] 10)))

(deftest word-count-test
  (testing "Validate word count"
    (are [word expect] (= (word-count word) expect)
      "hello world hello" {"hello" 2 "world" 1}
      "Clojure is fun and Clojure is powerful" {"Clojure" 2, "is" 2, "fun" 1, "and" 1, "powerful" 1})))

(deftest find-max-test
  (testing "Validate if query returns the biggest"
    (are [values expect] (= (find-max values) expect)
      [1 2 3 4 5] 5
      [-10 -5 -3 -20] -3)))

(deftest compress-seq-test
  (testing "Validate compress sequence result"
    (are [values expect] (= (compress-seq values) expect)
      [1 1 2 2 2 3 3 1 1 1] [1 2 3]
      ["a" "a" "b" "c" "c" "c" "d"] ["a" "b" "c" "d"])))

(run-tests)
