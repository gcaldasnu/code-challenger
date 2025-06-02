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

(run-tests)

