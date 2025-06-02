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

(run-tests)

