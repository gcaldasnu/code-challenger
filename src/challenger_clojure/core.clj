(ns challenger-clojure.core
  (:require [clojure.string :as str]))
;; Access the link above to understand what motivated the code below.
;; Challenge: https://github.com/nubank/tech-learning-clojure-service-stack-enabler/tree/main/00003-getting-comfortable-with-clojure/pt#descri%C3%A7%C3%A3o-do-desafio

(def ^:private allowed-operations [+ - * /])

(defn- valid-operation?
  [operation]
  (some #(= operation %) allowed-operations))

(defn- execute-operation-in-stack
  [oper stack]
  (if (valid-operation? oper)
    (let [operand1 (second stack)
          operand2 (first stack)]
      (oper operand1 operand2))
    nil))

(defn- operate-in-stack
  [operation stack]
  (let [result (execute-operation-in-stack operation stack)
        opers-number 2
        partial-stack (nthnext stack opers-number)]
    (cons result partial-stack)))

(defn- handle-operation
  [token stack]
  (if (number? token)
    (cons token stack)
    (operate-in-stack token stack)))

(defn evaluate-rpn
  [coll]
  (loop [opers coll
         stack []]
    (if (empty? opers)
      (first stack)
      (let [token (first opers)
            rest-opers (next opers)
            result (handle-operation token stack)]
        (recur rest-opers result)))))

(defn filter-and-sum
  [coll]
  (loop [values coll
         sum 0]
    (if (empty? values)
      sum
      (let [value (first values)
            rest-list (next values)]
        (->> (if (even? value)
               (+ sum value)
               sum)
             (recur rest-list))))))

(defn word-count
  [phrase]
  (-> phrase
      (str/split #" ")
      frequencies))

(defn- biggest
  [x y]
  (if (> x y) x y))

(defn- get-max
  [coll maxx]
  (-> coll
      first
      (biggest maxx)))

(defn find-max
  [coll]
  (loop [values coll
         maxx (first values)]
    (if (empty? values)
      maxx
      (->> maxx
           (get-max values)
           (recur (next values))))))
