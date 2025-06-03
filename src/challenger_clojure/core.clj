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
  [inputs]
  (loop [opers inputs
         stack []]
    (if (empty? opers)
      (first stack)
      (let [token (first opers)
            rest-opers (next opers)
            result (handle-operation token stack)]
        (recur rest-opers result)))))

(defn filter-and-sum
  [values]
  (loop [list-values values
         sum 0]
    (if (empty? list-values)
      sum
      (let [value (first list-values)
            rest-list (next list-values)]
        (if (even? value)
          (recur rest-list (+ sum value))
          (recur rest-list sum))))))

(defn word-count
  [frase]
  (-> frase
      (str/split #" ")
      frequencies))

(defn find-max
  [values]
  (loop [vs values
         mx (first vs)]
    (if (empty? vs)
      mx
      (let [vl (first vs)
            next-vs (next vs)]
        (if (> vl mx)
          (recur next-vs vl)
          (recur next-vs mx))))))

(defn- str-in?
  [x coll]
  (some #(= x %) coll))

(defn compress-seq
  ([values] (compress-seq values []))
  ([values lst]
   (if (empty? values)
     lst
     (let [current-value  (first values)
           next-values (next values)]
       (if (str-in? current-value lst)
         (recur next-values lst)
         (recur next-values (conj lst current-value)))))))
