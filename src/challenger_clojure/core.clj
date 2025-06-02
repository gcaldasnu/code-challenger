(ns challenger-clojure.core)
;; Access the link to understand what motivated the code below.
;; Challenge: https://github.com/nubank/tech-learning-clojure-service-stack-enabler/tree/main/00003-getting-comfortable-with-clojure/pt#descri%C3%A7%C3%A3o-do-desafio

(def ^:private allowed-operations [+ - * /])

(defn- valid-operation?
  [operation]
  (contains? allowed-operations operation))

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
        number-opers 2
        partial-stack (nthnext stack number-opers)]
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
        (recur result rest-opers)))))

