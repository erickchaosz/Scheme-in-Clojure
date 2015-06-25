(ns scheme-interpreter.core
  (:gen-class))

(declare driver-loop my-eval operator operands self-evaluating?)

(defn -main
  "Runs the read-eval-print-loop"
  [& args]
  (driver-loop))

(defn driver-loop
  []
  (do (println (my-eval (read)))
      (driver-loop)))

(defn my-eval
  [exp]
  ;;(println exp)
  (if (self-evaluating? exp)
    exp
    (apply (eval (operator exp)) (map my-eval (operands exp)))))

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn self-evaluating?
  [exp]
  (cond
   (number? exp) true
   (string? exp) true
   :else false))
