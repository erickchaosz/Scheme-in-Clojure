(ns scheme-interpreter.core
  (:refer-clojure :exclude [apply eval])
  (:require [scheme-interpreter.env :as environ])
  (:gen-class))


(declare eval apply
         self-evaluating?
         variable?
         operands operator
         make-procedure
         tagged-list?

         quoted? quoted-expression
         assignment? assignment-var assignment-val eval-assignment
         definition? definition-var definition-val eval-definition
         if? if-predicate if-action if-alternative eval-if
         lambda?
         begin? eval-sequence last-expression? first-expression rest-expressions
         cond?
         application?
         )

(def initial-env environ/empty-env)

(def apply-in-underlying-clojure clojure.core/apply)

(def primitives
  (list {'+ +}
        {'- -}
        {'/ /}
        {'* *}
        {'true true}
        {'false false}
        {'list list}
        {'cons cons}
        {'display print}
        {'newline (fn [] (println ""))}))

;;TO BE CHANGED 
(def initial-env
  (let [vars (map (comp first keys) primitives)
        vals (map (comp first vals) primitives)]
    (environ/extend-env vars vals initial-env)))

(defn eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (variable? exp) (environ/lookup-variable-value exp env)
        (quoted? exp) (quoted-expression exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (begin? exp) (eval-sequence (get-sequence exp) env)
        :else (apply (eval (operator exp) env)
                     (map (fn [exp]
                            (eval exp env)) (operands exp)))))

(defn apply
  [procedure arguments]
  (apply-in-underlying-clojure procedure arguments))


(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn make-procedure
  [parameters body env]
  (list 'procedure parameters body env))

(defn self-evaluating?
  [exp]
  (cond
   (number? exp) true
   (string? exp) true
   (true? exp) true
   (false? exp) true
   :else false))

(defn variable?
  [exp]
  (symbol? exp))

(defn tagged-list?
  [exp tag]
  (if (seq? exp)
    (= (first exp) tag)
    false))

(defn quoted?
  [exp]
  (tagged-list? exp 'quote))

(defn quoted-expression
  [exp]
  (first (rest exp)))

(defn assignment?
  [exp]
  (tagged-list? exp 'set!))

(defn assignment-var [exp] (first (rest exp)))
(defn assignment-val [exp] (first (rest (rest exp))))

(defn eval-assignment
  [exp env]
  (let [var (assignment-var exp)
        val (eval (assignment-val exp) env)]
    (do (environ/set-variable-value! var val env)
        val)))

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn definition-var [exp] (first (rest exp)))
(defn definition-val [exp] (first (rest (rest exp))))

(defn eval-definition
  [exp env]
  (let [var (definition-var exp)
        val (eval (definition-val exp) env)]
    (do (environ/define-variable! var val env)
        var)))

(defn if? [exp] (tagged-list? exp 'if))
(defn if-predicate [exp] (first (rest exp)))
(defn if-action [exp] (first (rest (rest exp))))
(defn if-alternative [exp] (first (rest (rest (rest exp)))))

(defn eval-if
  [exp env]
  (if (eval (if-predicate exp) env)
    (eval (if-action exp) env)
    (eval (if-alternative exp) env)))

(defn begin? [exp] (tagged-list? exp 'begin))
(defn get-sequence [exp] (rest exp))
(defn last-expression? [exp] (empty? (first (rest exp))))
(defn first-expression [exp] (first exp))
(defn rest-expressions [exp] (rest exp))

(defn eval-sequence
  [exp env]
  (if (last-expression? exp)
    (eval (first-expression exp) env)
    (do (eval (first-expression exp) env)
        (recur (rest exp) env ))))

(defn -main
  "Runs the read-eval-print-loop"
  [& args])
