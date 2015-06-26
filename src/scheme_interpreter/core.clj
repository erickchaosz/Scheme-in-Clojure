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
         assignment?
         definition? definition-var definition-val eval-definition
         if?
         lambda?
         begin?
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
        {'list list}
        {'cons cons}))

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
        (definition? exp) (eval-definition exp env)
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

(defn definition?
  [exp]
  (tagged-list? exp 'define))

(defn definition-var [exp] (first (rest exp)))
(defn definition-val [exp] (first (rest (rest exp))))

(defn eval-definition
  [exp env]
  (let [var (definition-var exp)
        val (definition-val exp)]
    (do (environ/define-variable! var val env)
        var)))

(defn -main
  "Runs the read-eval-print-loop"
  [& args])
