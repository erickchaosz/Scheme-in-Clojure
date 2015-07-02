(ns scheme-interpreter.core
  (:refer-clojure :exclude [apply eval])
  (:require [scheme-interpreter.env :as environ])
  (:gen-class))


(declare eval apply apply-in-underlying-clojure
         self-evaluating?
         variable?
         operands operator
         primitive-proc? primitive-body

         compound-proc? proc-body proc-params proc-env
         
         make-procedure
         tagged-list?
         sequence->exp
         quoted? quoted-expression
         assignment? assignment-var assignment-val eval-assignment
         definition? definition-var definition-val eval-definition 
         if? if-predicate if-action if-alternative eval-if
         lambda? lambda-params lambda-body make-lambda
         begin? eval-sequence last-expression? first-expression rest-expressions get-sequence
         cond? cond-clauses cond-predicate cond-body last-cond? cond->if expand-cond-clauses
         let? let->combination expand-let-clauses
         and? expand-and-clauses and->combination
         or? expand-or-clauses or->combination
         application?
         )


(defn eval
  [exp env]
  (cond (self-evaluating? exp) exp
        (nil? exp) (list)
        (variable? exp) (environ/lookup-variable-value exp env)
        (quoted? exp) (quoted-expression exp)
        (assignment? exp) (eval-assignment exp env)
        (definition? exp) (eval-definition exp env)
        (if? exp) (eval-if exp env)
        (cond? exp) (eval (cond->if exp) env)
        (let? exp) (eval (let->combination exp) env)
        (lambda? exp) (make-procedure (lambda-params exp)
                                      (lambda-body exp)
                                      env)
        (begin? exp) (eval-sequence (get-sequence exp) env)
        (and? exp) (eval (and->combination exp) env)
        (or? exp) (eval (or->combination exp) env)
        :else (apply (eval (operator exp) env)
                     (map (fn [exp]
                            (eval exp env)) (operands exp)))))

(defn apply
  [procedure arguments]
  (if (primitive-proc? procedure)
    (apply-in-underlying-clojure (primitive-body procedure) arguments)
    (eval-sequence
     (proc-body procedure)
     (environ/extend-env (proc-params procedure)
                         arguments
                         (proc-env procedure)))))

(def apply-in-underlying-clojure clojure.core/apply)

(defn empty-coll? [items]
  (if (seq? items)
    (empty? items)
    false))

(defn operator
  [exp]
  (first exp))

(defn operands
  [exp]
  (rest exp))

(defn make-procedure
  [parameters body env]
  (list 'procedure parameters body env))

(defn primitive-proc? [exp] (tagged-list? exp 'primitive))
(defn primitive-body [exp] (first (rest exp)))

(defn compound-proc? [exp] (tagged-list? exp 'procedure))
(defn proc-params [exp] (first (rest exp)))
(defn proc-body [exp] (first (rest (rest exp))))
(defn proc-env [exp] (first (rest (rest (rest exp)))))

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
  (or (symbol? exp) (nil? exp)))

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

(defn definition-var [exp]
  (let [var (first (rest exp))]
    (if (seq? var)
      (first var)
      var)))

(defn definition-val [exp]
  (let [var (first (rest exp))
        def-body (sequence->exp (rest (rest exp)))]
    (if (seq? var)
      (let [def-params (rest var)]
        (do
          (make-lambda def-params def-body)))
      def-body)))

(defn eval-definition
  [exp env]
  (let [var (definition-var exp)
        val (eval (definition-val exp) env)]
    (do
      (environ/define-variable! var val env)
        var)))

(defn if? [exp] (tagged-list? exp 'if))
(defn if-predicate [exp] (first (rest exp)))
(defn if-action [exp] (first (rest (rest exp))))
(defn if-alternative [exp] (first (rest (rest (rest exp)))))

(defn make-if [pred action alternative] (list 'if pred action alternative))

(defn eval-if
  [exp env]
  (if (eval (if-predicate exp) env)
    (eval (if-action exp) env)
    (eval (if-alternative exp) env)))

(defn lambda? [exp] (tagged-list? exp 'lambda))
(defn lambda-params [exp] (first (rest exp)))
(defn lambda-body [exp] (rest (rest exp)))
(defn make-lambda [params body] (list 'lambda params body))


(defn begin? [exp] (tagged-list? exp 'begin))
(defn get-sequence [exp] (rest exp))
(defn last-expression? [exp] (empty? (rest exp)))
(defn first-expression [exp] (first exp))
(defn rest-expressions [exp] (rest exp))

(defn eval-sequence
  [exp env]
  (if (last-expression? exp)
    (eval (first-expression exp) env)
    (do (eval (first-expression exp) env)
        (recur (rest exp) env ))))

(defn sequence->exp [exp]
  (if (last-expression? exp)
    (first exp)
    (cons 'begin exp)))

(defn cond? [exp] (tagged-list? exp 'cond))
(defn cond-clauses [exp] (rest exp))
(defn cond-predicate [exp] (first exp))
(defn cond-body [exp] (rest exp))
(defn last-cond? [exp] )
 
(defn cond->if [exp]
  (expand-cond-clauses (cond-clauses exp)))

(defn expand-cond-clauses [clauses]
  (let [curr-clause (first clauses)
        curr-body (sequence->exp (cond-body curr-clause))]
    (if (empty? clauses)
      nil
      (if (tagged-list? curr-clause 'else)
        (make-if true curr-body curr-body)
        (make-if (cond-predicate curr-clause)
                 curr-body
                 (expand-cond-clauses (rest clauses)))))))

(defn let? [exp] (tagged-list? exp 'let))
(defn let-clauses [exp] (rest exp))
(defn let-bindings [exp] (first exp))
(defn let-body [exp] (rest exp))

(defn let->combination [exp]
  (let [clauses (let-clauses exp)
        bindings (let-bindings clauses)
        body (sequence->exp (let-body clauses))]
    (expand-let-clauses bindings body)))

(defn expand-let-clauses [bindings body]
  (let [let-vars (map first bindings)
        let-actions (map (comp first rest) bindings)]
    (concat (list (list 'lambda let-vars body))
            let-actions)))


(defn and? [exp] (tagged-list? exp 'and))
(defn and-clauses [exp] (rest exp))
(defn and-action [clauses] (first clauses))

(defn and->combination [exp]
  (expand-and-clauses (and-clauses exp)))

(defn expand-and-clauses [clauses]
  (let [curr-clause (and-action clauses)]
    (if (empty? clauses)
      'true
      (if (last-expression? clauses)
        (make-if curr-clause
                 curr-clause
                 'false)
        (make-if curr-clause
                 (expand-and-clauses (rest clauses))
                 'false)))))

(defn or? [exp] (tagged-list? exp 'or))
(defn or-clauses [exp] (rest exp))
(defn or-action [clauses] (first clauses))

(defn or->combination [exp]
  (expand-or-clauses (or-clauses exp)))

(defn expand-or-clauses [clauses]
  (let [curr-clause (or-action clauses)]
    (if (empty? clauses)
      'false
      (if (last-expression? clauses)
        (make-if curr-clause
                 curr-clause
                 'false)
        (make-if curr-clause
                 curr-clause
                 (expand-or-clauses (rest clauses)))))))

(def primitives
  (list {'+ +}
        {'- -}
        {'/ /}
        {'* *}
        {'true true}
        {'True true}
        {'false false}
        {'False false}
        {'> >}
        {'< <}
        {'<= <=}
        {'>= >=}
        {'= =}
        {'length count}
        {'eq? =}
        {'null? empty-coll?}
        {'nil (list)}
        {'apply apply}
        {'remainder rem}
        {'car first}
        {'cdr rest}
        {'list list}
        {'cons cons}
        {'not not}
        {'pair? seq?}
        {'append concat}
        {'display print}
        {'newline (fn [] (println ""))}))

(defn init-env [env]
  (let [vars (map (comp first keys) primitives)
        vals (map (fn [primitive] (list 'primitive ((comp first vals) primitive))) primitives)]
    (environ/extend-env vars vals env)))



(defn driver-loop [env]
  (print ">>> ")
  (flush)
  (println (eval (read) env))
  (flush)
  (driver-loop env)
  )

(defn -main
  "Runs the read-eval-print-loop"
  [& args]
  (let [env (init-env environ/empty-env)]
    (driver-loop env)))

