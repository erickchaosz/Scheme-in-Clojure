(ns scheme-interpreter.core-test
  (:refer-clojure :exclude [eval])
  (:require [clojure.test :refer :all]
            [scheme-interpreter.core :as si]
            ))


(deftest arithmetic
  (testing "Arithmetics"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
         10 "10"
         486 "(+ 137 349)"
         495 "(* 5 99)"
         666 "(- 1000 334)"
         2 "(/ 10 5)"
         12.7 "(+ 2.7 10)"
         75 "(+ 21 35 12 7)"
         1200 "(* 25 4 12)"
         19 "(+ (* 3 5) (- 10 6))"
         57 "(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))"
         57 "(+ (* 3
                   (+ (* 2 4)
                      (+ 3 5)))
                (+ (- 10 7)
                   6))")))

(deftest definition-lambda
 (testing "Definition and lambda"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
         'size "(define size 2)"
         2 "size"
         10 "(* 5 size)"
         'pi "(define pi 3.14159)"
         'radius "(define radius 10)"
         314.159 "(* pi (* radius radius))"
         'circumference "(define circumference (* 2 pi radius))"
         62.8318 "circumference"
         'square "(define (square x) (* x x))"
         441 "(square 21)"))) 


(deftest conditionals-and-let
 (testing "Cond and let"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
         'square "(define square (lambda (x) (* x x)))"
         441 "(square 21)"
         49 "(square (+ 2 5))"
         81 "(square (square 3))"
         'sum-of-squares "(define (sum-of-squares x y)
                           (+ (square x) (square y)))"
         25 "(sum-of-squares 3 4)"
         'f "(define (f a)
              (sum-of-squares (+ a 1) (* a 2)))"
         136 "(f 5)"
         'abs "(define (abs x)
                 (cond ((> x 0) x)
                 ((= x 0) 0)
                 ((< x 0) (- x))))"
         3 "(abs -3)"
         0 "(abs 0)"
         3 "(abs 3)"
         'a-plus-abs-b "(define (a-plus-abs-b a b)
                          ((if (> b 0) + -) a b))"
         5 "(a-plus-abs-b 3 -2)"
         'sqrt-iter "(define (sqrt-iter guess x)
                       (if (good-enough? guess x)
                         guess
                         (sqrt-iter (improve guess x) x)))"
         'improve "(define (improve guess x)
                     (average guess (/ x guess)))"
         'average "(define (average x y)
                     (/ (+ x y) 2))"
         'good-enough? "(define (good-enough? guess x)
                          (< (abs (- (square guess) x)) 0.001))"
         'sqrt "(define (sqrt x)
                  (sqrt-iter 1.0 x))"
         3.00009155413138 "(sqrt 9)"
         11.704699917758145 "(sqrt (+ 100 37))"
         1.7739279023207892 "(sqrt (+ (sqrt 2) (sqrt 3)))"
         1000.000369924366 "(square (sqrt 1000))"
         'sqrt "(define (sqrt x)
                  (define (good-enough? guess)
                    (< (abs (- (square guess) x)) 0.001))
                  (define (improve guess)
                    (average guess (/ x guess)))
                  (define (sqrt-iter guess)
                    (if (good-enough? guess)
                      guess
                      (sqrt-iter (improve guess))))
                  (sqrt-iter 1.0))"
         3.00009155413138 "(sqrt 9)"
         11.704699917758145 "(sqrt (+ 100 37))"
         1.7739279023207892 "(sqrt (+ (sqrt 2) (sqrt 3)))"
         1000.000369924366 "(square (sqrt 1000))"
         'cube "(define (cube x) (* x x x))"
         'sum "(define (sum term a next b)
                 (if (> a b)
                   0
                   (+ (term a)
                      (sum term (next a) next b))))"
         'inc "(define (inc n) (+ n 1))"
         'sum-cubes "(define (sum-cubes a b)
                       (sum cube a inc b))"
         3025 "(sum-cubes 1 10)"
         'identity "(define (identity x) x)"
         'sum-integers "(define (sum-integers a b)
                          (sum identity a inc b))"
         55 "(sum-integers 1 10)"
         12 "((lambda (x y z) (+ x y (square z))) 1 2 3)"
         'f "(define (f x y)
               (let ((a (+ 1 (* x y)))
                     (b (- 1 y)))
                 (+ (* x (square a))
                    (* y b)
                    (* a b))))"
         456 "(f 3 4)"
         'x "(define x 5)"
         38 "(+ (let ((x 3))
                  (+ x (* x 10)))
                x)"
         21 "(let ((x 3)
                   (y (+ x 2)))
               (* x y))"
))) 


(deftest lists
 (testing "List operations"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
         'abs "(define (abs x)
                 (cond ((> x 0) x)
                 ((= x 0) 0)
                 ((< x 0) (- x))))"
         'one-through-four "(define one-through-four (list 1 2 3 4))"
         '(1 2 3 4) "one-through-four"
         1 "(car one-through-four)"
         '(2 3 4) "(cdr one-through-four)"
         2 "(car (cdr one-through-four))"
         '(10 1 2 3 4) "(cons 10 one-through-four)"
         'map "(define (map proc items)
                 (if (null? items)
                   nil
                   (cons (proc (car items))
                         (map proc (cdr items)))))"
         '(10 2.5 11.6 17) "(map abs (list -10 2.5 -11.6 17))"
         '(1 4 9 16) "(map (lambda (x) (* x x))
     (list 1 2 3 4))"
         'scale-list "(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))"
         '(10 20 30 40 50) "(scale-list (list 1 2 3 4 5) 10)"
         'count-leaves "(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))"
         'x "(define x (cons (list 1 2) (list 3 4)))"
         4 "(count-leaves x)"
         8 "(count-leaves (list x x))"
         'odd? "(define (odd? x) (= 1 (remainder x 2)))"
         'filter "(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))"
         '(1 3 5) "(filter odd? (list 1 2 3 4 5))"
         'accumulate "(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))"
         15 "(accumulate + 0 (list 1 2 3 4 5))"
         120 "(accumulate * 1 (list 1 2 3 4 5))"
         '(1 2 3 4 5) "(accumulate cons (list) (list 1 2 3 4 5))"
         'enumerate-interval "(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))"
         '(2 3 4 5 6 7) "(enumerate-interval 2 7)"
         'enumerate-tree "(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))"
         '(1 2 3 4 5) "(enumerate-tree (list 1 (list 2 (list 3 4)) 5))"
         
         )))
 
(deftest two-three-one
 (testing "2.3.1"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
         'a "(define a 1)"
         'b "(define b 2)"
         '(1 2) "(list a b)"
         '(a b) "(list 'a 'b)"
         '(a 2) "(list 'a b)"
         'a "(car '(a b c))"
         '(b c) "(cdr '(a b c))"
         'memq "(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))"
         'false "(memq 'apple '(pear banana prune))"
         '(apple pear) "(memq 'apple '(x (apple sauce) y apple pear))"
         'equal? "(define (equal? x y)
  (cond ((pair? x) (and (pair? y)
                        (equal? (car x) (car y))
                        (equal? (cdr x) (cdr y))))
        ((null? x) (null? y))
        (else (eq? x y))))"
        

         )))
         
(deftest Peter-Norvig-tests
 (testing "From Peter Norvig http://norvig.com/lispy2.html"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
         'square "(define square (lambda (x) (* x x)))"
         'double "(define double (lambda (x) (* 2 x)))"
         10 "(double 5)"
         'compose "(define compose (lambda (f g) (lambda (x) (f (g x)))))"
         '(10) "((compose list double) 5)"
         'apply-twice "(define apply-twice (lambda (f) (compose f f)))"
         20 "((apply-twice double) 5)"
         80 "((apply-twice (apply-twice double)) 5)"
         'fact "(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))"
         6 "(fact 3)"
         'combine "(define (combine f)
  (lambda (x y)
    (if (null? x) nil
      (f (list (car x) (car y))
         ((combine f) (cdr x) (cdr y))))))"
         'zip "(define zip (combine cons))"
         '((1 5) (2 6) (3 7) (4 8)) "(zip (list 1 2 3 4) (list 5 6 7 8))"
         'riff-shuffle "(define riff-shuffle (lambda (deck) (begin
    (define take (lambda (n seq) (if (<= n 0) (quote ()) (cons (car seq) (take (- n 1) (cdr seq))))))
    (define drop (lambda (n seq) (if (<= n 0) seq (drop (- n 1) (cdr seq)))))
    (define mid (lambda (seq) (/ (length seq) 2)))
    ((combine append) (take (mid deck) deck) (drop (mid deck) deck)))))"
         '(1 5 2 6 3 7 4 8) "(riff-shuffle (list 1 2 3 4 5 6 7 8))"
         '(1 3 5 7 2 4 6 8) "((apply-twice riff-shuffle) (list 1 2 3 4 5 6 7 8))"
         '(1 2 3 4 5 6 7 8) "(riff-shuffle (riff-shuffle (riff-shuffle (list 1 2 3 4 5 6 7 8))))"
         4 "(apply square '(2))"
         10 "(apply + '(1 2 3 4))"
         '(1 2 3 4) "(apply (if false + append) '((1 2) (3 4)))"
         1 "(if 0 1 2)"
         1 "(if '() 1 2)"
         'true "(or false true)"
         'false "(or)"
         'true "(and)"
         1 "(or 1 2 3)"
         3 "(and 1 2 3)"
         'false "(and false (/ 1 0))"
         3 "(or 3 (/ 1 0))"
         'hello "(or (quote hello) (quote world))"
         1 "(if nil 1 2)"
         1 "(if 0 1 2)"
         4 "(let* ((a 1)
                   (b (+ a 1))
                   (c (+ b 2))) c)"
         'fib "(define (fib n)
                 (let fib-iter ((a 1)
                                (b 0)
                                (count n))
                   (if (= count 0)
                       b
                       (fib-iter (+ a b) a (- count 1)))))"
         13 "(fib 7)"
         )))
