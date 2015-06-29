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
         441 "(square 21)"
         'square "(define square (lambda (x) (* x x)))"
         441 "(square 21)"
         49 "(square (+ 2 5))"
         81 "(square (square 3))"
         'sum-of-squares "(define (sum-of-squares x y)
                           (+ (square x) (square y)))"
         25 "(sum-of-squares 3 4)"
         'f "(define (f a)
              (sum-of-squares (+ a 1) (* a 2)))"
         136 "(f 5)"))) 


(deftest conditionals 
 (testing "Cond"
    (are [result exp] (= result (si/eval (read-string exp) si/initial-env
                                 ))
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

))) 
