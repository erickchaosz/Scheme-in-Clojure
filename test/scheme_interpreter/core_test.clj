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
    (is (= 'size (si/eval (read-string "(define size 2)") si/initial-env)))
    (is (= 2 (si/eval (read-string "size") si/initial-env)))))


