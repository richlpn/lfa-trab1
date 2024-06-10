(ns clirc.core-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clirc.core :refer :all]))

(def env0 {'temp1 0
           'temp2 0
           'temp3 1
           'a1 1
           'a2 0
           :in [0 0 0 1 1 0 1 1]
           :out {0 0, 1 1, 2 0, 3 1, 5 0, 6 1, 7 0}})

(def env1 (merge aon-funcs env0))

(deftest test-eval-expr
  (testing "Variable evaluation"
    (are [expr value] (= (eval-expr expr env0) value)
      'temp1 0
      'temp2 0
      'temp3 1
      'a1 1
      'a2 0))
  (testing "Input evaluation"
    (are [index value] (= (eval-expr `(:in ~index) env0) value)
      0 0
      1 0
      2 0
      3 1
      4 1
      5 0
      6 1
      7 1))
  (testing "Outpt evaluation"
    (are [index value] (= (eval-expr `(:out ~index) env0) value)
      0 0
      1 1
      2 0
      3 1
      4 nil
      5 0
      6 1
      7 0)))


(deftest test-eval-funcall
  (testing "Undefined function in env0"
    (are [funcall] (thrown? Exception (eval-funcall funcall env0))
      '(and temp1 temp2)
      '(or temp1 temp2)
      '(not a1)))
  (testing "Undefined function in env1"
    (are [funcall] (thrown? Exception (eval-funcall funcall env0))
      '(if temp1 temp2 temp3)
      '(nand temp1 temp2)
      '(zero a1)))
  (testing "Wrong number of arguments"
    (are [funcall] (thrown? Exception (eval-funcall funcall env1))
      '(and temp1)
      '(or temp1 temp2 temp3)
      '(not temp2 temp3)
      '(not)))
  (testing "Function call evaluation"
    (are [funcall result] (= result (eval-funcall funcall env1))
      '(and temp1 temp2) 0
      '(and temp2 temp3) 0
      '(and temp3 a1) 1
      '(not (:in 0)) 1
      '(not (:in 3)) 0
      '(not (:out 0)) 1
      '(not (:out 3)) 0
      '(or a1 a2) 1
      '(or a2 a2) 0
      '(or temp1 temp2) 0)))


(def basic-aon
  '[(set! (:out 0) (not (:in 0)))
    (set! (:out 1) (not (:in 1)))
    (set! (:out 2) (and (:in 0) (:in 1)))
    (set! (:out 3) (or (:in 0) (:in 1)))])

(def basic-izo
  '[(set! (:out 0) (zero (:in 0)))
    (set! (:out 1) (one (:in 1)))
    (set! ichi (one (:in 0)))
    (set! rei (zero (:in 0)))
    (set! (:out 2) (if (:in 2) ichi rei))
    (set! (:out 3) (if (:in 3) rei ichi))])

(def basic-nand
  '[(set! (:out 0) (nand (:in 0) (:in 1)))
    (set! (:out 1) (nand (:in 0) (:in 0)))
    (set! (:out 2) (nand (:in 1) (:in 1)))])

(def basic-nor
  '[(set! (:out 0) (nor (:in 0) (:in 1)))
    (set! (:out 1) (nor (:in 0) (:in 0)))
    (set! (:out 2) (nor (:in 1) (:in 1)))])


(deftest test-eval-prog-predefs
  (testing "Basic AON function evaluation"
    (are [input output] (= (eval-prog-aon basic-aon input) output)
      [0 0] [1 1 0 0]
      [0 1] [1 0 0 1]
      [1 0] [0 1 0 1]
      [1 1] [0 0 1 1]))
  (testing "Basic IZO function evaluation"
    (are [input output] (= (eval-prog-izo basic-izo input) output)
      [0 0 0 0] [0 1 0 1]
      [0 0 0 1] [0 1 0 0]
      [0 0 1 0] [0 1 1 1]
      [0 0 1 1] [0 1 1 0]
      [0 1 0 0] [0 1 0 1]
      [0 1 0 1] [0 1 0 0]
      [0 1 1 0] [0 1 1 1]
      [0 1 1 1] [0 1 1 0]
      [1 0 0 0] [0 1 0 1]
      [1 0 0 1] [0 1 0 0]
      [1 0 1 0] [0 1 1 1]
      [1 0 1 1] [0 1 1 0]
      [1 1 0 0] [0 1 0 1]
      [1 1 0 1] [0 1 0 0]
      [1 1 1 0] [0 1 1 1]
      [1 1 1 1] [0 1 1 0]))
  (testing "Basic NAND function evaluation"
    (are [input output] (= (eval-prog-nand basic-nand input) output)
      [0 0] [1 1 1]
      [0 1] [1 1 0]
      [1 0] [1 0 1]
      [1 1] [0 0 0]))
  (testing "Basic NOR function evaluation"
    (are [input output] (= (eval-prog-nor basic-nor input) output)
      [0 0] [1 1 1]
      [0 1] [0 1 0]
      [1 0] [0 0 1]
      [1 1] [0 0 0])))


(def xor
  ;; w1 = AND(X[0], X[1])
  ;; w2 = NOT(w1)
  ;; w3 = OR(X[0], X[1])
  ;; Y[0] = AND(w2, w3)
  '[(set! w1 (and (:in 0) (:in 1)))
    (set! w2 (not w1))
    (set! w3 (or (:in 0) (:in 1)))
    (set! (:out 0) (and w2 w3))])

(def xor3
  ;; # Com açúcar sintático
  ;; result = XOR(X[0], XOR(X[1], X[2]))
  ;;
  ;; # Eliminando a chamada inline
  ;; __xor3__1 = XOR(X[1], X[2])
  ;; result = XOR(X[0], __xor3__1)
  ;;
  ;; # Eliminando a função definida pelo usuário
  ;; w1$xor$1 = AND(X[1], X[2])
  ;; w2$xor$1 = NOT(w1$xor$1)
  ;; w3$xor$1 = OR(X[1], X[2])
  ;; __xor3__1 = AND(w2$xor$1, w3$xor$1)
  ;; w1$xor$2 = AND(X[0], __xor3__1)
  ;; w2$xor$2 = NOT(w1$xor$2)
  ;; w3$xor$2 = OR(X[0], __xor3__1)
  ;; Y[0] = AND(w2$xor$2, w3$xor$2)
  '[(set! w1$xor$1 (and (:in 1) (:in 2)))
    (set! w2$xor$1 (not w1$xor$1))
    (set! w3$xor$1 (or (:in 1) (:in 2)))
    (set! __xor3__1 (and w2$xor$1 w3$xor$1))
    (set! w1$xor$2 (and (:in 0) __xor3__1))
    (set! w2$xor$2 (not w1$xor$2))
    (set! w3$xor$2 (or (:in 0) __xor3__1))
    (set! (:out 0) (and w2$xor$2 w3$xor$2))])

(def maj3
  ;; a1   = AND (X0,X1)
  ;; a2   = AND (X1,X2)
  ;; a3   = AND (X0,X2)
  ;; temp = OR (a2,a3)
  ;; Y[0] = OR (a1,temp)
  '[(set! a1 (and (:in 0) (:in 1)))
    (set! a2 (and (:in 1) (:in 2)))
    (set! a3 (and (:in 0) (:in 2)))
    (set! temp (or a2 a3))
    (set! (:out 0) (or a1 temp))])

(def cmp2
  ;; temp_1 = NOT(X[2])
  ;; temp_2 = AND(X[0],temp_1)
  ;; temp_3 = OR(X[0],temp_1)
  ;; temp_4 = NOT(X[3])
  ;; temp_5 = AND(X[1],temp_4)
  ;; temp_6 = AND(temp_5,temp_3)
  ;; Y[0] = OR(temp_2,temp_6)
  '[(set! temp1 (not (:in 2)))
    (set! temp2 (and (:in 0) temp1))
    (set! temp3 (or (:in 0) temp1))
    (set! temp4 (not (:in 3)))
    (set! temp5 (and (:in 1) temp4))
    (set! temp6 (and temp5 temp3))
    (set! (:out 0) (or temp2 temp6))])

(def add2
  ;; Sejam A e B dois números binários de 2 bits. A e B será representados por
  ;; A=X[0],X[1] e B=X[2],X[3]. Desse modo, a entrata de dados [1 0 1 1]
  ;; representará os números 2 e 3 (A = 10, B=11). O programa abaixo calcula a
  ;; soma dos números A e B e dá o resultado como ichi número de 3 bits, onde Y[0]
  ;; conterá o bit mais significativo e Y[2] o menos significativo.
  ;;
  ;; # Usando o açúcar sintático
  ;; Y[2] = XOR(X[1], X[3])
  ;; c1   = AND(X[1], X[3])
  ;; Y[1] = XOR3(c1, X[0], X[2])
  ;; Y[0] = MAJ3(c1, X[0], X[2])
  ;;
  ;; # Sem açúcar sintático
  ;; w1$xor$1 = AND(X[1], X[3])
  ;; w2$xor$1 = NOT(w1$xor$1)
  ;; w3$xor$1 = OR(X[1], X[3])
  ;; result$xor$1 = AND(w2$xor$1, w3$xor$1)
  ;; Y[2] = OR(result$xor$1, result$xor$1)
  ;; c1 = AND(X[1], X[3])
  ;; w1$xor$2 = AND(X[0], X[2])
  ;; w2$xor$2 = NOT(w1$xor$2)
  ;; w3$xor$2 = OR(X[0], X[2])
  ;; __xor3__1 = AND(w2$xor$2, w3$xor$2)
  ;; w1$xor$3 = AND(c1, __xor3__1)
  ;; w2$xor$3 = NOT(w1$xor$3)
  ;; w3$xor$3 = OR(c1, __xor3__1)
  ;; Y[1] = AND(w2$xor$3, w3$xor$3)
  ;; a1$maj3$1 = AND(c1, X[0])
  ;; a2$maj3$1 = AND(X[0], X[2])
  ;; a3$maj3$1 = AND(c1, X[2])
  ;; temp$maj3$1 = OR(a2$maj3$1, a3$maj3$1)
  ;; Y[0] = OR(a1$maj3$1, temp$maj3$1)
  '[(set! w1$xor$1 (and (:in 1) (:in 3)))
    (set! w2$xor$1 (not w1$xor$1))
    (set! w3$xor$1 (or (:in 1) (:in 3)))
    (set! result$xor$1 (and w2$xor$1 w3$xor$1))
    (set! (:out 2) (or result$xor$1 result$xor$1))
    (set! c1 (and (:in 1) (:in 3)))
    (set! w1$xor$2 (and (:in 0) (:in 2)))
    (set! w2$xor$2 (not w1$xor$2))
    (set! w3$xor$2 (or (:in 0) (:in 2)))
    (set! __xor3__1 (and w2$xor$2 w3$xor$2))
    (set! w1$xor$3 (and c1 __xor3__1))
    (set! w2$xor$3 (not w1$xor$3))
    (set! w3$xor$3 (or c1 __xor3__1))
    (set! (:out 1) (and w2$xor$3 w3$xor$3))
    (set! a1$maj3$1 (and c1 (:in 0)))
    (set! a2$maj3$1 (and (:in 0) (:in 2)))
    (set! a3$maj3$1 (and c1 (:in 2)))
    (set! temp$maj3$1 (or a2$maj3$1 a3$maj3$1))
    (set! (:out 0) (or a1$maj3$1 temp$maj3$1))])


(deftest test-eval-prog-aon
  (testing "XOR function evaluation"
    (are [input output] (= (eval-prog-aon xor input) output)
      [0 0] [0]
      [0 1] [1]
      [1 0] [1]
      [1 1] [0]))
  (testing "XOR3 function evaluation"
    (are [input output] (= output (eval-prog-aon xor3 input))
      [0 0 0] [0]
      [0 0 1] [1]
      [0 1 0] [1]
      [0 1 1] [0]
      [1 0 0] [1]
      [1 0 1] [0]
      [1 1 0] [0]
      [1 1 1] [1]))
  (testing "MAJ3 function evalutation"
    (are [input output] (= output (eval-prog-aon maj3 input))
      [0 0 0] [0]
      [0 0 1] [0]
      [0 1 0] [0]
      [0 1 1] [1]
      [1 0 0] [0]
      [1 0 1] [1]
      [1 1 0] [1]
      [1 1 1] [1]))
  (testing "CMP2 function evaluation"
    (are [input output] (= output (eval-prog-aon cmp2 input))
      [0 0 0 0] [0]
      [0 0 0 1] [0]
      [0 0 1 0] [0]
      [0 0 1 1] [0]
      [0 1 0 0] [1]
      [0 1 0 1] [0]
      [0 1 1 0] [0]
      [0 1 1 1] [0]
      [1 0 0 0] [1]
      [1 0 0 1] [1]
      [1 0 1 0] [0]
      [1 0 1 1] [0]
      [1 1 0 0] [1]
      [1 1 0 1] [1]
      [1 1 1 0] [1]
      [1 1 1 1] [0]))
  (testing "ADD2 function evaluation"
    (are [input output] (= output (eval-prog-aon add2 input))
      [0 0 0 0] [0 0 0]
      [0 0 0 1] [0 0 1]
      [0 0 1 0] [0 1 0]
      [0 0 1 1] [0 1 1]
      [0 1 0 0] [0 0 1]
      [0 1 0 1] [0 1 0]
      [0 1 1 0] [0 1 1]
      [0 1 1 1] [1 0 0]
      [1 0 0 0] [0 1 0]
      [1 0 0 1] [0 1 1]
      [1 0 1 0] [1 0 0]
      [1 0 1 1] [1 0 1]
      [1 1 0 0] [0 1 1]
      [1 1 0 1] [1 0 0]
      [1 1 1 0] [1 0 1]
      [1 1 1 1] [1 1 0])))
