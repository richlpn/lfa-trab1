(ns clirc.bool-logic-test
  (:require [clojure.test :refer [are deftest is testing]]
            [clirc.bool-logic :refer :all]))


(deftest bit-get-test
  (testing "Bit reading"
    (are [n b] (= (bit-get 2r1111000110 n) b)
      0 0
      1 1
      2 1
      3 0
      4 0
      5 0
      6 1
      7 1
      8 1
      9 1)))


(deftest min-bitvec-size-test
  (testing "Minimum required bitvec size"
    (are [m n] (= (min-bitvec-size m) n)
      0 1
      1 1
      2 2
      3 2
      4 3
      6 3
      7 3
      8 4
      9 4
      12 4
      15 4
      16 5
      20 5
      30 5
      32 6
      48 6
      63 6
      64 7
      1023 10
      1024 11)))

(deftest int->bitvec-test
  (testing "Converting integer to bitvec"
    (are [n v] (= (int->bitvec n) (into [] (reverse v)))
      0 [0]
      1 [1]
      2 [1 0]
      3 [1 1]
      4 [1 0 0]
      7 [1 1 1]
      8 [1 0 0 0]
      11 [1 0 1 1]
      15 [1 1 1 1]
      16 [1 0 0 0 0]
      25 [1 1 0 0 1]
      31 [1 1 1 1 1]
      1022 [1 1 1 1 1 1 1 1 1 0])))

(deftest map->bitvec-test
  (testing "Converting maps to bitvec"
    (are [m v] (= v (map->bitvec m))
      {} []
      {0 0} [0]
      {0 0, 1 1, 2 2} [0 1 2]
      {3 3} [nil nil nil 3]
      {1 1, 2 2, 4 4, 8 8} [nil 1 2 nil 4 nil nil nil 8])))

