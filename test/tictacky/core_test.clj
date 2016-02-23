(ns tictacky.core-test
  (:require [clojure.test :refer :all]
            [tictacky.core :refer :all]))

(deftest check-best-move-x
  (testing "best move for x"
    (let [board [[:o :o :x]
                 [:e :x :x]
                 [:o :x :o]]]
      (is (= [1 0] (best-move board :x))))))

(deftest check-best-move-o
  (testing "best move for o"
    (let [board [[:e :e :x]
                 [:o :x :x]
                 [:o :x :o]]]
      (is (= [0 0] (best-move board :o))))))

(deftest check-best-move-o-1
  (testing "best move for o"
    (let [board [[:e :e :x]
                 [:o :x :x]
                 [:o :x :o]]]
      (is (= [0 0] (best-move board :o))))))
