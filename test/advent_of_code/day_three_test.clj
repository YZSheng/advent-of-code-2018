(ns advent-of-code.day-three-test
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn get-positions [claim]
  (let [[_ _ starting-position dimension] (str/split claim #":? ")
        [x y] (map read-string (str/split starting-position #","))
        [width height] (map read-string (str/split dimension #"x"))
        x-range (range x (+ x width))
        y-range (range y (+ y height))]
    (for [x x-range y y-range]
      (vector x y))))

(deftest find-all-the-positions-for-given-claim
  (testing "find all the positions for a given claim"
    (let [positions [[3 1] [3 2] [4 1] [4 2]]]
      (= positions (get-positions "#1 @ 3,1: 2x2")))))