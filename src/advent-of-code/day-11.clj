(ns advent-of-code.day-11
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

;; For an explanation about how this works see:
;; https://www.redblobgames.com/grids/hexagons/#distances-cube

(def dirs
  {"n" [0 1 -1]
   "s" [0 -1 1]
   "ne" [1 0 -1]
   "sw" [-1 0 1]
   "se" [1 -1 0]
   "nw" [-1 1 0]})

(defn sum-pos [[x1 y1 z1] [x2 y2 z2]]
  [(+ x1 x2) (+ y1 y2) (+ z1 z2)])

(defn dist [[x y z]]
  (/ (+ (Math/abs x) (Math/abs y) (Math/abs z)) 2))

(defn part-1 [input]
  (->> (str/split input #",")
       (map dirs)
       (reduce sum-pos)
       (dist)))

(defn part-2 [input]
  (->> (str/split input #",")
       (map dirs)
       (reductions sum-pos)
       (map dist)
       (reduce max)))

(deftest part-1-tests
  (testing "part 1 tests"
    (is (= 3 (part-1 "ne,ne,ne")))
    (is (= 0 (part-1 "ne,ne,sw,sw")))
    (is (= 2 (part-1 "ne,ne,s,s")))
    (is (= 3 (part-1 "se,sw,se,sw,sw")))
    (is (= 4 (part-1 "sw,sw,nw,nw")))
    (is (= 5 (part-1 "sw,sw,nw,nw,n,n,n")))))

(deftest part-2-tests
  (testing "part 2 tests"
    (is (= 3 (part-2 "ne,ne,ne")))
    (is (= 2 (part-2 "ne,ne,sw,sw")))
    (is (= 2 (part-2 "ne,ne,s,s")))
    (is (= 3 (part-2 "se,sw,se,sw,sw")))
    (is (= 4 (part-2 "sw,sw,nw,nw")))
    (is (= 5 (part-2 "sw,sw,nw,nw,n,n,n")))))
