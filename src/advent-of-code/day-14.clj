(ns advent-of-code.day-14
  (:require [clojure.test :refer :all]
            [advent-of-code.day-10 :as day-10]))

(defn to-bits [val]
  (->> (range 8)
       (map #(if (bit-test val (- 7 %)) 1 0))))

(defn line-bits [line]
  (->> (day-10/knot-hash-bytes line)
       (map to-bits)
       (flatten)
       (into [])))

(defn build-grid [input]
  (mapv #(line-bits (str input "-" %)) (range 128)))

(defn neighbours-of [[x y]]
  [[(- x 1) y]
   [(+ x 1) y]
   [x (- y 1)]
   [x (+ y 1)]])

(defn remove-group [matrix pos]
  (if (= 1 (get-in matrix pos))
    (reduce remove-group
            (assoc-in matrix pos 0)
            (neighbours-of pos))
    matrix))

(defn matrix-positions-for [size]
  (for [row (range size)
        col (range size)]
    [row col]))

(defn count-groups [size matrix]
  (->> (matrix-positions-for size)
       (reduce
        (fn [[cnt m] pos]
          (if (= 1 (get-in m pos))
            [(inc cnt) (remove-group m pos)]
            [cnt m]))
        [0 matrix])
       (first)))

(defn part-1 [input]
  (->> (build-grid input)
       (flatten)
       (reduce +)))

(defn part-2 [input]
  (->> (build-grid input)
       (count-groups 128)))

(deftest part-1-tests
  (testing "how many squares are used"
    (is (= 8108 (part-1 "flqrgnkx")))))

(deftest part-2-tests
  (testing "number of regions"
    (is (= 1242 (part-2 "flqrgnkx")))))
