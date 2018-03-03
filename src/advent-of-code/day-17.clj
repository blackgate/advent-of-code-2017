(ns advent-of-code.day-17
  (:require [clojure.test :refer :all]))

(defn insert-at [p value coll]
  (apply conj (subvec coll 0 p) value (subvec coll p)))

(defn circular-buffer [steps max-val]
  (reduce
   (fn [[pos res] i]
     (let [p (inc (rem (+ pos steps) i))]
       [p (insert-at p i res)]))
   [0 [0]]
   (range 1 (+ max-val 1))))

(defn part-1 [steps]
  (let [[p buffer] (circular-buffer steps 2017)]
    (nth buffer (inc p))))

(defn part-2 [steps max-value]
  (loop [pos 0, n 1, after-zero nil]
    (if (> n max-value)
      after-zero
      (let [p (inc (rem (+ pos steps) n))]
        (recur p (inc n) (if (= p 1) n after-zero))))))

(deftest part-1-tests
  (testing "What is the value after 2017"
    (is (= 638 (part-1 3)))))

(deftest part-2-tests
  (testing "What is the value after 0"
    (is (= 1226 (part-2 3 2017)))))
