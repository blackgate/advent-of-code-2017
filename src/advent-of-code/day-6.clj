(ns advent-of-code.day-6
  (:require [clojure.test :refer :all]))

(defn find-max-indexed [coll]
  (->> (map-indexed #(hash-map :index %1 :value %2) coll)
       (reduce #(if (> (:value %2) (:value %1)) %2 %1))))

(defn inc-steps [index value len]
  (->> (range (inc index) (+ index value 1))
       (map #(rem % len))))

(defn distrib-mem [mem]
  (let [{:keys [index value]} (find-max-indexed mem)]
    (->> (inc-steps index value (count mem))
         (reduce #(update %1 %2 inc) (assoc mem index 0)))))

(defn part-1 [input]
  (loop [mem input, steps 0, last-runs #{}]
    (if (contains? last-runs mem)
      steps
      (recur (distrib-mem mem) (inc steps) (conj last-runs mem)))))

(defn part-2 [input]
  (loop [mem input, steps 0, last-runs {}]
    (if (contains? last-runs mem)
      (- steps (last-runs mem))
      (recur (distrib-mem mem) (inc steps) (assoc last-runs mem steps)))))

(deftest part-1-tests
  (testing "should return the steps until it repeats"
    (is (= (part-1 [0 2 7 0]) 5))))

(deftest part-2-tests
  (testing "should return the steps since last repetition"
    (is (= (part-2 [0 2 7 0]) 4))))
