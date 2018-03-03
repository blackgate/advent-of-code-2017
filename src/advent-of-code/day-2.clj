(ns advent-of-code.day-2
  (:require [clojure.test :refer :all]))

(defn split-cols [line]
  (map read-string (clojure.string/split line #"\s")))

(defn row-min [row]
  (reduce min row))

(defn row-max [row]
  (reduce max row))

(defn row-checksum [row]
  (- (row-max row) (row-min row)))

(defn even-div? [a b]
  (zero? (rem a b)))

(defn find-even-div [n arr]
  (first (filter #(even-div? % n) arr)))

(defn row-checksum-2 [row]
  (let [sorted-row (sort row)]
    (loop [[n & r] sorted-row]
      (if-let [div (find-even-div n r)]
        (/ div n)
        (recur r)))))

(defn part-1 [input]
  (->> (clojure.string/split-lines input)
       (map #(row-checksum (split-cols %)))
       (reduce +)))

(defn part-2 [input]
  (->> (clojure.string/split-lines input)
       (map #(row-checksum-2 (split-cols %)))
       (reduce +)))

(def test-input-1
  "5 1 9 5
7 5 3
2 4 6 8");

(def test-input-2
  "5 9 2 8
9 4 7 3
3 8 6 5");

(deftest part-1-tests
  (testing "What is the checksum"
    (is (= (part-1 test-input-1) 18))))

(deftest part-2-tests
  (testing "sum of each row's result"
    (is (= (part-2 test-input-2) 9))))
