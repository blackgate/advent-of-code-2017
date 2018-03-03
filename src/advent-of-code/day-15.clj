(ns advent-of-code.day-15
  (:require [clojure.test :refer :all]))

(defn gen-with-factor [factor]
  (fn [value] (rem (* value factor) 2147483647)))

(defn a-gen [start]
  (iterate (gen-with-factor 16807) start))

(defn b-gen [start]
  (iterate (gen-with-factor 48271) start))

(defn count-lower-16bits-matches [a-vals b-vals samples]
  (->> (map =
            (map #(bit-and 0xFFFF %) a-vals)
            (map #(bit-and 0xFFFF %) b-vals))
       (take samples)
       (filter identity)
       (count)))

(defn part-1 [a-start b-start]
  (count-lower-16bits-matches
   (a-gen a-start)
   (b-gen b-start)
   40000000))

(defn part-2 [a-start b-start]
  (count-lower-16bits-matches
   (filter #(zero? (rem % 4)) (a-gen a-start))
   (filter #(zero? (rem % 8)) (b-gen b-start))
   5000000))

(deftest part-1-tests
  (testing "what is the judge's final count"
    (is (= 588 (part-1 65 8921)))))

(deftest part-2-tests
  (testing "what is the judge's final count"
    (is (= 309 (part-2 65 8921)))))
