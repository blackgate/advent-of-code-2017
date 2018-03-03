(ns advent-of-code.day-10  
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn rot-reverse! [size start rev-len ^ints arr]
  (let [end (+ start rev-len -1)]
    (dotimes [p (/ rev-len 2)]
      (let [l (rem (+ start p) size)
            u (rem (- end p) size)
            l-val (aget arr l)
            u-val (aget arr u)]
        (aset-int arr l u-val)
        (aset-int arr u l-val)))))

(defn compute-knot-for [size lengths]
  (let [res (int-array (range size))]
    (loop [[l & rem-lens] lengths, skip 0, pos 0]
      (when-not (nil? l)
        (rot-reverse! size pos l res)
        (recur rem-lens (inc skip) (+ pos l skip))))
    res))

(defn xor-list [coll]
  (reduce bit-xor coll))

(defn knot-hash-bytes [input]
  (->> (into (mapv int input) [17 31 73 47 23])
       (repeat 64)
       (flatten)
       (compute-knot-for 256)
       (partition 16)
       (map xor-list)))

(defn knot-hash [input]
  (->> (knot-hash-bytes input)       
       (map #(format "%02x" %))
       (reduce str)))

(defn part-1 [size lengths]
  (let [[f s & _] (compute-knot-for size lengths)]
    (* f s)))

(defn part-2 [input]
  (knot-hash input))
  
(deftest part-1-tests
  (testing "result of multiplying the first two numbers in the list"
    (is (= 12 (part-1 5 [3 4 1 5])))))

(deftest part-2-tests
  (testing "knot hash of"
    (is (= "a2582a3a0e66e6e86e3812dcb672a272" (part-2 "")))
    (is (= "33efeb34ea91902bb2f59c9920caa6cd" (part-2 "AoC 2017")))
    (is (= "3efbe78a8d82f29979031a4aa0b16a9d" (part-2 "1,2,3")))
    (is (= "63960835bcdc130f0b66d7ff4f6a5a8e" (part-2 "1,2,4")))))
