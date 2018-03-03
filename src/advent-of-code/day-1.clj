(ns advent-of-code.day-1
  (:require [clojure.test :refer :all]))

(defn digit-to-int [digit]
  (- (int digit) (int \0)))

(defn to-digits-vector [code]
  (->> code
       (map digit-to-int)
       (into [])))

(defn part-1 [code]
  (loop [acc 0
         [digit & r] (to-digits-vector (str code (first code)))]
    (if (empty? r)
      acc
      (recur (if (= digit (first r)) (+ acc digit) acc) r))))

(defn part-2 [code]
  (let [size (count code)
        digits (to-digits-vector code)]
    (loop [acc 0 i 0 j (/ size 2)]
      (if (= i size)
        acc
        (let [a (nth digits i)
              b (nth digits (rem j size))]
          (recur (if (= a b) (+ acc a) acc)
                 (inc i)
                 (inc j)))))))

(deftest part-1-tests
  (testing "What is the solution"
    (is (= (part-1 "1122") 3))
    (is (= (part-1 "1111") 4))
    (is (= (part-1 "1234") 0))
    (is (= (part-1 "91212129") 9))))

(deftest part-2-tests
  (testing "What is the solution"
    (is (= (part-2 "1212") 6))
    (is (= (part-2 "1221") 0))
    (is (= (part-2 "123425") 4))
    (is (= (part-2 "123123") 12))
    (is (= (part-2 "12131415") 4))))
