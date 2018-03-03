(ns advent-of-code.day-5
  (:require [clojure.test :refer :all]))

(defn parse-input [input]
  (->> (clojure.string/split-lines input)
       (map #(Integer/parseInt %))
       (into [])))

(defn run-with-incrementor [input inc-fn]
  (loop [offsets (parse-input input), pos 0, steps 0]
    (if (>= pos (count offsets))
      steps
      (let [offset (offsets pos)
            new-offsets (update offsets pos inc-fn)
            new-pos (+ pos offset)]
        (recur new-offsets new-pos (inc steps))))))

(defn part-1 [input]
  (run-with-incrementor input inc))

(defn part-2 [input]
  (run-with-incrementor input #(if (>= % 3) (dec %) (inc %))))

(deftest part-1-tests
  (testing "steps until exit reached"
    (is (= (part-1 "0\n3\n0\n1\n-3") 5))))

(deftest part-2-tests
  (testing "steps until exit reached"
    (is (= (part-2 "0\n3\n0\n1\n-3") 10))))
