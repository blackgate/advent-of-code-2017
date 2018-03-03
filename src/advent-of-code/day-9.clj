(ns advent-of-code.day-9
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(def garbage-match #"<(?:!.|[^!>])*>")

(defn part-1
  ([input score level]
   (if (empty? input)
     score
     (let [[first-ch & rem-chars] input]
       (case first-ch
         \{ (recur rem-chars score (inc level))
         \} (recur rem-chars (+ score level) (dec level))
         (recur rem-chars score level)))))
  ([input]
   (let [clean-input (str/replace input garbage-match "")]
     (part-1 clean-input 0 0))))

(defn part-2 [input]
  (->> (re-seq garbage-match input)
       (map #(str/replace % #"!." ""))
       (reduce #(+ %1 (count %2) -2) 0)))
       
(deftest part-1-tests
  (testing "total score"
    (is (= (part-1 "{}") 1))
    (is (= (part-1 "{{{}}}") 6))
    (is (= (part-1 "{{},{}}") 5))
    (is (= (part-1 "{{{},{},{{}}}}") 16))
    (is (= (part-1 "{<a>,<a>,<a>,<a>}") 1))
    (is (= (part-1 "{{<ab>},{<ab>},{<ab>},{<ab>}}") 9))
    (is (= (part-1 "{{<!!>},{<!!>},{<!!>},{<!!>}}") 9))
    (is (= (part-1 "{{<a!>},{<a!>},{<a!>},{<ab>}}") 3))))

(deftest part-2-tests
  (testing "removed non-ignored chars"
    (is (= (part-2 "<>") 0))
    (is (= (part-2 "<random characters>") 17))
    (is (= (part-2 "<<<<>") 3))
    (is (= (part-2 "<{!>}>") 2))
    (is (= (part-2 "<!!>") 0))
    (is (= (part-2 "<!!!>>") 0))
    (is (= (part-2 "<{o\"i!a,<{i<a>") 10))))
