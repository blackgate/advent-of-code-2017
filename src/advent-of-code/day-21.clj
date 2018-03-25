(ns advent-of-code.day-21
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn parse-square-line [line]
  (mapv {\# 1 \. 0} line))

(defn parse-square [square]
  (->> (str/split square #"/")
       (mapv parse-square-line)))

(def start-square
  (parse-square ".#./..#/###"))

(defn transpose [square]
  (apply mapv vector square))

(defn flip-square [square]
  (->> square reverse vec))

(defn rotate-square [square]
  (->> square transpose flip-square))

(defn square-rotations [square]
  (take 4 (iterate rotate-square square)))

(defn sub-square [square [start-x start-y] size]
  (mapv
   (fn [x]
     (mapv #(get-in square [x %]) (range start-y (+ start-y size))))
   (range start-x (+ start-x size))))

(defn apply-rules [rules square]
  (let [size (count square)
        sub-size (if (zero? (rem size 2)) 2 3)]
    (->> (for [x (range 0 size sub-size)]
           (for [y (range 0 size sub-size)]
             (get rules (sub-square square [x y] sub-size))))
         (mapv #(apply mapv (comp vec concat)))
         (reduce into []))))

(defn square-variations [square]
  (->> (square-rotations square)
       (mapcat #(vector % (flip-square %)))))
       
(defn add-square-variations [rules [k v]]
  (->> (square-variations k)
       (reduce #(if (get %1 %2) %1 (assoc %1 %2 v)) rules)))
    
(defn add-variations [rules]
  (reduce add-square-variations rules rules))

(defn parse-rule [rule]
  (let [[_ r t] (re-matches #"(.*) => (.*)" rule)]
    [(parse-square r) (parse-square t)]))
    
(defn parse-rules [rules]
  (->> (str/split-lines rules)
       (map parse-rule)
       (into {})
       (add-variations)))

(defn part-1 [iterations rules-str]
  (let [rules (parse-rules rules-str)]
    (->> (iterate (partial apply-rules rules) start-square)
         (drop iterations)
         (first)
         (flatten)
         (reduce +))))

(def test-rules
  "../.# => ##./#../...
.#./..#/### => #..#/..../..../#..#");

(deftest part-1-tests
  (testing "How many pixels stay on after 2 iterations?"
    (is (= 12 (part-1 2 test-rules)))))

;; No changes nedded for part2
