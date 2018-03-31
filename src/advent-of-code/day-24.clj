(ns advent-of-code.day-24
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn add-component [bridge [port-a port-b]]
  (if (= port-a (last bridge))
    (conj bridge port-a port-b)
    (conj bridge port-b port-a)))

(defn find-valid-components [comps port]
  (filter (fn [[a b]] (or (= a port) (= b port))) comps))

(defn bridges-of
  ([comps bridge]   
   (let [last-port (last bridge)
         next-comps (find-valid-components comps last-port)]
     (if (empty? next-comps)
       [{:strength (reduce + bridge)
         :length (count bridge)}]
       (mapcat
        #(bridges-of (disj comps %) (add-component bridge %))
        next-comps))))
  ([comps]
   (let [first-comps (filter #(zero? (first %)) comps)]
     (mapcat #(bridges-of (disj comps %) %) first-comps))))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map #(mapv read-string (str/split % #"/")))
       (set)))

(defn part-1 [input]
  (->> (parse-input input)
       (bridges-of)
       (reduce #(max %1 (:strength %2)) 0)))

(defn compare-bridge-part2 [a b]
  (cond
    (> (:length a) (:length b)) a
    (< (:length a) (:length b)) b
    (> (:strength a) (:strength b)) a
    :else b))
    
(defn part-2 [input]
  (->> (parse-input input)
       (bridges-of)
       (reduce compare-bridge-part2)
       (:strength)))

(def test-input
  "0/2
2/2
2/3
3/4
3/5
0/1
10/1
9/10")

(deftest part-1-tests
  (testing "What is the strength of the strongest bridge you can make"
    (is (= 31 (part-1 test-input)))))

(deftest part-2-tests
  (testing "What is the strength of the longest bridge"
    (is (= 19 (part-2 test-input)))))
