(ns advent-of-code.day-13
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn parse-layer [input]
  (let [[_ layer depth] (re-matches #"\s*(\d+):\s*(\d+)" input)]
    {:index (Integer/parseInt layer)
     :depth (Integer/parseInt depth)}))
  
(defn get-layers [input]
  (->> (str/split-lines input)
       (map parse-layer)))

(defn layer-pos [layer delay]
  (let [total-delay (+ delay (:index layer))]
    (rem total-delay (* (- (:depth layer) 1) 2))))

(defn caught?
  ([layer delay]
   (zero? (layer-pos layer delay)))
  ([layer]
   (caught? layer 0)))

(defn severity [{:keys [index depth] :as layer}]
  (* index depth))

(defn part-1 [input]
  (->> (get-layers input)
       (filter caught?)
       (map severity)
       (reduce +)))

(defn can-cross? [layers delay]
  (not (some #(caught? % delay) layers)))

(defn part-2 [input]
  (let [layers (get-layers input)]
    (first (filter #(can-cross? layers %) (range)))))

(def testing-input
  "0: 3
  1: 2
  4: 4
  6: 4")

(deftest part-1-tests
  (testing "severity of the whole trip"
    (is (= 24 (part-1 testing-input)))))

(deftest part-2-tests
  (testing "fewest number of picoseconds to travel without being caught"
    (is (= 10 (part-2 testing-input)))))
