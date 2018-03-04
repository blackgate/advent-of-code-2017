(ns advent-of-code.day-19
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn next-pos [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn collectable? [ch]
  (and (<= (int \A) (int ch) (int \Z))))

(defn outside-bounds? [ch]
  (or (nil? ch) (= ch \space)))

(defn change-dir [dir pos lines]
  (let [new-dir (vec (reverse dir))
        ch (get-in lines (next-pos pos new-dir))]
    (if (outside-bounds? ch)
      (mapv - new-dir)
      new-dir)))

(defn traverse-route [input]
  (let [[first-line :as lines] (str/split-lines input)]
    (loop [dir [1 0]
           pos [0 (str/index-of first-line \|)]
           route []]
      (let [ch (get-in lines pos)]
        (if (outside-bounds? ch)
          route
          (let [new-dir (if (= ch \+) (change-dir dir pos lines) dir)]
            (recur new-dir (next-pos pos new-dir) (conj route ch))))))))

(defn part-1 [input]
  (->> (traverse-route input)
       (filter collectable?)
       (apply str)))

(defn part-2 [input]
  (->> (traverse-route input)
       (count)))

(def test-input
  (str/join
   \newline
   ["     |          "
    "     |  +--+    "
    "     A  |  C    "
    " F---|----E|--+ " 
    "     |  |  |  D "
    "     +B-+  +--+ "]))

(deftest part-1-tests
  (testing "What letters will it see"
    (is (= "ABCDEF" (part-1 test-input)))))

(deftest part-2-tests
  (testing "How many steps does the packet need to go?"
    (is (= 38 (part-2 test-input)))))
