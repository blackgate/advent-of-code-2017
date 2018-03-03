(ns advent-of-code.day-3
  (:require [clojure.test :refer :all]))

(defn next-pos [[x y] [dx dy]]
  [(+ x dx) (+ y dy)])

(defn flip-dir [[dx dy]]
  (if (zero? dx)
    [(- dy) 0]
    [0 dx]))

(defn spiral-init []
  {:pos [0 0]
   :seg-len 1
   :rem-len 1
   :dir [1 0]
   :data {[0 0] 1}})

(defn spiral-get [{:keys [data pos]}]
  (data pos))

(defn spiral-set [spiral value]
  (let [pos (:pos spiral)]
    (assoc-in spiral [:data pos] value)))

(defn spiral-change-dir [{:keys [dir seg-len] :as spiral}]
  (let [[dx _] dir
        len (if (zero? dx) (inc seg-len) seg-len)]
    (assoc spiral :dir (flip-dir dir) :seg-len len :rem-len len)))

(defn spiral-move-next [{:keys [rem-len pos dir] :as spiral}]
  (if (zero? rem-len)
    (recur (spiral-change-dir spiral))
    (assoc spiral :pos (next-pos pos dir) :rem-len (dec rem-len))))

;; PART 1

(defn spiral-manhattan-dist [spiral]
  (let [[x y] (:pos spiral)]
    (+ (Math/abs x) (Math/abs y))))

(defn spiral-next-part-1 [spiral]
  (let [n (spiral-get spiral)
        s (spiral-move-next spiral)]
    (spiral-set s (inc n))))

(defn part-1 [num]
  (loop [s (spiral-init)]
    (let [n (spiral-get s)]
      (if (< n num)
        (recur (spiral-next-part-1 s))
        (spiral-manhattan-dist s)))))

;; PART 2

(defn spiral-next-num-part-2 [{:keys [data pos]}]
  (let [[x y] pos]
    (+ (or (data [x (dec y)]) 0)
       (or (data [x (inc y)]) 0)
       (or (data [(dec x) y]) 0)
       (or (data [(dec x) (dec y)]) 0)
       (or (data [(dec x) (inc y)]) 0)
       (or (data [(inc x) y]) 0)
       (or (data [(inc x) (dec y)]) 0)
       (or (data [(inc x) (inc y)]) 0))))

(defn spiral-next-part-2 [spiral]
  (let [s (spiral-move-next spiral)]
    (spiral-set s (spiral-next-num-part-2 s))))

(defn part-2 [num]
  (loop [s (spiral-init)]
    (let [n (spiral-get s)]
      (if (<= n num)
        (recur (spiral-next-part-2 s))
        n))))

(deftest part-1-tests
  (testing "How many steps"
    (is (= (part-1 1) 0))
    (is (= (part-1 12) 3))
    (is (= (part-1 23) 2))
    (is (= (part-1 1024) 31))))

(deftest part-2-tests
  (testing "First value written that is larger than"
    (is (= (part-2 26) 54))
    (is (= (part-2 133) 142))
    (is (= (part-2 362) 747))))
