(ns advent-of-code.day-22
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn parse-line [x line]
  (let [y-start (- (quot (count line) 2))]
    (keep-indexed #(when (= %2 \#) [x (+ y-start %1)]) line)))

(defn initial-state [infected]
  {:dir [1 0]
   :pos [0 0]
   :infected-count 0
   :status (zipmap infected (repeat :infected))})

(defn parse-input [input]
  (let [lines (str/split-lines input)
        x-start (quot (count lines) 2)]
    (->> lines
         (map-indexed #(parse-line (- x-start %1) %2))
         (apply concat)
         (initial-state))))

(defn turn-right [[x y]]
  [(- y) x])

(defn turn-left [[x y]]
  [y (- x)])

(defn reverse-direction [[x y]]
  [(- x) (- y)])

(defn change-direction [{:keys [status pos] :as state}]
  (update state :dir (case (get status pos)
                       :infected turn-right
                       :weakened identity
                       :flagged reverse-direction
                       turn-left)))

(defn move [{[x y] :pos, [dx dy] :dir, :as state}]
  (assoc state :pos [(+ x dx) (+ y dy)]))

(defn infect [state pos]
  (-> state
      (update :status assoc pos :infected)
      (update :infected-count inc)))

(defn update-status-1 [{:keys [status pos] :as state}]
  (if (= :infected (get status pos))
    (update state :status dissoc pos)
    (infect state pos)))

(defn burst-1 [state]
  (-> state
      (change-direction)
      (update-status-1)
      (move)))

(defn part-1 [bursts input]
  (->> (parse-input input)
       (iterate burst-1)
       (drop bursts)
       (first)
       :infected-count))

(defn update-status-2 [{:keys [status pos] :as state}]
  (case (get status pos)
    :infected (assoc-in state [:status pos] :flagged)
    :flagged (update state :status dissoc pos)
    :weakened (infect state pos)
    (assoc-in state [:status pos] :weakened)))

(defn burst-2 [state]
  (-> state
      (change-direction)
      (update-status-2)
      (move)))

(defn part-2 [bursts input]
  (->> (parse-input input)
       (iterate burst-2)
       (drop bursts)
       (first)
       :infected-count))

(def test-input
  "..#
#..
...")

(deftest part-1-tests
  (testing "how many bursts cause a node to become infected"
    (is (= 5 (part-1 7 test-input)))
    (is (= 41 (part-1 70 test-input)))))

(deftest part-2-tests
  (testing "how many bursts cause a node to become infected"
    (is (= 26 (part-2 100 test-input)))))
