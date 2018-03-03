(ns advent-of-code.day-12  
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn parse-pipe [input]
  (let [[_ id com-ids] (re-matches #"^(.+) <-> (.+)$" input)
        com-ids-list (str/split com-ids #"[,\s]+")]
    [id com-ids-list]))

(defn get-visits [start-id pipes]
  (loop [to-visit [start-id] visited #{}]
    (if (empty? to-visit)
      visited
      (let [next-to-visit (remove visited (flatten (map pipes to-visit)))
            next-visited (into visited to-visit)]
        (recur next-to-visit next-visited)))))

(defn get-groups [pipes]
  (loop [[curr-pipe & rem-pipes] (keys pipes)
         groups []]
    (if (nil? curr-pipe)
      groups
      (let [visited (get-visits curr-pipe pipes)
            new-rem-pipes (remove visited rem-pipes)]
        (recur new-rem-pipes (conj groups visited))))))

(defn part-1 [input]
  (->> (str/split-lines input)
       (map parse-pipe)
       (into {})
       (get-visits "0")
       (count)))

(defn part-2 [input]
  (->> (str/split-lines input)
       (map parse-pipe)
       (into {})
       (get-groups)
       (count)))

(def testing-input
  "0 <-> 2
1 <-> 1
2 <-> 0, 3, 4
3 <-> 2, 4
4 <-> 2, 3, 6
5 <-> 6
6 <-> 4, 5")

(deftest part-1-tests
  (testing "Number of programs in the group of program 0"
    (is (= 6 (part-1 testing-input)))))

(deftest part-2-tests
  (testing "Total number of groups"
    (is (= 2 (part-2 testing-input)))))
