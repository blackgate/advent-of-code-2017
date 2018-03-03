(ns advent-of-code.day-7
  (:require [clojure.test :refer :all]))

(defn parse-line [line]
  (let [re #"^(\w+) \((\d+)\)(?: -> (.+))?$"
        [_ name weight subs] (re-matches re line)]
    {:name name
     :weight (Integer/parseInt weight)
     :subs (if (nil? subs) [] (clojure.string/split subs #",\s*"))}))

(defn index-by [prop coll]
  (into {} (map (juxt prop identity) coll)))

(defn tower-bottom-candidates [nodes]
  (->> (filter #(> (count (:subs %)) 0) nodes)
       (sort-by :weight)))

(defn get-weight-sum [subs]
  (reduce + (map :weight-sum subs)))

(defn get-tower-tree [{:keys [subs] :as node} indexed-nodes]
  (let [s (map #(get-tower-tree (indexed-nodes %) indexed-nodes) subs)
        weight-sum (+ (:weight node) (get-weight-sum s))
        visited (inc (reduce + (map :visited s)))]
    (assoc node :weight-sum weight-sum :visited visited :subs s)))

(defn is-tower-valid [tower indexed-nodes]
  (let [visited (:visited tower)]
    (= (count indexed-nodes) visited)))

(defn find-tower [nodes]
  (let [candidates (tower-bottom-candidates nodes)
        indexed-nodes (index-by :name nodes)]
    (loop [[candidate & rem-candidates] candidates]
      (let [tower (get-tower-tree candidate indexed-nodes)]
        (if (is-tower-valid tower indexed-nodes)
          tower
          (recur rem-candidates))))))

(defn part-1 [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (find-tower)
       (:name)))

(defn find-unbalanced-weight
  ([node]
   (find-unbalanced-weight node nil))
  ([node valid]
   (let [grps (group-by :weight-sum (:subs node))
         [_ v] (first (filter (fn [[k v]] (> (count v) 1)) grps))
         [_ n] (first (filter (fn [[k v]] (= (count v) 1)) grps))]
     (if (nil? n)
       (+ (:weight node) (- (:weight-sum valid) (:weight-sum node)))
       (recur (first n) (first v))))))
    
(defn part-2 [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (find-tower)
       (find-unbalanced-weight)))

(def testing-input
  "pbga (66)
xhth (57)
ebii (61)
havc (66)
ktlj (57)
fwft (72) -> ktlj, cntj, xhth
qoyq (66)
padx (45) -> pbga, havc, qoyq
tknk (41) -> ugml, padx, fwft
jptl (61)
ugml (68) -> gyxo, ebii, jptl
gyxo (61)
cntj (57)")

(deftest part-1-tests
  (testing "name of the bottom program"
    (is (= (part-1 testing-input) "tknk"))))

(deftest part-2-tests
  (testing "corrent wrong weight"
    (is (= (part-2 testing-input) 60))))
