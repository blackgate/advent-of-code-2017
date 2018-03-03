(ns advent-of-code.day-16
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(def progs-start '[a b c d e f g h i j k l m n o p])

(defn spin [num-progs progs]
  (->> (drop-last num-progs progs)
       (into (vec (take-last num-progs progs)))))

(defn exchange [a b progs]
  (assoc progs a (nth progs b) b (nth progs a)))

(defn partner [a b progs]
  (mapv
   #(condp = %
      a b
      b a
      %)
   progs))

(def commands {"s" spin "x" exchange "p" partner})

(defn parse-move [move]
  (let [[_ cmd & args] (re-matches #"([sxp])(\w+)(?:/(\w+))?" move)
        parsed-args (map read-string (remove nil? args))]
    (apply partial (commands cmd) parsed-args)))

(defn parse-moves [input]
  (->> (str/split input #",")
       (map parse-move)))

(defn dance [progs moves]
  (reduce #(%2 %1) progs moves))

(defn part-1 [progs input]
  (let [moves (parse-moves input)]
    (str/join (dance progs moves))))

(defn get-reptition-pattern [progs moves]  
  (->> (repeat moves)
       (reductions #(dance %1 %2) progs)
       (drop 1)
       (take-while #(not= progs %))
       (into [progs])))

(defn part-2 [progs reps input]
  (let [moves (parse-moves input)
        pattern (get-reptition-pattern progs moves)]
    (->> (count pattern)
         (rem reps)
         (nth pattern)
         (str/join))))

(deftest part-1-tests
  (testing "In what order are the programs standing"
    (is (= "baedc" (part-1 '[a b c d e] "s1,x3/4,pe/b")))))

(deftest part-2-tests
  (testing "In what order are the programs standing"
    (is (= "ceadb" (part-2 '[a b c d e] 2 "s1,x3/4,pe/b")))))
