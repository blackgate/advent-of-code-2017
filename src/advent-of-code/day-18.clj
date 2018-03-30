(ns advent-of-code.day-18
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(def start-state
  (->> (repeat 0)
       (map vector '[a b c d e f g h i j k l m n o p q r s t u v w x y z])
       (into {'in [] 'out [] 'pc 0})))

(defn reg-or-val [state b]
  (if (symbol? b) (state b) b))

(defn inc-pc [state]
  (update state 'pc inc))

(defmulti command (fn [_ [cmd]] cmd))

(defmethod command 'add [state [_ a b]]
  (inc-pc (update state a + (reg-or-val state b))))

(defmethod command 'mul [state [_ a b]]
  (inc-pc (update state a * (reg-or-val state b))))

(defmethod command 'mod [state [_ a b]]
  (inc-pc (update state a mod (reg-or-val state b))))

(defmethod command 'set [state [_ a b]]
  (inc-pc (assoc state a (reg-or-val state b))))

(defmethod command 'jgz [state [_ a b]]
  (if (> (reg-or-val state a) 0)
    (update state 'pc + (reg-or-val state b))
    (inc-pc state)))

(defmethod command 'snd [state [_ a]]
  (inc-pc (update state 'out conj (reg-or-val state a))))

(defmethod command 'rcv [state [_ a]]  
  (if-let [in (first (state 'in))]
    (-> state
        (assoc a in)
        (update 'in subvec 1)
        (inc-pc));)
    state))

(defn read-instructions [input]
  (->> (str/split-lines input)
       (map #(str "(" % ")"))
       (mapv read-string)))

(defn run-until-stuck [inst start-state]  
  (loop [state start-state]
    (let [cmd (nth inst (state 'pc))
          new-state (command state cmd)]
      (if (= state new-state)
        state
        (recur new-state)))))

(defn part-1 [input]
  (let [inst (read-instructions input)]
    (->> (run-until-stuck inst start-state)
         ('out)
         (last))))

(defn part-2 [input]
  (let [inst (read-instructions input)]
    (loop [state-1 start-state
           state-2 (assoc start-state 'p 1)
           sent-values 0]
      (let [new-state-1 (run-until-stuck inst state-1)
            new-state-2 (run-until-stuck inst state-2)]
        (if (and (= state-1 new-state-1) (= state-2 new-state-2))
          sent-values
          (recur (assoc new-state-1 'in ('out new-state-2) 'out [])
                 (assoc new-state-2 'in ('out new-state-1) 'out [])
                 (+ sent-values (count ('out new-state-2)))))))))

(def testing-input-1
  "set a 1
add a 2
mul a a
mod a 5
snd a
set a 0
rcv a
jgz a -1
set a 1
jgz a -2")

(def testing-input-2
  "snd 1
snd 2
snd p
rcv a
rcv b
rcv c
rcv d")

(deftest part-1-tests
  (testing "What is the value of the recovered frequency"
    (is (= 4 (part-1 testing-input-1)))))

(deftest part-2-tests
  (testing "How many times did program 1 send a value"
    (is (= 3 (part-2 testing-input-2)))))
