(ns advent-of-code.day-8
  (:require [clojure.test :refer :all]))

(def operations
  {"inc" +
   "dec" -
   ">" >
   "<" <
   ">=" >=
   "<=" <=
   "==" =
   "!=" not=})

(defn parse-line [line]
  (let [parse-re #"^(\w+) (\w+) (-?\d+) if (\w+) (>|>=|<|<=|==|!=) (-?\d+)$"
        tokens (re-matches parse-re line) 
        [_ act-reg act-op act-val cond-reg cond-op cond-val] tokens]
    {:action {:op act-op :reg act-reg :val (Integer/parseInt act-val)}
     :condition {:op cond-op :reg cond-reg :val (Integer/parseInt cond-val)}}))

(defn run-action [env action]
  (let [op (operations (:op action))
        reg (:reg action)
        reg-val (or (get-in env [:regs reg]) 0)
        new-val (op reg-val (:val action))
        new-max (max new-val (:max env))
        new-regs (assoc (:regs env) reg new-val)]
    (assoc env :regs new-regs :max new-max)))

(defn run-instruction [env {:keys [action condition]}]
  (let [cond-op (operations (:op condition))
        cond-reg (or (get-in env [:regs (:reg condition)]) 0)]
    (if (cond-op cond-reg (:val condition))
      (run-action env action)
      env)))
      
(defn part-1 [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (reduce run-instruction {:regs {} :max 0})
       (:regs)
       (vals)
       (reduce max)))

(defn part-2 [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (reduce run-instruction {:regs {} :max 0})
       (:max)))

(def testing-input
  "b inc 5 if a > 1
a inc 1 if b < 5
c dec -10 if a >= 1
c inc -20 if c == 10")

(deftest part-1-tests
  (testing "largest value in any register"
    (is (= (part-1 testing-input) 1))))

(deftest part-2-tests
  (testing "highest value held in any register during the process"
    (is (= (part-2 testing-input) 10))))
