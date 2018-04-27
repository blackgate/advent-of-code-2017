(ns advent-of-code.day-25
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn parse-command [line]
  (let [[_ cmd val] (re-matches #"^\s+- (\w+) .* (\w+).$" line)]
    [(read-string cmd) (read-string val)]))

(defn blueprint-partition [input]
  (->> (str/split-lines input)
       (partition-by empty?)
       (remove #(-> % first empty?))))

(defn parse-action [[[sel] commands]]
  (let [[_ val] (re-matches #"^\s*If the current value is (\d+):$" sel)]
    [(read-string val) (map parse-command commands)]))

(defn parse-actions [actions]
  (->> actions
       (partition-by #(re-matches #"^\s*If.*$" %))
       (partition 2)
       (map parse-action)
       (into {})))

(defn parse-state [script]
  (let [[_ s] (re-matches #"^In state (\w):$" (first script))]
    [(read-string s) (parse-actions (rest script))]))

(defn parse-setup [[begin-txt steps-txt]]
  (let [[_ state] (re-matches #"^Begin in state (\w+).$" begin-txt)
        [_ steps] (re-matches #"^Perform .* (\d+) steps." steps-txt)]
    {:initial-state (read-string state) :steps (read-string steps)}))

(defn parse-blueprint [input]
  (let [[setup & states] (blueprint-partition input)]
    {:setup (parse-setup setup)
     :states (into {} (map parse-state states))}))

(defmulti run-action (fn [_ [cmd _]] cmd))

(defmethod run-action 'Write [state [_ val]]
  (assoc-in state [:tape (:pos state)] val))

(defmethod run-action 'Continue [state [_ s]]
  (assoc state :state s))

(defmethod run-action 'Move [state [_ dir]]
  (let [pos-inc (get {'right 1 'left -1} dir)] 
    (update state :pos + pos-inc)))

(defn run-actions [{:keys [states]} state]
  (let [tape-val (get-in state [:tape (:pos state)] 0)
        actions (get-in states [(:state state) tape-val])]
    (reduce run-action state actions)))

(defn get-initial-state [blueprint]
  {:pos 0
   :tape {0 0}
   :state (get-in blueprint [:setup :initial-state])})

(defn part-1 [input]
  (let [blueprint (parse-blueprint input)
        state (get-initial-state blueprint)]
    (->> state
         (iterate #(run-actions blueprint %))
         (drop (-> blueprint :setup :steps))
         (first)
         (:tape)
         (vals)
         (reduce +))))

(def test-input
  "Begin in state A.
Perform a diagnostic checksum after 6 steps.

In state A:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state B.
  If the current value is 1:
    - Write the value 0.
    - Move one slot to the left.
    - Continue with state B.

In state B:
  If the current value is 0:
    - Write the value 1.
    - Move one slot to the left.
    - Continue with state A.
  If the current value is 1:
    - Write the value 1.
    - Move one slot to the right.
    - Continue with state A.")

(deftest part-1-tests
  (testing "What is the diagnostic checksum"
    (is (= 3 (part-1 test-input)))))
