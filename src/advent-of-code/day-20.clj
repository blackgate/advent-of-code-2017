(ns advent-of-code.day-20
  (:require [clojure.test :refer :all]
            [clojure.string :as str]))

(defn get-readable-lines [input]
  (-> (str/replace input #"(\w+)=<([^>]+)>" ":$1 [$2]")
      (str/split-lines)))

(defn parse-input [input]
  (->> (get-readable-lines input)
       (map #(str "{" % "}"))
       (map read-string)
       (map-indexed #(assoc %2 :index %1))))

(defn dist-to-origin [particle]
  (reduce + (map #(Math/abs %) (:p particle))))

;; The velocity at any given time is: v + ta
;; Given that, the accumulation of the velocity
;; over time can be expressed as:
;; (v + 1a) + (v + 2a) + ... + (v + ta)
;; <=> tv + (1 + 2 + ... + t)a
;; The sum of positive integers from 1 to t is
;; given by t(t + 1) / 2, so:
;; tv + (t(t + 1) / 2)a
;; <=> tv + at(t + 1) / 2
(defn pos-at-time [p v a t]
  (+ p (* v t) (* 0.5 a t (inc t))))

(defn particle-at-time [{:keys [p v a] :as particle} t]
  (assoc
   particle
   :p (mapv pos-at-time p v a (repeat t))
   :v (mapv #(+ %1 (* %2 t)) v a)))

(defn part-1 [input]
  (->> (parse-input input)
       (map #(particle-at-time % 1000))
       (sort-by dist-to-origin)
       (first)
       (:index)))

(defn rem-next-particles [particles]
  (->> (map #(particle-at-time % 1) particles)
       (group-by :p)
       (filter #(= (count (second %)) 1))
       (mapcat second)))

(defn part-2 [input]
  (->> (parse-input input)
       (iterate rem-next-particles)
       (drop 100)
       (first)
       (count)))

(def test-input-1
  "p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>
p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")

(def test-input-2
  "p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>
p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>
p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>
p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>")

(deftest part-1-tests
  (testing "Which particle will stay closest to position <0,0,0>"
    (is (= 0 (part-1 test-input-1)))))

(deftest part-2-tests
  (testing "Particles are left after all collisions are resolved?"
    (is (= 1 (part-2 test-input-2)))))
