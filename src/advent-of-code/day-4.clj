(ns advent-of-code.day-4
  (:require [clojure.test :refer :all]))

(defn words-of [phrase]
  (re-seq #"\w+" phrase))

(defn has-repeated-elems? [coll]
  (->> (frequencies coll)
       (some (fn [[_ c]] (> c 1)))))

(defn has-repeated-words? [phrase]
  (->> (words-of phrase)
       (has-repeated-elems?)))

(defn part-1 [phrases]
  (->> (clojure.string/split-lines phrases)
       (remove has-repeated-words?)
       (count)))

(defn has-anagram-words? [phrase]
  (->> (words-of phrase)
       (map sort)
       (has-repeated-elems?)))

(defn part-2 [phrases]
  (->> (clojure.string/split-lines phrases)
       (remove has-anagram-words?)
       (count)))

(def test-input-1
  "aa bb cc dd ee
aa bb cc dd aa
aa bb cc dd aaa")

(def test-input-2
  "abcde fghij
abcde xyz ecdab
a ab abc abd abf abj
iiii oiii ooii oooi oooo
oiii ioii iioi iiio")

(deftest part-1-tests
  (testing "How many passphrases are valid?"
    (is (= (part-1 test-input-1) 2))))

(deftest part-2-tests
  (testing "How many passphrases are valid?"
    (is (= (part-2 test-input-2) 3))))

