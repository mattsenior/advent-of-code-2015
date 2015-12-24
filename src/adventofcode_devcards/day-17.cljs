(ns adventofcode-devcards.day-17
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input (slurp-input "17"))

(def test-input
  "20
15
10
5
5")

(defn lines->containers [lines]
  (->> lines clojure.string/split-lines (map int)))

(defn a [target path nodes]
  (mapcat (fn [[k v]]
         (let [new-target (- target v)
               path (conj path k)
               remaining (->> nodes
                              (drop-while (fn [[k2 _]] (not (= k2 k))))
                              (drop 1)
                              (filter (fn [[_ v2]] (<= v2 new-target))))]
           (if (empty? remaining)
             (when (= target v) [path])
             (a new-target path remaining)))) nodes))

(defn get-combinations [target containers]
  (let [nodes (map vector (range) containers)]
    (into [] (a target [] nodes))))

(defn count-min [combinations]
  (let [counts (map count combinations)
        min (apply min counts)]
    (get (frequencies counts) min)))

(deftest part-1
  (testing "Part 1"
    (is (= 4 (count (get-combinations 25 (lines->containers test-input)))))))

(deftest part-2
  (testing "Part 2"
    (is (= 3 (count-min (get-combinations 25 (lines->containers test-input)))))))

(defcard part-1-result (count (get-combinations 150 (lines->containers input))))
(defcard part-2-result (count-min (get-combinations 150 (lines->containers input)))) 
