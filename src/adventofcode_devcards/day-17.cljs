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

(defn get-remaining [target exclude-k nodes]
  (let [xf (comp (drop-while (fn [[k _]] (not (= k exclude-k))))
                 (drop 1)
                 (filter (fn [[_ v]] (<= v target))))]
    (sequence xf nodes)))

(defn combos [target path nodes]
  (letfn [(sub-combos [[k v]]
            (let [new-target (- target v)
                  path (conj path k)
                  remaining (get-remaining new-target k nodes)]
              (if (empty? remaining)
                ;; There are no remaining choices, so we are on an end node.
                ;; If it exactly matches the remaining target, return the path
                ;; for adding to our list, else nil
                (when (= target v) [path])
                ;; There are still remaining choices so delve into them with
                ;; our new target
                (combos new-target path remaining))))]
    (mapcat sub-combos nodes)))

(defn get-combinations [target containers]
  (let [nodes (map vector (range) containers)]
    (combos target [] nodes)))

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
