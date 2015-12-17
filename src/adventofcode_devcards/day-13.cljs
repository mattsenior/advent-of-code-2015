(ns adventofcode-devcards.day-13
  (:require
   [adventofcode-devcards.combinatorics :refer [permutations]])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input (slurp-input "13"))

(def test-input
  "Alice would gain 54 happiness units by sitting next to Bob.
Alice would lose 79 happiness units by sitting next to Carol.
Alice would lose 2 happiness units by sitting next to David.
Bob would gain 83 happiness units by sitting next to Alice.
Bob would lose 7 happiness units by sitting next to Carol.
Bob would lose 63 happiness units by sitting next to David.
Carol would lose 62 happiness units by sitting next to Alice.
Carol would gain 60 happiness units by sitting next to Bob.
Carol would gain 55 happiness units by sitting next to David.
David would gain 46 happiness units by sitting next to Alice.
David would lose 7 happiness units by sitting next to Bob.
David would gain 41 happiness units by sitting next to Carol.")

(defn add-opinion [opinions v]
  (let [[_ a lose-gain d b] (re-find #"(\w+) would (lose|gain) (\d+) happiness units by sitting next to (\w+)\." v)
        a (keyword a)
        b (keyword b)
        lose-gain (if (= lose-gain "gain") 1 -1)
        d (int d)]
    (assoc-in opinions [a b] (* lose-gain d))))

(defn get-arrangement-happiness [arrangement opinions]
  (->> (reduce (fn [{:keys [a happiness] :as acc} b]
                 (let [opinion-a (get-in opinions [a b])
                       opinion-b (get-in opinions [b a])]
                   (-> acc
                       (assoc :a b)
                       (update :happiness (partial + opinion-a opinion-b)))))
               {:a (first arrangement) :happiness 0}
               (rest arrangement))
       :happiness))

(defn arrangement-rf [comparator opinions current next-arrangement]
  (let [next-arrangement-happiness (get-arrangement-happiness next-arrangement opinions)]
    (if (or (nil? current)
            (comparator next-arrangement-happiness (:happiness current)))
      {:happiness next-arrangement-happiness
       :arrangement next-arrangement}
      current))) 

(defn lines->opinions [lines]
  (->> lines
       clojure.string/split-lines
       (reduce add-opinion {})))

(defn add-self [opinions]
  (let [people (keys opinions)]
    ;; Continue here
    opinions))

(defn get-winning-arrangement [comparator opinions]
  (let [nodes (keys opinions)
        arrangements (permutations nodes)]
    (->> arrangements
         (map (fn [x] (concat x [(first x)])))
         (reduce (partial arrangement-rf comparator opinions) nil))))

(def get-happiest-arrangement (partial get-winning-arrangement >))

(deftest part-1-tests
  (testing "Part 1"
    (is (= 330 (:happiness (get-happiest-arrangement (lines->opinions test-input)))))))

#_(defcard part-1-result (get-happiest-arrangement (lines->opinions input)))

(defcard part-2-result (get-happiest-arrangement (->> test-input lines->opinions add-self)))
