(ns adventofcode-devcards.day-16
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def sues (slurp-input "16"))

(def test-results
  {:children 3
   :cats 7
   :samoyeds 2
   :pomeranians 3
   :akitas 0
   :vizslas 0
   :goldfish 5
   :trees 3
   :cars 2
   :perfumes 1})

(defn line->sue [line]
  (let [[_ sue metrics] (re-matches #"^Sue (\d+): (.*)$" line)
        sue (int sue)
        metrics (clojure.string/split metrics ", ")
        metrics (mapcat (fn [x]
                          (let [[k v] (clojure.string/split x ": ")]
                            [(keyword k) (int v)])) metrics)]
    [sue (apply hash-map metrics)]))

(defn part-1-matcher [k v]
  (= v (k test-results)))

(defn part-2-matcher [k v]
  (condp = k
    ;; Cats and trees results indicate the actual value is GT the test result
    :cats (> v (:cats test-results))
    :trees (> v (:trees test-results))

    ;; Pomeranians and goldfish results indicate the actual value is LT the test result
    :pomeranians (< v (:pomeranians test-results))
    :goldfish (< v (:goldfish test-results))

    ;; Everything else must be equal
    (= v (k test-results))))

(defn find-sue [result-matches?]
  (let [sues (clojure.string/split-lines sues)
        xf (comp (map line->sue)
                 (filter (fn [[sue metrics]]
                           (let [a (map (fn [[k v]] (result-matches? k v)) metrics)]
                             (every? identity a)))))]
    (ffirst (sequence xf sues))))

(defcard results-1 (find-sue part-1-matcher))
(defcard results-2 (find-sue part-2-matcher))
