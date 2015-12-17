(ns adventofcode-devcards.day-12
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input (slurp-input "12"))

(defn sum-numbers [x]
  (->> x (re-seq #"-?\d+") (map int) (reduce + 0)))

(defn remove-red [x]
  x)

(deftest part-1-tests
  (testing "Part 1"
    (is (= 145 (sum-numbers "{\"e\":{\"a\":{\"e\":-39,\"c\":119,\"a\":{\"c\":65}}}}")))))

(defcard part-1-result (sum-numbers input))

(deftest part-2-tests
  (testing "Part 2"
    (is (= "{\"e\":{\"abc\":30,\"a\":}}"
           (remove-red "{\"e\":{\"abc\":30,\"a\":{\"foo\":\"red\",\"e\":-39,\"c\":119,\"a\":{\"c\":65}}}}")))))
