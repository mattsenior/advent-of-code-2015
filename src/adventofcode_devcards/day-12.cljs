(ns adventofcode-devcards.day-12
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input (slurp-input "12"))

(defn sum-numbers [x]
  (->> x (re-seq #"-?\d+") (map int) (reduce + 0)))

(defn json->clj [x]
  (js->clj (js/JSON.parse x)))

(defn clj->json [x]
  (js/JSON.stringify (clj->js x)))

(defn remove-red [x]
  (condp #(%1 %2) x
    map? (when-not (some #{"red"} (vals x))
           (reduce-kv (fn [acc k v] (assoc acc k (remove-red v))) {} x))
    sequential? (map remove-red x)
    x))

(deftest part-1-tests
  (testing "Part 1"
    (is (= 145 (sum-numbers "{\"e\":{\"a\":{\"e\":-39,\"c\":119,\"a\":{\"c\":65}}}}")))))

(defcard part-1-result (sum-numbers input))

(deftest part-2-tests
  (testing "Part 2"
    (is (= "{\"e\":{\"abc\":30,\"a\":null}}"
           (->> "{\"e\":{\"abc\":30,\"a\":{\"foo\":\"red\",\"e\":-39,\"c\":119,\"a\":{\"c\":65}}}}" json->clj remove-red clj->json)))
    (is (= "[\"red\",20,{\"e\":{\"abc\":30,\"a\":null}}]"
           (->> "[\"red\",20,{\"e\":{\"abc\":30,\"a\":{\"foo\":\"red\",\"e\":-39,\"c\":119,\"a\":{\"c\":65}}}}]" json->clj remove-red clj->json)))
    (is (= 30 (->> "{\"e\":{\"abc\":30,\"a\":{\"foo\":\"red\",\"e\":-39,\"c\":119,\"a\":{\"c\":65}}}}" json->clj remove-red clj->json sum-numbers)))))

(defcard part-2-result (->> input json->clj remove-red clj->json sum-numbers))
