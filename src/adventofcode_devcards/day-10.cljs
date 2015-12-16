(ns adventofcode-devcards.day-10
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]))

(enable-console-print!)

(def look-and-say-xf
  (comp (partition-by identity)
        (mapcat (fn [x] [(count x) (first x)]))))

(defn look-and-say [n]
  (let [x (sequence look-and-say-xf n)]
    (lazy-seq (cons n (look-and-say x)))))

(deftest part-1-tests
  (testing "Part 1"
    (is (= [1 1] (nth (look-and-say [1]) 1)))
    (is (= [2 1] (nth (look-and-say [1 1]) 1)))
    (is (= [1 2 1 1] (nth (look-and-say [2 1]) 1)))
    (is (= [1 1 1 2 2 1] (nth (look-and-say [1 2 1 1]) 1)))
    (is (= [3 1 2 2 1 1] (nth (look-and-say [1 1 1 2 2 1]) 1)))
    (is (= [3 1 2 2 1 1] (nth (look-and-say [1]) 5)))))

#_(defcard part-1-result (count (nth (look-and-say [1 3 2 1 1 3 1 1 1 2]) 40)))
#_(defcard part-2-result (count (nth (look-and-say [1 3 2 1 1 3 1 1 1 2]) 50)))
