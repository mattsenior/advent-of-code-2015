(ns adventofcode-devcards.day-15
  (:require
   [adventofcode-devcards.combinatorics :refer [permutations]])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input
  "Frosting: capacity 4, durability -2, flavor 0, texture 0, calories 5
Candy: capacity 0, durability 5, flavor -1, texture 0, calories 8
Butterscotch: capacity -1, durability 0, flavor 5, texture 0, calories 6
Sugar: capacity 0, durability 0, flavor -2, texture 2, calories 1")

(def test-input
  "Butterscotch: capacity -1, durability -2, flavor 6, texture 3, calories 8
Cinnamon: capacity 2, durability 3, flavor -2, texture -1, calories 3")

(defn to-ingredients [line]
  (let [[_ name capacity durability flavor texture calories]
        (re-matches #"^(\w+): capacity (-?\d+), durability (-?\d+), flavor (-?\d+), texture (-?\d+), calories (-?\d+)$" line)]
    {:name name
     :capacity (int capacity)
     :durability (int durability)
     :flavor (int flavor)
     :texture (int texture)
     :calories (int calories)}))

(def spoons 100)

(defn next-available-amounts [rem [a & b]]
  (let [available-for-a (- spoons (apply + b))
        a' (inc a)]
    (if (> a' available-for-a)
      (when-not (every? (partial = spoons) b)
        (cons 0 (when b (next-available-amounts 100 b))))
      (cons a' b))))

(defn available-amounts-seq [a]
  (lazy-seq
   (when-let [a' (next-available-amounts spoons a)]
     (cons a (available-amounts-seq a')))))

(defn get-combinations [ingredients]
  (let [available-amounts (repeat (count ingredients) 0)]
    (available-amounts-seq available-amounts)))

(def metrics [:capacity :durability :flavor :texture])

(def xf-filter-sums-to-total-spoons
  (filter (fn [x] (= spoons (apply + x)))))

(defn xf-apply-amounts [ingredients]
  (letfn [(calculate-ingredient [ingredient amount]
            (let [metric-vs (map #(* amount (get ingredient %)) metrics)]
              {:calories (* (:calories ingredient) amount)
               :metrics metric-vs}))

          (apply-amounts [amounts]
            (map calculate-ingredient ingredients amounts))]
    (map apply-amounts)))

(def xf-combine-ingredients
  (map (partial reduce (fn [acc {:keys [calories metrics]}]
                   (-> acc
                       (update :calories (partial + calories))
                       (update :metrics (partial map + metrics)))))))

(def xf-remove-negative-metrics
  (filter #(every? pos? (:metrics %))))

(defn xf-filter-calories [cals]
  (if (nil? cals)
    (map identity)
    (filter #(= cals (:calories %)))))

(def xf-multiply-metrics
  (map #(apply * (:metrics %))))

(defn get-best-recipe
  ([lines] (get-best-recipe nil lines))
  ([exact-calories lines]
   (let [ingredients (map to-ingredients (clojure.string/split-lines lines))
         combinations (get-combinations ingredients)
         xf (comp xf-filter-sums-to-total-spoons
                  (xf-apply-amounts ingredients)
                  xf-combine-ingredients
                  xf-remove-negative-metrics
                  (xf-filter-calories exact-calories)
                  xf-multiply-metrics)]
     (transduce xf max combinations))))

(deftest part-1-tests
  (testing "Part 1"
    (is (= 62842880 (get-best-recipe test-input)))))

#_(defcard part-1-result (get-best-recipe input))

(deftest part-2-tests
  (testing "Part 2"
    (is (= 57600000 (get-best-recipe 500 test-input)))))

(defcard part-2-result (get-best-recipe 500 input))
