(ns adventofcode-devcards.day-14
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input
  "Vixen can fly 8 km/s for 8 seconds, but then must rest for 53 seconds.
Blitzen can fly 13 km/s for 4 seconds, but then must rest for 49 seconds.
Rudolph can fly 20 km/s for 7 seconds, but then must rest for 132 seconds.
Cupid can fly 12 km/s for 4 seconds, but then must rest for 43 seconds.
Donner can fly 9 km/s for 5 seconds, but then must rest for 38 seconds.
Dasher can fly 10 km/s for 4 seconds, but then must rest for 37 seconds.
Comet can fly 3 km/s for 37 seconds, but then must rest for 76 seconds.
Prancer can fly 9 km/s for 12 seconds, but then must rest for 97 seconds.
Dancer can fly 37 km/s for 1 seconds, but then must rest for 36 seconds.")

(def test-input
  "Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.")

(defn line->reindeer [x]
  (let [[_ name speed fly-time rest-time] (re-matches #"^(\w+) can fly (\d+) km/s for (\d+) seconds, but then must rest for (\d+) seconds\.$" x)]
    {:name name
     :speed (int speed)
     :fly-time (int fly-time)
     :rest-time (int rest-time)}))

;; Part 1

(defn add-distance-xf [sec]
  (comp (mapcat (fn [[seconds distance-per-second]] (repeat seconds distance-per-second)))
        (take sec)))

(defn add-distance-after [sec {:keys [fly-time rest-time speed] :as r}]
  (let [s (cycle [[fly-time speed] [rest-time 0]])]
    (assoc r :distance (->> (transduce (add-distance-xf sec) + 0 s)))))

(defn get-distances [sec lines]
  (let [xf (comp (map line->reindeer)
                 (map (partial add-distance-after sec)))]
    (->> (into [] xf (clojure.string/split-lines lines))
         (sort-by :distance >)
         (map (juxt :name :distance)))))

;; Part 2

(defn- add-distances [{:keys [fly-time rest-time speed] :as r}]
  (let [c (cycle [[fly-time speed] [rest-time 0]])
        d (mapcat (fn [[seconds distance-per-second]]
                    (repeat seconds distance-per-second)) c)]
    (assoc r :distances d :distance 0 :points 0)))

(defn- step-distance [reindeer]
  (map (fn [r]
         (let [[d & remaining-ds] (:distances r)]
           (-> r
               (update :distance (partial + d))
               (assoc :distances remaining-ds)))) reindeer))

(defn- assign-points [reindeer]
  (let [sorted (sort-by :distance > reindeer)
        [leaders & rest] (partition-by :distance sorted)
        updated-leaders (map #(update % :points inc) leaders)]
    (flatten (cons updated-leaders rest))))

(defn get-points [sec lines]
  (let [steps (->> lines
                   clojure.string/split-lines
                   (map (comp add-distances line->reindeer))
                   (iterate (comp assign-points step-distance)))
        n (nth steps sec)
        n (sort-by :points > n)]
    (map (juxt :name :points :distance) n)))

(deftest part-1-tests
  (testing "Part 1"
    (is (= [["Dancer" 16] ["Comet" 14]] (get-distances 1 test-input)))
    (is (= [["Dancer" 160] ["Comet" 140]] (get-distances 10 test-input)))
    (is (= [["Dancer" 176] ["Comet" 140]] (get-distances 11 test-input)))
    (is (= [["Comet" 1120] ["Dancer" 1056]] (get-distances 1000 test-input)))))

(defcard part-1-result (get-distances 2503 input))

(deftest part-2-tests
  (testing "Part 2"
    (is (= [["Dancer" 1 16] ["Comet" 0 14]] (get-points 1 test-input)))
    (is (= [["Dancer" 139 176] ["Comet" 1 182]] (get-points 140 test-input)))
    (is (= [["Dancer" 689 1056] ["Comet" 312 1120]] (get-points 1000 test-input)))))

(defcard part-2-result (get-points 2503 input))
