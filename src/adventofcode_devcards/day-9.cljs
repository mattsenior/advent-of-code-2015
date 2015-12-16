(ns adventofcode-devcards.day-9
  (:require
   [adventofcode-devcards.combinatorics :refer [permutations]])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input (slurp-input "9"))

(def test-input
  "London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141")

(defn add-distance [distances v]
  (let [[_ from to d] (re-find #"(\w+) to (\w+) = (\d+)" v)
        from (keyword from)
        to (keyword to)
        d (int d)]
    (-> distances
        (assoc-in [from to] d)
        (assoc-in [to from] d))))

(defn get-path-length [route distances]
  (->> (reduce (fn [{:keys [from path-length] :as acc} to]
                 (let [distance (get-in distances [from to])]
                   (-> acc
                       (assoc :from to)
                       (update :path-length (partial + distance)))))
               {:from (first route) :path-length 0}
               (rest route))
       :path-length))

(defn path-rf [comparator distances current next-route]
  (let [next-path-length (get-path-length next-route distances)]
    (if (or (nil? current)
            (comparator next-path-length (:path-length current)))
      {:path-length next-path-length
       :route next-route}
      current))) 

(defn get-winning-path [comparator lines]
  (let [distances (->> lines
                       clojure.string/split-lines
                       (reduce add-distance {}))
        nodes (keys distances)
        routes (permutations nodes)]
    (reduce (partial path-rf comparator distances) nil routes)))

(def get-shortest-path (partial get-winning-path <))
(def get-longest-path (partial get-winning-path >))

(deftest part-1-tests
  (testing "Part 1"
    (is (= 605 (:path-length (get-shortest-path test-input))))))

(defcard part-1-result (get-shortest-path input))

(deftest part-2-tests
  (testing "Part 2"
    (is (= 982 (:path-length (get-longest-path test-input))))))

(defcard part-2-result (get-longest-path input))
