(ns adventofcode-devcards.day-18
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def ON "#")
(def OFF ".")

(def input (slurp-input "18"))

(def test-input
  ".#.#.#
...##.
#....#
..#...
#.#..#
####..")

(def test-input-2
  "##.#.#
...##.
#....#
..#...
#.#..#
####.#")

(defn off-row [rows]
  (repeat (count (first rows)) false))

(defn add-surrounding [pad xs]
  [xs
   ;; Above or Left
   (drop-last (cons pad xs))
   ;; Below or Right
   (concat (drop 1 xs) [pad])])

(defn add-above-and-below [rows]
  (add-surrounding (off-row rows) rows))

(defn add-left-and-right [row]
  (add-surrounding false row))

(defn add-lr-to-all [& rows]
  (mapcat add-left-and-right rows))

(defn all->next [row]
  (apply map (fn [x & neighbours]
               (let [c (count (filter identity neighbours))]
                 (or (and x (or (= c 2) (= c 3)))
                       (and (not x) (= c 3))))) row))

(defn switch-on-first-and-last [row]
  (concat [true] (drop 1 (drop-last row)) [true]))

(defn switch-on-corners [lights]
  (let [top (switch-on-first-and-last (first lights))
        bottom (switch-on-first-and-last (last lights))]
    (concat [top] (drop 1 (drop-last lights)) [bottom])))

(defn next-frame [lights]
  (let [ab (add-above-and-below lights)
        all (apply map add-lr-to-all ab)]
    (map all->next all)))

(defn frames [lights]
  (lazy-seq
   (let [next-lights (next-frame lights)]
     (cons next-lights (frames next-lights)))))

(defn frames-with-stuck-corners [lights]
  (lazy-seq
   (let [lights (switch-on-corners lights)
         next-lights (next-frame lights)
         next-lights (switch-on-corners next-lights)]
     (cons next-lights (frames-with-stuck-corners next-lights)))))

(defn count-lights [lights]
  (apply + (map #(count (filter identity %)) lights)))

(defn light->bool [x] (condp = x ON true OFF false))

(defn bool->light [x] (condp = x true ON false OFF))

(defn lines->lights [lines]
  (->> lines
       clojure.string/split-lines
       (map #(map light->bool (vec %)))))

(defn lights->lines [lights]
  (->> lights
       (map #(apply str (map bool->light %)))
       (clojure.string/join "\n")))

(def expected-1 "..##..\n..##.#\n...##.\n......\n#.....\n#.##..")
(def expected-2 "..###.\n......\n..###.\n......\n.#....\n.#....")
(def expected-3 "...#..\n......\n...#..\n..##..\n......\n......")
(def expected-4 "......\n......\n..##..\n..##..\n......\n......")

(def expected-part-2-1 "#.##.#\n####.#\n...##.\n......\n#...#.\n#.####")
(def expected-part-2-2 "#..#.#\n#....#\n.#.##.\n...##.\n.#..##\n##.###")
(def expected-part-2-3 "#...##\n####.#\n..##.#\n......\n##....\n####.#")
(def expected-part-2-4 "#.####\n#....#\n...#..\n.##...\n#.....\n#.#..#")
(def expected-part-2-5 "##.###\n.##..#\n.##...\n.##...\n#.#...\n##...#")

(deftest part-1
  (testing "Part 1"
    (let [result (frames (lines->lights test-input))]
      (is (= expected-1 (lights->lines (nth result 0))))
      (is (= expected-2 (lights->lines (nth result 1))))
      (is (= expected-3 (lights->lines (nth result 2))))
      (is (= expected-4 (lights->lines (nth result 3))))
      (is (= 4 (count-lights (nth result 3)))))))

(deftest part-2
  (testing "Part 2"
    (let [result (frames-with-stuck-corners (lines->lights test-input-2))]
      (is (= expected-part-2-1 (lights->lines (nth result 0))))
      (is (= expected-part-2-2 (lights->lines (nth result 1))))
      (is (= expected-part-2-3 (lights->lines (nth result 2))))
      (is (= expected-part-2-4 (lights->lines (nth result 3))))
      (is (= expected-part-2-5 (lights->lines (nth result 4))))
      (is (= 17 (count-lights (nth result 4)))))))

#_(defcard part-1-result (count-lights (nth (frames (lines->lights input)) 99)))

(defcard part-2-result (count-lights (nth (frames-with-stuck-corners (lines->lights input)) 99)))
 
