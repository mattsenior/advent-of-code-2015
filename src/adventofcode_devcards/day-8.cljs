(ns adventofcode-devcards.day-8
  (:require
   [sablono.core :as sab :include-macros true]
   [cljs.core.async :refer [chan >! <! take!]])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]
   [cljs.core.async.macros :refer [go go-loop]]
   [adventofcode-devcards.macros :refer [slurp-input]]))

(enable-console-print!)

(def input (slurp-input "8"))

(def test-input-1 (->> input
                     clojure.string/split-lines
                     (take 10)
                     (clojure.string/join "\n")))

(def test-input-2
  "\"\"
\"abc\"
\"aaa\\\"aaa\"
\"\\x27\"
\"abc\\\\def\"
")

(def parse-replacement-pairs
  {#"^\"" ""
   #"\"$" ""
   #"\\\"" "."
   #"\\\\" "."
   #"\\x[0-9a-fA-F]{2}" "."})

(defn parse-line [s]
  (let [total-count (count s)
        data (reduce-kv clojure.string/replace s parse-replacement-pairs)
        data-count (count data)
        cruft-count (- total-count data-count)]
    {:total total-count
     :data data-count
     :cruft cruft-count}))

(defn parse [lines]
  (->> lines
       clojure.string/split-lines
       (map parse-line)
       (reduce (partial merge-with +))))

(def encode-replacement-pairs
  {#"\\" "\\\\"   ; Slashes first
   #"\"" "\\\""}) ; then quotes

(defn encode-line [s]
  (reduce-kv clojure.string/replace s encode-replacement-pairs))

(defn encode [lines]
  (->> lines
       clojure.string/split-lines
       (map encode-line)
       (map #(str "\"" % "\""))
       (clojure.string/join "\n")))

(deftest part-1-tests
  (testing "Part 1"
    (is (= 34 (:cruft (parse test-input-1))))
    (is (= 15 (:cruft (parse test-input-2))))))

(defcard part-1-result (parse input))

(def test-input-3
  "\"\"
\"abc\\x27a\\b\\\"c\"")

(deftest part-2-tests
  (testing "Part 2"
    (is (= "\"\\\"\\\"\"\n\"\\\"abc\\\\x27a\\\\b\\\\\\\"c\\\"\"" (encode test-input-3)))))

(defcard part-2-result (parse (encode input)))
