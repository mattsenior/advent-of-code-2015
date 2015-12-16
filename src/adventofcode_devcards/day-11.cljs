(ns adventofcode-devcards.day-11
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]))

(enable-console-print!)

;; Password validation

(defn correct-length? [x]
  (= 8 (count x)))

(def valid? (comp correct-length?))

;; Password sequences

(defn str->pw [x]
  (->> x
       seq
       reverse
       (map #(- (js/parseInt % 36) 10))))

(defn pw->str [x]
  (->> x
       reverse
       (map #(.toString (+ 10 %) 36))
       (clojure.string/join "")))

(defn inc-password [[a & b]]
  (let [a' (inc a)]
    (if (> a' 25)
      (cons 0 (when b (inc-password b)))
      (cons a' b))))

(defn passwords [x]
  (let [y (inc-password x)]
    (lazy-seq (cons x (passwords y)))))

(deftest part-1-tests
  (testing "str->pw"
    (is (= [0 0 0] (str->pw "aaa")))
    (is (= [1 1 1] (str->pw "bbb")))
    (is (= [25 25 25] (str->pw "zzz")))
    (is (= [2 1 0] (str->pw "abc")))
    (is (= [23 25 0] (str->pw "azx"))))
  (testing "correct-length?"
    (is (not (correct-length? (str->pw "xxx"))))
    (is (not (correct-length? (str->pw "xxxxxxxxx"))))
    (is (correct-length? (str->pw "xxxxxxxx"))))
  (testing "valid?"
    (is (valid? (str->pw "xxxxxxxx"))))
  (testing "Part 1"
    (is (= "y" (->> "x" str->pw passwords second pw->str)))
    (is (= "xy" (->> "xx" str->pw passwords second pw->str)))
    (is (= "xz" (->> "xy" str->pw passwords second pw->str)))
    (is (= "ya" (->> "xz" str->pw passwords second pw->str)))
    (is (= "xxxxxxxy" (->> "xxxxxxxx" str->pw passwords second pw->str)))
    ))

#_(defcard part-1-result (count (nth (look-and-say [1 3 2 1 1 3 1 1 1 2]) 40)))
#_(defcard part-2-result (count (nth (look-and-say [1 3 2 1 1 3 1 1 1 2]) 50)))
