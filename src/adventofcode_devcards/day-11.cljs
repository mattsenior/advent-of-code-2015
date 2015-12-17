(ns adventofcode-devcards.day-11
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing async]]))

(enable-console-print!)

;; Password validation

(defn correct-length? [x]
  (= 8 (count x)))

(defn has-straight? [[a & z]]
  (let [[b c & y] z]
    (if (and (= 1 (- a b)) (= 1 (- b c)))
      true
      (if (nil? y) false (has-straight? z)))))

(def i 8)
(def o 14)
(def l 11)
(def illegal-characters #{i o l})

(defn valid-characters? [x]
  (not-any? illegal-characters x))

(defn has-pairs? [x]
  (->> x
       (partition-by identity)
       (filter #(> (count %) 1))
       (map first)
       set
       count
       (<= 2)))

(def valid? (every-pred correct-length?
                        has-straight?
                        valid-characters?
                        has-pairs?))

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

(defn valid-passwords [x]
  (->> x passwords (filter valid?)))

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
  (testing "has-straight?"
    (is (= true (has-straight? (str->pw "abc"))))
    (is (= true (has-straight? (str->pw "bcd"))))
    (is (= true (has-straight? (str->pw "xyz"))))
    (is (= true (has-straight? (str->pw "zabc"))))
    (is (= true (has-straight? (str->pw "abcz"))))
    (is (= true (has-straight? (str->pw "zabcz"))))
    (is (= false (has-straight? (str->pw "abd"))))
    (is (= false (has-straight? (str->pw "aaa"))))
    (is (= false (has-straight? (str->pw "yyz"))))
    )
  (testing "valid-characters?"
    (is (= true (valid-characters? (str->pw "abcdefghjkmnpqrstuvwxyz"))))
    (is (= false (valid-characters? (str->pw "i"))))
    (is (= false (valid-characters? (str->pw "o"))))
    (is (= false (valid-characters? (str->pw "l"))))
    (is (= false (valid-characters? (str->pw "abcdefghijkl")))))
  (testing "has-pairs?"
    (is (= true (has-pairs? (str->pw "aacc"))))
    (is (= true (has-pairs? (str->pw "aabcc"))))
    (is (= false (has-pairs? (str->pw "aabaa")))))
  (testing "valid?"
    (is (valid? (str->pw "xxabcxyy"))))
  (testing "Part 1"
    (is (= "y" (->> "x" str->pw passwords second pw->str)))
    (is (= "xy" (->> "xx" str->pw passwords second pw->str)))
    (is (= "xz" (->> "xy" str->pw passwords second pw->str)))
    (is (= "ya" (->> "xz" str->pw passwords second pw->str)))
    (is (= "xxxxxxxy" (->> "xxxxxxxx" str->pw passwords second pw->str)))
    (is (= ["xxxxxxxy" "xxxxxxxz"] (->> "xxxxxxxx" str->pw passwords (map pw->str) (drop 1) (take 2))))
    (is (= ["xxxxyzaa" "xxxxyzbb"] (->> "xxxxxxxx" str->pw valid-passwords (map pw->str) (drop 1) (take 2))))
    ))

(defcard results (->> "hxbxwxba" str->pw valid-passwords (map pw->str) (take 2)))
