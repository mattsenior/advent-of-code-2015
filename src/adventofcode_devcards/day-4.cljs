(ns adventofcode-devcards.day-4
  (:require
   [sablono.core :as sab :include-macros true]
   [goog.crypt :as gcrypt])
  (:require-macros
   [devcards.core :as dc :refer [defcard deftest]]
   [cljs.test :refer [is testing]])
  (:import [goog.crypt Md5]))

(enable-console-print!)

(defn md5 [x]
  (let [md5 (Md5.)
        _ (.update md5 x)]
    (gcrypt/byteArrayToHex (.digest md5))))

(defn find-lowest-input
  [zeroes secret-key]
  (let [z (apply str (repeat zeroes "0"))]
    (->> (range)
         (map (fn [i]
                (let [hash (subs (md5 (str secret-key i)) 0 zeroes)]
                  {:i i :hash hash})))
         (filter #(= z (:hash %)))
         first
         :i
         )))

(deftest part-1-tests
  (testing
    "If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so."
    (is (= 609043 (find-lowest-input 5 "abcdef"))))
  (testing
      "If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef...."
    (is (= 1048970 (find-lowest-input 5 "pqrstuv")))))

(def input "yzbqklnj")

(defcard part-1-result
  (find-lowest-input 5 input))

(defcard part-2-result
  (find-lowest-input 6 input))
