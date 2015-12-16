;;; combinatorics.clj: efficient, functional algorithms for generating lazy
;;; sequences for common combinatorial functions.

;; by Mark Engelberg (mark.engelberg@gmail.com)
;; Last updated - March 20, 2015

(ns
  #^{:author "Mark Engelberg",
     :doc "Efficient, functional algorithms for generating lazy
sequences for common combinatorial functions. (See the source code 
for a longer description.)"}
  adventofcode-devcards.combinatorics
  (:refer-clojure :exclude [update]))

(defn- all-different?
  "Annoyingly, the built-in distinct? doesn't handle 0 args, so we need
  to write our own version that considers the empty-list to be distinct"
  [s]
  (if (seq s)
    (apply distinct? s)
    true))

(defn- iter-perm [v]
  (let [len (count v),
        j (loop [i (- len 2)]
            (cond (= i -1) nil
                  (< (v i) (v (inc i))) i
                  :else (recur (dec i))))]
    (when j
      (let [vj (v j),
            l (loop [i (dec len)]
                (if (< vj (v i)) i (recur (dec i))))]
        (loop [v (assoc v j (v l) l vj), k (inc j), l (dec len)]
          (if (< k l)
            (recur (assoc v k (v l) l (v k)) (inc k) (dec l))
            v))))))

(defn- vec-lex-permutations [v]
  (when v (cons v (lazy-seq (vec-lex-permutations (iter-perm v))))))

(defn- lex-permutations
  "DEPRECATED as a public function.

In prior versions of the combinatorics library, there were two similar functions: permutations and lex-permutations.  It was a source of confusion to know which to call.  Now, you can always call permutations.  When appropriate (i.e., when you pass in a sorted sequence of numbers), permutations will automatically call lex-permutations as a speed optimization."
  [c]
  (lazy-seq
    (let [vec-sorted (vec (sort c))]
      (if (zero? (count vec-sorted))
        (list [])
        (vec-lex-permutations vec-sorted)))))

(defn- sorted-numbers?
  "Returns true iff s is a sequence of numbers in non-decreasing order"
  [s]
              (and (every? number? s)
       (or (empty? s) (apply <= s))))

(defn- multi-perm
  "Handles the case when you want the permutations of a list with duplicate items."
  [l]
  (let [f (frequencies l),
        v (vec (distinct l)),
        indices (apply concat
                       (for [i (range (count v))]
                         (repeat (f (v i)) i)))]
    (map (partial map v) (lex-permutations indices))))

(defn permutations
  "All the distinct permutations of items, lexicographic by index 
(special handling for duplicate items)."
  [items]
  (cond
    (sorted-numbers? items) (lex-permutations items),
    
    (all-different? items)
    (let [v (vec items)]
      (map #(map v %) (lex-permutations (range (count v)))))
    
    :else
    (multi-perm items)))
