(ns adventofcode-devcards.macros
  (:refer-clojure :exclude [slurp])
  (:require [clojure.java.io :as io]))

(defmacro slurp-input [n]
  (clojure.core/slurp (io/resource (str "input/day-" n))))
