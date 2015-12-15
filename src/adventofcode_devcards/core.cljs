(ns adventofcode-devcards.core
  (:require
   [sablono.core :as sab :include-macros true]
   [adventofcode-devcards.day-1]
   [adventofcode-devcards.day-2]
   [adventofcode-devcards.day-3]
   [adventofcode-devcards.day-4]
   [adventofcode-devcards.day-5]
   [adventofcode-devcards.day-6]))

(defn main []
  ;; conditionally start the app based on wether the #main-app-area
  ;; node is on the page
  (if-let [node (.getElementById js/document "main-app-area")]
    (js/React.render (sab/html [:div "This is working"]) node)))

(main)

;; remember to run lein figwheel and then browse to
;; http://localhost:3449/cards.html
