(ns lightswitch.core
  (:require [clojure.math.numeric-tower :as math]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))


(defn combination [x y] 
  "From collection x choose a random, in-order subset of size y"
nil)

(defn test-expression [list-of-terms]
         (reduce bit-and (map (fn [a] (reduce bit-xor a)) list-of-terms)))
                  
(defn expr->assignment [list-of-terms assignment-map]
  "Replace the variables of an expression with the values assigned to them."
  (map
   (fn [a]
     (map (fn [b] (get assignment-map b))
          a))                     
   list-of-terms))



;(eval-expression '((a) (c) (a b c d) (c d e) (b e)) '((a 1) (b 1) (c 1) (d 0) (e 0)))
