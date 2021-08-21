(ns nan.core
  (:require [clojure.core.match :as mat]))

;define memoized function 
(defmacro defn-memo [name [& params*] body]
    `(def ~name
      (memoize
       (fn [~@params*] ~body))))

(defn-memo is-prime? [n]
  (loop [bool true 
         root (Math/floor (Math/sqrt n))]
    (cond
      (or (= 0 n) (= 1 n)) false
      (== root 1) bool
      (== 0 (mod n root)) false
      (not (== 0 (mod n root))) (recur true (- root 1)))))

(defn primes-unto [num]
  (filter is-prime? (range 2 (+ 1 num))))

;num -> list of pairs (prime exponent)
(defn prime-factors [num]
  (let [pls (primes-unto (Math/floor (Math/sqrt num)))]
    (loop [fls '()
           root ]
    (cond
      (or (= 0 n) (= 1 n)) fls
      (== root 1) (list (list num 1))
      (== 0 (mod n root)) 
      (not (== 0 (mod n root))) (recur true (- root 1)))))

;(defn pm->gn [exp]

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
