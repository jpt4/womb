;; collatz/src/collatz/core.clj
;; 20220209Z
;; jpt4
;; Implement the Collatz Project

;; Notes:

;;20220207Z
;;Revised algorithm to produce a canonical ordering of the Collatz Tree:
;Given a tree of size k, and a desired tree of size s, calculate all valid 
;next largest extensions to the tree, select the smallest such extension, 
;and append it. Increment k (optionally annotating the selected extension
;with its index), and iterate until k=s.


;;20210131Z New forward and reverse Collatz implementation, tree growth ordering

(ns collatz.core
  (:require [ubergraph.core :as ub]
            [incanter.core :as incc]
            [incanter.charts :as charts]
            ))

;define memoized function 
(defmacro defn-memo [name [& params*] body]
    `(def ~name
      (memoize
       (fn [~@params*] ~body))))

;; Assuming a list (tree) of lists (branches) of values (leaves)
(def tst-tree
  '((1 2 4 8 16) (5 10 20) (3 6 12)))

;; Shrinking orbit
(defn-memo czf-step [n]
  (cond
    (= 0 (mod n 2)) (/ n 2)
    (= 1 (mod n 2)) (+ (* 3 n) 1)))

;; Collatz function, "forward" calculation, from leaf to root
(defn-memo czf [leaf]
  (loop [f leaf ls '()]
    (if (= f 1) 
      (concat ls '(1))
      (recur (czf-step f) (concat ls (list f))))))

;; Growing orbit
(defn-memo czr-step [n]
  (if (= 4 (mod n 6))
    (list (* 2 n) (/ (- n 1) 3))
    (* 2 n)))

;; Auxiliary definitions for generating the Collatz Tree

;; tree-member?
(defn-memo tree-member? [leaf tree]
  (let [ls (flatten tree)]
    (not (empty? (filter (fn [a] (= leaf a)) ls)))))

;; find-tips
(defn-memo find-tips [tree] 
  (let [all-tips (reduce 
                  (fn [acc a]
                    (let [tips (czr-step (last a))]
                      (if (list? tips)
                        (cons (list (last a) (first tips))
                              (cons (list (last a) (last tips))
                                    acc))
                        (cons (list (last a) tips) acc))))
                  '()
                  tree)]
    (filter (fn [a] (not (tree-member? (last a) tree))) all-tips)))

;; min-tips
(defn-memo min-tip [tree]
  (let [tips (find-tips tree)]
    (reduce (fn [acc a] 
              (cond
                (not (list? acc)) a
                (< (last acc) (last a)) acc
                (<= (last a) (last acc)) a))
            0
            tips)))

;; Append a leaf to the Collatz tree
(defn-memo czr-tree-append [[parent child] tree]
  (if (< parent child)
    (map (fn [a] 
           (if (= (last a) parent)
             (concat a (list child))
             a))
         tree)
    (concat tree (list (list child)))))

;; Extend tree with next smallest leaf
(defn-memo extend-czr-step [tree]
 (czr-tree-append (min-tip tree) tree))

;; Collatz relation, "reverse" calculation, from root to leaves
(defn-memo czr [size]
  (if (< size 1)
    '()
    (loop [i 0
           tree '((1))]
      (if (= i (- size 1))
        tree
        (recur (+ i 1) (extend-czr-step tree))))))

;; Auxiliary definitions for generating an Extensible Collatz Tree

;; Total number of elements in a Collatz Tree
(defn-memo czr-tree-size [tree] (count (flatten tree)))

;; Extensible Collatz Tree
(defn-memo extend-czr [tree size]
  (let [current-size (czr-tree-size tree)]
    (loop [t tree s current-size]
      (if (>= s size)
        t
        (recur (extend-czr-step t) (+ 1 s))))))

;; Prime generation using the reverse Collatz map

;; Collect roots of branches
(defn-memo collect-roots [tree] (flatten (map first tree)))

;; Prime test
(defn-memo is-prime? [n]
  (loop [bool true 
         root (Math/floor (Math/sqrt n))]
    (cond
      (or (= 0 n) (= 1 n)) false
      (== root 1) bool
      (== 0 (mod n root)) false
      (not (== 0 (mod n root))) (recur true (- root 1)))))

;; (prime-count)
(defn-memo prime-count [t]
  (+ 1 (count (filter is-prime? (collect-roots t)))))

;; Visualize the Collatz map, forward and reverse

