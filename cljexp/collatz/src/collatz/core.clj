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

(defn-memo cseq [seed]
  (loop [s seed
         ls '()]
    (cond
      (= s 1) (concat ls '(1))
      (= 0 (mod s 2)) (recur (/ s 2) (concat ls (list s)))
      (= 1 (mod s 2)) (recur (+ 1 (* 3 s)) (concat ls (list s))))))

(declare geo-range) (declare branchable?) (declare new-branch-from)
(declare grow-ctree) (declare grow-ctree-aux)

(defn-memo cbranch [start max] (geo-range start max 2))

(defn-memo ctree [depth] (grow-ctree depth (sorted-map 0 (cbranch 1 4))))

;growable ctree
(defn-memo grow-ctree [depth tree-kernel]
  (loop [tree tree-kernel
         branch-index 0]
    (let [tree-max (last (get tree 0))
          branch (get tree branch-index)
          branch-max (last (get tree branch-index))]
      (cond
        ;at spine of tree
        (= branch-index 0)
        (let [leaf (* branch-max 2)]
          (if (> leaf depth)
            tree
            (if (branchable? leaf)
              (recur
               (assoc tree 
                      0 (concat branch (list leaf)) 
                      (count tree) (new-branch-from leaf))
               (if (= branch-index (- (count tree) 1))
                 0
                 (+ 1 branch-index)))
              (recur
               (assoc tree 0 (concat branch (list leaf)))
               (if (= branch-index (- (count tree) 1))
                 0
                 (+ 1 branch-index))))))
        ;between start and end
        (not (= branch-index 0))
        (let [leaf (* branch-max 2)]
          (if (> leaf tree-max)
            (recur tree (if (= branch-index (- (count tree) 1))
                          0
                          (+ 1 branch-index)))
            (if (branchable? leaf)
              (recur
               (assoc tree 
                      branch-index (concat branch (list leaf)) 
                      (count tree) (new-branch-from leaf))
               branch-index)
              (recur
               (assoc tree branch-index (concat branch (list leaf)))
               branch-index))))))))
            
;geometric progressions
(defn-memo geo-range [start end factor]
  (loop [s start
         ls (list start)]
    (let [next (* factor s)]
      (cond
        (> next end) ls
        (<= next end) (recur next (concat ls (list next)))))))

(defn-memo branchable? [n] (= 0 (mod (- n 1) 3)))

(defn-memo new-branch-from [n] (list (/ (- n 1) 3)))

(defn-memo size-of-ctree [t] (reduce (fn [acc x] (+ (count (last x)) acc)) 0 t))

(defn-memo branch-seeds [tree] 
  (loop [i (- (count tree) 1)
         ls '()]
    (cond
      (= i 0) (cons (first (get tree i)) ls)
      (> i 0) (recur (- i 1) (cons (first (get tree i)) ls)))))

(defn-memo is-prime? [n]
  (loop [bool true 
         root (Math/floor (Math/sqrt n))]
    (cond
      (or (= 0 n) (= 1 n)) false
      (== root 1) bool
      (== 0 (mod n root)) false
      (not (== 0 (mod n root))) (recur true (- root 1)))))

;t <- tree
(defn-memo prime-count [t]
  (+ 1 (count (filter is-prime? (branch-seeds t)))))

(defn tree-report [t]
  (let [tree-size (size-of-ctree t)
        seeds (branch-seeds t)
        seed-count (count seeds)
        primes (filter is-prime? seeds)
        prime-count (count primes)
        primes-to-seeds (/ prime-count seed-count)
        primes-to-tree (/ prime-count tree-size)]
    (println
     'tree-size tree-size
     'seed-count seed-count
     'prime-count prime-count
     'prime-count-to-seed-count-ratio primes-to-seeds
     'prime-count-to-tree-size-ratio primes-to-tree)))

(defn expected-prime-count [n] (/ n (Math/log n)))
(defn expected-prime-ratio [n] (/ 1 (Math/log n)))
(defn-memo prime-counting-fn [n] 
  (loop [acc 0 num n]
    (cond
      (<= num 1) acc
      (is-prime? num) (recur (+ 1 acc) (- num 1))
      'else (recur acc (- num 1)))))

;ctree of depth(tree), numberline [1, num]
(defn-memo ctree-to-numberline-prime-ratio [tree num]
  (/ (* 1.0 (prime-count tree)) (prime-counting-fn num)))

(defn-memo cumulative-comparison [max-depth]
  (loop [depth 4
         tree (ctree depth)
         out (sorted-map depth (ctree-to-numberline-prime-ratio tree depth))]
    (cond
      (= depth max-depth) out
      (< depth max-depth)
      (let [next-depth (+ 1 depth)
            next-tree (grow-ctree next-depth tree)
            next-out 
            (assoc out 
                   next-depth
                   (ctree-to-numberline-prime-ratio next-tree next-depth))]
        (recur next-depth next-tree next-out)))))

;;graph translation
;;TODO - create collatz structures as visualizable graphs from the start
(defn cseq->graph [cseq]
  (loop [g (ub/digraph nil)
         c cseq]
    (cond
      (empty? c) (ub/remove-nodes g nil)
      (not (empty? c)) 
      (recur (ub/add-directed-edges g [(first c) (first (next c))])
             (next c)))))

(defn combine-graphs [g1 g2]
  (ub/add-edges* 
   g1
   (map (partial ub/edge-with-attrs g2) (ub/edges g2))))

(defn ctree->graph [ctree]
  (loop [g (ub/digraph nil)
         i 0]
    (cond
      (= i (count ctree)) (ub/remove-nodes g nil)
      (< i (count ctree))
      (let [b (get ctree i)]
        (recur 
         (combine-graphs 
          (ub/add-directed-edges 
           g 
           [(first b) (+ 1 (* 3 (first b)))])
          (cseq->graph (reverse b)))
         (+ 1 i))))))

;;visualization process
;create ctree
;(def c1 (ctree 100))
;create graph
;(def g1 (ctree->graph c1))
;visualize graph
;(ub/viz-graph g1)
;alternatively
;(ub/viz-graph (ctree->graph (ctree 100)))
;see collatz/gdefault.png for an example, generated using
;(ub/viz-graph (ctree->graph (ctree 100)))
;{:save {:filename "gdefault.png" :format :png}})

;;data analysis
(defn cumulative-plot [cc]
                (let [x (map first cc)
                      y (map second cc)]
                  (charts/scatter-plot x y)))

;(def cctest (cumulative-plot (cumulative-comparison 10000)))
;(incc/view cc)
;(incc/save cctest "cctest.png")

;tests
;collatz.core> (= (grow-ctree 300 (ctree 150)) (grow-ctree 300 (ctree 300)))
;true
;collatz.core> (= (grow-ctree 300 (ctree 150)) (grow-ctree 300 (ctree 150)))
;true

;memoization benefits
;collatz.core> (time (count (ctree 2000)))
;"Elapsed time: 16.348727 msecs"
;88
;collatz.core> (time (count (ctree 2000)))
;"Elapsed time: 0.216717 msecs"
;88

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
