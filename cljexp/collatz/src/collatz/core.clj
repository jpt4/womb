(ns collatz.core)

(defn cseq [seed]
  (loop [s seed
         ls '()]
    (cond
      (= s 1) (concat ls '(1))
      (= 0 (mod s 2)) (recur (/ s 2) (concat ls (list s)))
      (= 1 (mod s 2)) (recur (+ 1 (* 3 s)) (concat ls (list s))))))

(declare geo-range) (declare branchable?) (declare new-branch-from)
(declare grow-ctree)

(defn cbranch [start max] (geo-range start max 2))

(defn ctree [depth] (grow-ctree depth (sorted-map 0 (cbranch 1 4))))

;growable ctree
(defn grow-ctree [depth tree-kernel]
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
(defn geo-range [start end factor]
  (loop [s start
         ls (list start)]
    (let [next (* factor s)]
      (cond
        (> next end) ls
        (<= next end) (recur next (concat ls (list next)))))))

(defn branchable? [n] (= 0 (mod (- n 1) 3)))

(defn new-branch-from [n] (list (/ (- n 1) 3)))

(defn size-of-ctree [t] (reduce (fn [acc x] (+ (count (last x)) acc)) 0 t))

(defn branch-seeds [tree] 
  (loop [i (- (count tree) 1)
         ls '()]
    (cond
      (= i 0) (cons (first (get tree i)) ls)
      (> i 0) (recur (- i 1) (cons (first (get tree i)) ls)))))

(defn is-prime? [n]
  (loop [bool true 
         root (Math/floor (Math/sqrt n))]
    (cond
      (or (= 0 n) (= 1 n)) false
      (== root 1) bool
      (== 0 (mod n root)) false
      (not (== 0 (mod n root))) (recur true (- root 1)))))

;t <- tree
(defn prime-count [t]
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
(defn prime-counting-fn [n] 
  (loop [acc 0 num n]
    (cond
      (<= num 1) acc
      (is-prime? num) (recur (+ 1 acc) (- num 1))
      'else (recur acc (- num 1)))))

;ctree of depth(tree), numberline [1, num]
(defn ctree-to-numberline-prime-ratio [tree num]
  (/ (* 1.0 (prime-count tree)) (prime-counting-fn num)))

(defn cumulative-comparison [max-depth]
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
   
;tests
;collatz.core> (= (grow-ctree 300 (ctree 150)) (grow-ctree 300 (ctree 300)))
;true
;collatz.core> (= (grow-ctree 300 (ctree 150)) (grow-ctree 300 (ctree 150)))
;true



(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
