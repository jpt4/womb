(ns collatz.core)

(defn cseq [seed]
  (loop [s seed
         ls '()]
    (cond
      (= s 1) (concat ls '(1))
      (= 0 (mod s 2)) (recur (/ s 2) (concat ls (list s)))
      (= 1 (mod s 2)) (recur (+ 1 (* 3 s)) (concat ls (list s))))))

(declare geo-range) (declare branchable?) (declare new-branch-from)

(defn cbranch [start max] (geo-range start max 2))

(defn ctree [depth]
  (loop [tree (sorted-map 0 (cbranch 1 8))
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

(defn branch-seeds [t] 
  (reduce (fn [ls x] (concat ls (list (first (last x))))) '() t))

(defn is-prime? [n]
  (and (not (= 0 n)) (not (= 1 n))
       (not (reduce (fn [bc i] (or bc (= 0 (mod n (+ 1 i))))) 
                    false 
                    (range 1 (Math/floor (Math/sqrt n)))))))

;t <- tree
(defn prime-count [t]
  (count (filter is-prime? (branch-seeds t))))

(defn prime-ratio [t]
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
(defn primes-less-than [n] (count (filter is-prime? (range 1 n))))
(defn ctree-to-numberline-prime-ratio [n]
  (* 1.0 (/ (prime-count (ctree n)) (primes-less-than n))))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))
