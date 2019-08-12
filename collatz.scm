;;2090811Z
;;collatz.scm
;;Chez Scheme v9.5
;;jpt4
;;Collatz Conjecture experiments.

(define (collatz-list num)
  (let loop ([ls '()]
	     [n num])
    (cond
     [(eq? n 1) (reverse (cons 1 ls))]
     [(eq? (mod n 2) 0) (loop (cons n ls)
			      (/ n 2))]
     [(eq? (mod n 2) 1) (loop (cons n ls)
			      (+ (* n 3) 1))])))

(define (collatz-joints cls)
  (filter (lambda (a) (eq? (mod (+ (* a 3) 1) 2) 0)) cls))

(define (joints-list-ratio cls)
  (/ (* 1.0 (length (collatz-joints cls))) (length cls)))

(define (prime? num)
  (if (or (eq? 1 num) (eq? 0 num))
      #f
      (not (ormap (lambda (a) (eq? (mod num a) 0)) 
		  (cddr (iota (num->fixnum (+ (floor (sqrt num)) 1))))))))

(define (num->fixnum num)
  (cond
   [(fixnum? num) num]
   [(flonum? num) (flonum->fixnum num)]))
     
(define (prime-ratio ls)
  (/ (length (filter prime? ls)) (* (length ls) 1.0)))

;depth = all trajectories of length d
(define (collatz-branch start depth)
  (map (lambda (a) (* (expt 2 a) start)) (iota depth)))

(define (collatz-tree start depth)
  (cond
   [(eq? depth 1) (list start)]
   [(and (eq? (mod (/ (sub1 start) 3) 2) 1)
	 (> start 4))
    (cons start (cons (collatz-tree (/ (sub1 start) 3) (sub1 depth))
		      (collatz-tree (* 2 start) (sub1 depth))))]
   [(cons start (collatz-tree (* 2 start) (sub1 depth)))]))
