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
  (andmap (lambda (a) (eq? (mod num a) 0)) 
	  (cdr (iota (+ (floor (sqrt num)) 1)))))
  
  
	
