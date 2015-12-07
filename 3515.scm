;3515.scm
;cracklepop task
;jpt4
;UTC20151207

#|
f(x) = {
        crackle   , 0 = x (mod 3)
        pop       , 0 = x (mod 5)
        cracklepop, 0 = x (mod 15)
        x         , else
       }

L = { f(x) | x = [0, 100] }
|#

; Give the n-th element of a lazy list
(define (n-th L n)
	(let ((L (force L)))
		(if (positive? n) (n-th (cdr L) (- n 1))
				(car L))))

(define (pointwise f L1 L2)
	(let ((L1 (force L1)) (L2 (force L2)))
		(cond
		 ((null? L1) '())
		 ((null? L2) '())
		 (else (cons (f (car L1) (car L2))
								 (delay (pointwise f (cdr L1) (cdr L2))))))))

(define fibs (cons 1 (cons 1 (delay (pointwise + fibs (cdr fibs))))))

;stream of 1s
(define ones (delay (cons 1 ones)))

;first n elements of a lazy list
(define (first-n ls n)
	(cond
	 [(zero? (- n 1)) (cons (n-th ls n) '())]
	 [else (cons (n-th ls n) (first-n ls (- n 1)))]
	 ))

#|
;lazy iota
(define iota-stream
  (cons 1 (delay (pointwise + 
|#

;manual experiments
(cons (car (force (cdr (force (cdr (force ones)))))) (cons (car (force (cdr (force ones)))) (cons (car (force ones)) '())))
;$37 = (1 1 1)


