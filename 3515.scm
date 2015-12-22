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

;l-cons must be defined as syntax else guile refuses to expand early enough
(define-syntax l-cons
	(syntax-rules ()
		((l-cons x y) (cons (delay x) (delay y)))))

(define (f-car st)
	(force (car st)))

(define (f-cdr st)
	(force (cdr st)))

;stream of 1s
(define ones (delay (cons 1 ones)))

(define l-ones (l-cons 1 l-ones))

;natural numbers TOFIX
(define nats 
	(let tail ([t 0])
		(l-cons t (tail (+ 1 t)))))

;first n elements of a lazy list
(define (first-n ls n)
	(define (first-n-aux ls n acc)
	  (cond
		 [(not (positive? n)) (cons (gl-n-th ls 0) acc)]
		 [else (first-n-aux ls (- n 1) (cons (gl-n-th ls n) acc))]
		 ))
	(first-n-aux ls (- n 1) '()))

#|
;lazy iota
(define iota-stream
  (cons 1 (delay (pointwise + 
|#

;manual experiments
;(cons (car (force (cdr (force (cdr (force ones)))))) (cons (car (force (cdr (force ones)))) (cons (car (force ones)) '())))
;$37 = (1 1 1)


