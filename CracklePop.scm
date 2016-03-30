;CracklePop.scm
;CracklePop task
;jpt4
;UTC20160101

;language: Guile Scheme v2.2

#|
f(x) = {
        crackle   , 0 = x (mod 3)
        pop       , 0 = x (mod 5)
        cracklepop, 0 = x (mod 15)
        x         , else
       }

L = { f(x) | x = [0, 100] }
|#

;l-cons must be defined as syntax else guile evaluates its arguments eagerly.
(define-syntax l-cons
	(syntax-rules ()
		((l-cons x y) (cons (delay x) (delay y)))))
(define (f-car st)
	(force (car st)))
(define (f-cdr st)
	(force (cdr st)))

;lazy map over a stream
(define (l-map op st)
	(let next ([s st])
		(l-cons (op (f-car s)) (next (f-cdr s)))))

;first n elements of a lazy list
(define (first-n ls n)
	(cond
	 [(zero? n) '()]
	 [else (cons (f-car ls) (first-n (f-cdr ls) (- n 1)))]))

;natural numbers stream
(define nats 
	(let tail ([t 0])
		(l-cons t (tail (+ 1 t)))))

;CracklePop predicate
(define (cp? num)
	(cond
	 [(eq? (modulo num 15) 0) (string->symbol "CracklePop")]
	 [(eq? (modulo num 3) 0) (string->symbol "Crackle")]
	 [(eq? (modulo num 5) 0) (string->symbol "Pop")]
	 [else num]))

;CracklePop stream for [1, 100]
(define CracklePop
	(cdr (first-n (l-map cp? nats) 101)))

;automatically evaluate CracklePop in guile REPL upon (load "CracklePop.scm")
CracklePop
