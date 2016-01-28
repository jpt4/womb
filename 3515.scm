;3515.scm
;cracklepop task sandbox
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

;natural numbers stream
(define nats 
	(let tail ([t 0])
		(l-cons t (tail (+ 1 t)))))

;cracklePop stream
(define cp
	(let next ([n nats])
		(cond
		 [(eq? (modulo (f-car n) 15) 0) (l-cons "CracklePop" (next (f-cdr n)))]
		 [(eq? (modulo (f-car n) 3) 0) (l-cons "Crackle" (next (f-cdr n)))]
		 [(eq? (modulo (f-car n) 5) 0) (l-cons "Pop" (next (f-cdr n)))]
		 [else (l-cons (f-car n) (next (f-cdr n)))])))

;lazy map over a stream
(define (l-map op st)
	(let next ([s st])
		(l-cons (op (f-car s)) (next (f-cdr s)))))

;CracklePop predicate
(define (cp? num)
	(cond
	 [(eq? (modulo num 15) 0) "CracklePop"]
	 [(eq? (modulo num 3) 0) "Crackle"]
	 [(eq? (modulo num 5) 0) "Pop"]
	 [else num]))

;first n elements of a lazy list
(define (first-n ls n)
	(cond
	 [(zero? n) '()]
	 [else (cons (f-car ls) (first-n (f-cdr ls) (- n 1)))]))

;full task
(define CracklePop
	(cdr (first-n (l-map cp? nats) 101)))


(define (first-n-acc stream num)
	(let aux ([st stream] [n num] [acc '()])
						(cond
						 [(zero? n) (reverse acc)]
						 [else (aux (f-cdr st) (- n 1) (cons (f-car st) acc))])))

(define (first-n-acc2 stream num)
	(if (zero? num) '()
			(let aux ([st (f-cdr stream)] [n num] [acc (list (f-car stream))])
				(cond
				 [(eq? n 1) acc]
				 [else (aux (f-cdr st) (- n 1) (append acc (list (f-car st))))]))))

(define last-arg 
	(lambda (f s p . t)
		(if (null? t)
				(list f s)
				(list t))))	

(define (opt-arg a . b)
	(cond
	 [(null? b) a]
	 [(zero? (car b)) 'zero] ;car to access element of residual list b
	 [else b]))
	
#|
(define (first-n-aux ls n acc)
	  (cond
		 [(not (positive? n)) (cons (gl-n-th ls 0) acc)]
		 [else (first-n-aux ls (- n 1) (cons (gl-n-th ls n) acc))]
		 ))
	(first-n-aux ls (- n 1) '()))

(define one-list 
	(lambda (n)
		(if (zero? n)
				'()
				(cons 1 (one-list (- n 1))))))

;lazy iota
(define iota-stream
  (cons 1 (delay (pointwise + 
|#

;manual experiments
;(cons (car (force (cdr (force (cdr (force ones)))))) (cons (car (force (cdr (force ones)))) (cons (car (force ones)) '())))
;$37 = (1 1 1)
