;http://okmij.org/ftp/Scheme/lazy-Fibonacci.txt

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

;what I didn't read
                ; Lazy cons
(define-macro (l-cons x y) `(cons ,x (delay ,y)))

                ; Eager null? car and cdr
                ; Note, Gambit's null? car and cdr force their arguments
                ; by default. The following macros can be regular
                ; functions as well.

(define-macro (e-null? x) `(null? (force ,x)))
(define-macro (e-car x) `(car (force ,x)))
(define-macro (e-cdr x) `(cdr (force ,x)))

(define (l-pointwise f L1 L2)
     (cond
     ((e-null? L1) '())
     ((e-null? L2) '())
     (else (l-cons (f (e-car L1) (e-car L2))
                   (l-pointwise f (e-cdr L1) (e-cdr L2))))))

(define l-fibs (l-cons 1 (l-cons 1 (l-pointwise + l-fibs (e-cdr l-fibs)))))

(define (l-n-th L n)
    (if (positive? n) (l-n-th (e-cdr L) (- n 1)) (e-car L)))

;gavwhela - correcting for R5/6RS car/cdr non-deafault argument forcing
(define-syntax gl-cons
	(syntax-rules ()
		((gl-cons x y) (cons (delay x) (delay y)))))
(define-syntax ge-car
	(syntax-rules ()
		((ge-car x) (force (car x)))))
(define-syntax ge-cdr
	(syntax-rules ()
		((ge-cdr x) (force (cdr x)))))
(define (gl-pointwise f L1 L2)
	(cond
	 ((null? L1) '())
	 ((null? L2) '())
	 (else (gl-cons (f (ge-car L1) (ge-car L2))
								 (gl-pointwise f (ge-cdr L1) (ge-cdr L2))))))
(define gl-fibs 
	(gl-cons 1 (gl-cons 1 (gl-pointwise + gl-fibs (ge-cdr gl-fibs)))))
(define (gl-n-th L n)
	(if (positive? n) (gl-n-th (ge-cdr L) (- n 1)) (ge-car L)))
