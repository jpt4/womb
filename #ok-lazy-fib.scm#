;http://okmij.org/ftp/Scheme/lazy-Fibonacci.txt

(define (pointwise f L1 L2)
	(let ((L1 (force L1)) (L2 (force L2)))
		(cond
		 ((null? L1) '())
		 ((null? L2) '())
		 (else (cons (f (car L1) (car L2))
								 (delay (pointwise f (cdr L1) (cdr L2))))))))

(define fibs (cons 1 (cons 1 (delay (pointwise + fibs (cdr fibs))))))

; Give the n-th element of a lazy list
(define (n-th L n)
	(let ((L (force L)))
		(if (positive? n) (n-th (cdr L) (- n 1))
				(car L))))
