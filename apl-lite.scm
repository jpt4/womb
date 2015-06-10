;20130223
;apl-lite.scm
;Minimal functionality APL implementation in Scheme
;http://www.cs.unm.edu/~williams/cs257/baby-apl.html

(lambda (pred ls)
	(cond ((null? ls) '())
				((pred (car ls))
				 (cons (car ls) (filter pred (cdr ls))))
				(else
				 (filter pred (cdr ls))))))

(define atom-map
	(lambda (proc . args)
		(cond ((null? (car args)) '())
					((pair? (car args))
					 (cons (apply atom-map proc (map car args))
								 (apply atom-map proc (map cdr args))))
					(else
					 (apply proc args)))))

(define promote-scalars
	(lambda (mixed-args)
		(let ((non-scalars (filter pair? mixed-args)))
			(if (null? non-scalars)
					mixed-args
					(let ((template (car non-scalars)))
						(map (lambda (arg)
									 (if (pair? arg)
											 arg
											 (atom-map (lambda (x) arg) template)))
								 mixed-args))))))

(define apl-ize
	(lambda (proc)
		(lambda args
			(let ((foo (promote-scalars args)))
				(cond ((null? foo)
							 (proc))
							((pair? (car foo))
							 (apply atom-map proc foo))
							(else
							 (apply proc args)))))))

(define + (apl-ize +))
(define * (apl-ize *))
(define - (apl-ize -))
(define / (apl-ize /))
