;;2dnn.scm
;;jpt4
;;UTC20160222
;;2-dimensional, n neighbor clustering, with simple majority decision function

;;point=(value, category, (dimension list))

;;XX - NOT ENOUGH DATA
;;training point list - known category
(define 3d-training (list
										 '(val cata (0.1 0.7 0.2))
										 ;...
										 ))

;;testing points - unknown category
(define testing (list
								 '(vally _ 0.2 0.8)
								 '(vallz _ 0.8 0.4)
								 ;...
								 ))

;;point carving
(define (val p) (car p))
(define (cat p) (cadr p))
(define (dls p) (cddr p))
(define (n-co p n) (list-ref (cddr p) n))

;;helper functions
(define (foldr kons knil lst)
	(if (null? lst)
			knil
			(foldr kons (kons (car lst) knil) (cdr lst))))
(define (zip lsa lsb)
	(map (lambda (e) (list (list-ref lsa e)
												 (list-ref lsb e)))
			 (iota (length lsa))))

;;n-dimensional distance between points
(define (nd-euclidean p1 p2)
	(expt (foldr (lambda (zp rt) (+ (expt (- (car zp) (cadr zp)) 2) rt))
							 (zip (dls p1) (dls p2)) 0)
				0.5))

;;calulate nearest n neighbors among list to point
(define (nearest n nls po)
	(list-head n (sort (map (lambda (p) (cons p (nd-euclidean p po))) nls) 
										 (lambda (l r) (< (cadr l) (cadr r))))))

;;determine category of a point based on its nearest n neighbors
(define (infer-category n nls po)
	(let* ([polity (nearest n nls po)] [red (cat (car nls))]
				 [blue (car (filter (lambda (c) (not (equal? (cat c) red))) nls))]
				 [red-count (length (filter (lambda (c) (equal? (cat c) red)) nls))]
				 [blue-count (- (length nls) red-count)])
		(cond
		 [(equal? red-count blue-count) 'borderline]
		 [(> red-count blue-count) red]
		 [else blue])))

;;classify a single point with unknown category by comparison with nearest
;;n neighbors
(define (classify n nls po)
	(cons* (val po) (infer-category n nls po) (dls po)))

;;classify a list of unknown points according to n nearest neighbors
(define (classify* n nls pls)
	(map (lambda (po) (classify n nls po)) pls))

;;XX - INSUFFICIENT TESTS
;;test suite
;;a list of (expression, expected result) pairs.
(define tests (list
							 '((n-co 1 '(12 left 0.3 0.2)) 0.3)
							 '((infer-category 1 training (car testing)) cata)
							 '((classify* 1 training testing) 
								 ((vally cata 0.2 0.8) (vallz cata 0.8 0.4)))
							 ))

(define (run-tests)
	(for-each (lambda (t)  (test (car t) (cadr t))) tests))

(define (test exp res)
	(if (equal? (eval exp (interaction-environment)) res)
			(display "Passed")
			(begin (display exp) (display " ") (display res)))
	(newline))

