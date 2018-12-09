;;2dkm.scm
;;jpt4
;;UTC20160221
;;2-dimensional, single nearest neighbor k-means clustering

;;point=(value, category, x-dimension, y-dimension)

;;training point list - known category
(define training (list
									'(val cata 0.1 0.7)
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
(define (x-co p) (caddr p))
(define (y-co p) (cadddr p))

;;2-dimensional distance between points
(define (2d-euclidean p1 p2)
	(expt (+ (expt (- (x-co p1) (x-co p2)) 2)
					 (expt (- (y-co p1) (y-co p2)) 2))
				0.5))

;;calulate nearest neighbor among list to point
(define (nearest nls po)
	(caar (sort (map (lambda (p) (cons p (2d-euclidean p po))) nls) 
						 (lambda (l r) (< (cadr l) (cadr r))))))

(define (infer-category nls po)
	(cat (nearest nls po)))

;;classify a single point with unknown category
(define (classify nls po)
	(list (val po) (infer-category nls po) (x-co po) (y-co po)))

;;classify a list of unknown points
(define (classify* nls pls)
	(map (lambda (po) (classify nls po)) pls))

;;test suite
;;a list of (expression, expected result) pairs.
(define tests (list
							 '((x-co '(12 left 0.3 0.2)) 0.3)
							 '((infer-category training (car testing)) cata)
							 '((classify* training testing) 
								 ((vally cata 0.2 0.8) (vallz cata 0.8 0.4)))
							 ))

(define (run-tests)
	(for-each (lambda (t)  (test (car t) (cadr t))) tests))

(define (test exp res)
	(if (equal? (eval exp (interaction-environment)) res)
			(display "Passed")
			(begin (display exp) (display " ") (display res)))
	(newline))
			
		 


