;;mdnn.scm
;;jpt4
;;UTC20160222
;;m-dimensional, n neighbor (variable neighborhood size) clustering, with
;;simple majority decision function

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
(define (tag td) (car td))
(define (data td) (cadr td))

;;helper functions
(define (foldr kons knil lst)
	(if (null? lst)
			knil
			(foldr kons (kons (car lst) knil) (cdr lst))))
(define (zip lsa lsb)
	(map (lambda (e) (list (list-ref lsa e)
												 (list-ref lsb e)))
			 (iota (length lsa))))
;;is val a tag in the tagged data list?
(define (tag-member val ls)
	(cond
	 [(null? ls) #f]
	 [(equal? (caar ls) val) ls]
	 [(not (equal? (caar ls) val)) (tag-member val (cdr ls))]))
;;replace data in tagged data list at tag ref with new data val.
(define (update-data ref val ls)
	(let ([new-element (list (car (list-ref ls ref)) val)])
		(map (lambda (d) (if (equal? (car d) ref) 
												 new-element
												 d))
				 ls)))
(define (data-at-tag tag ls)
	(cadr (tag-member tag ls)))
(define (tag-max tls)
	(let loop ([ls tls] [max '(_ 0)])
		(cond
		 [(null? ls) max]
		 [(> (data max) (data (car ls))) (loop (cdr ls) (car ls))]
		 [else (loop (cdr ls) max)])))
			
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
	(let* ([polity (nearest n nls po)]
				 [ballot (let tally ([ls nls] [acc '()])
									 (cond
										[(null? ls) acc]
										[(tag-member (cat (car ls)) acc) 
										 (tally (cdr ls) (update-data 
																			(cat (car ls))
																			(+ (data-at-tag (cat (car ls)) ls) 1)
																			acc))]
										[else (tally (cdr ls) (cons (list (cat (car ls)) 1) acc))]
										))])
		(tag (tag-max ballot))))

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

