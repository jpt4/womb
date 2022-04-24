(define (cartesian-product leftls rightls)
  (foldr (lambda (left acc) 
	   (append (map (lambda (right)
			  (list left right))
			rightls)
		   acc))
	 '()	 
	 leftls))

(define (fully-connected-directed-graph verticesls)
  (cartesian-product verticesls verticesls))

(define (fully-connected-non-recurrent-directed-graph verticesls) 
  (filter (lambda (a) (not (eq? (car a) (cadr a)))) 
	  (fully-connected-undirected-graph verticesls)))

(define (fully-connected-undirected-graph verticesls)
  (foldr (lambda (a acc)
	   (if (and (member a acc) (member (reverse a) acc))
	       (filter (lambda (b) (not (eq? (reverse a) b))) acc)
	       acc))
	 (fully-connected-directed-graph verticesls)
	 (fully-connected-directed-graph verticesls)))
	 
(define (random-choose-k-wo-replacement k ls)
  (if (<= k (length ls))
      (foldr
       (lambda (a acc)
	 (append
	  (let loop ([i k] [l ls] [cca '()])
	    (cond
	     [(= i 0) cca]
	     [else (let ([r (list-ref l (random (length l)))])
		     (loop (- i 1) (remove r l) (cons `(,a ,r) cca)))]))
	  acc))
       '()
       ls)
      "err, k greater than size of list"))

(define (spanning-tree graph)
  (let ([v0 (car (list-ref graph (random (length graph))))])
    (let loop ([v v0] [g graph]
	       
