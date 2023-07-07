#lang racket

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


(define (merge-lists l1 l2) (remove-duplicates (append l1 l2)))

(define (directed-next-layer graph root)

		    (let ([v (first graph)] [e (second graph)])
		      (map second (filter (lambda (a) (equal? (first a) root))
					  e))))

(define (spanning-tree graph)
  (let loop ([g graph] [acc '()])
    (let ([v (first g)] [e (second g)] [root (car v)] [g' (next-layer g root)])
      (cond
       [(empty? g) #t]
       
    
    

;(define (spanning-tree graph)
;  (let ([v0 (car (list-ref graph (random (length graph))))])
 ;   (let loop ([v v0] [g graph])))
	       
