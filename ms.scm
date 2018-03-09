;;ms.scm
;;jpt4
;;UTC20160218
;;random martial technique sequences for unbiased training

;;n random lists of length m, drawn from the elements of list l
;;separated by tokens
(define (randseq ls size rep tok)
  (map (lambda (a) 
	 (cons tok
	       (map (lambda (b) 
		      (list-ref ls (random (length ls)))) 
		    (iota size))
	       ))                              
       (iota rep)))

;;nested random lists, each level drawn from the one beneath it.
;;size and breadth of each level independent
(define (randseq* ls sizels repls tokls)
  (cond 
   [(null? sizels) ls] 
   [else (randseq* (randseq ls (car sizels) (car repls) (car tokls)) 
		   (cdr sizels) (cdr repls) (cdr tokls))]))


