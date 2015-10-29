;;zzstruct.scm
;;jpt4
;;UTC20151028

#| 

A ZZ structure (zzstruct) is an indexed collection of cells and their
neighbor list. A cell is a generic container for arbitrary content,
formed by appending the tag "cell" to the list of a cell's index in
the zztruct and the symbol which names the content referent,
e.g. (cell 0 c0). A neighbor list is a list of ordered pairs
identifying the positive/upstream (left element), and
negative/downstream (right element) neighbors of a cell along each
dimensional axis. All cells are neighbors along the fundamental
indexical axis, with positive and negative ordering implicit,
corresponding to the numerical order of the cell indices. The neighbor
lists of all cells contain an ordered pair (even if the empty pair (0
0)) for each axis inhabited by any cell, thus maintaining length
parity between neighbor lists.

|#

(define (zz-mk cls)
	(zz-mk-aux cls))

(define (zz-mk-aux cls)
	(zz-mk-acc cls '())
	;(zz-mk-direct cls)
)

(define (zz-mk-direct cls)
	(cond
	 [(null? cls) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty
	 [(zz-validate cls)]))

(define (zz-mk-acc cls acc)
	(cond
	 [(and (null? cls) (null? acc)) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty
	 [(null? cls) acc]
	 [(cell-record? (car cls)) 
		(zz-mk-acc (cdr cls) (list acc (car cls)))]))

(define (cell-record? cr)
	(and (cell? (car cr)) (and-map cell-ordered-pair? (cdr cr)))
)

(define (cell? cl)
	(and (eq? (car cl) 'cell) (number? (cadr cl)) (symbol? (caddr cl)))
)

(define (cell-ordered-pair? cp)
	(and (ordered-pair? cp)http://tineye.com/search/39ebe6a4ae18a04f4822ce067bc8d424fa70495b/File Edit Options Buffers Tools Scheme Help
			 ;;zzstruct.scm                                                                                                                                                |;;; /home/jpt4/code/womb/zzstruct.scm:44:30: warning: possibly unbound variable `cell-pair?'
			 ;;jpt4                                                                                                                                                        |;;; compiled /home/jpt4/.cache/guile/ccache/2.2-LE-8-3.6/home/jpt4/code/womb/zzstruct.scm.go
			 ;;UTC20151028                                                                                                                                                 |scheme@(guile-user) [11]> (pair? '(1 2 3 4 5))
			                                                                                                                                                               |$26 = #t
#|                                                                                                                                                            |scheme@(guile-user) [11]> (length '(1 2 3))
                                                                                                                                                              |$27 = 3
																																																																															A ZZ structure (zzstruct) is an indexed collection of cells and their                                                                                         |scheme@(guile-user) [11]> ,a length
neighbor list. A cell is a generic container for arbitrary content,                                                                                           |(guile): integer-length #<procedure integer-length (_)>
formed by appending the tag "cell" to the list of a cell's index in                                                                                           |(guile): stack-length #<procedure stack-length (_)>
the zztruct and the symbol which names the content referent,                                                                                                  |(guile): string-prefix-length #<procedure string-prefix-length (_ _ #:optional _ _ _ _)>
e.g. (cell 0 c0). A neighbor list is a list of ordered pairs                                                                                                  |(guile): hostent:length #<procedure hostent:length (obj)>
identifying the positive/upstream (left element), and                                                                                                         |(guile): string-suffix-length-ci  #<procedure string-suffix-length-ci (_ _ #:optional _ _ _ _)>
negative/downstream (right element) neighbors of a cell along each                                                                                            |(guile): bitvector-length #<procedure bitvector-length (_)>
dimensional axis. All cells are neighbors along the fundamental                                                                                               |(guile): length #<procedure length (_)>
indexical axis, with positive and negative ordering implicit,                                                                                                 |(guile): string-prefix-length-ci  #<procedure string-prefix-length-ci (_ _ #:optional _ _ _ _)>
corresponding to the numerical order of the cell indices. The neighbor                                                                                        |(guile): vector-length  #<procedure vector-length (_)>
lists of all cells contain an ordered pair (even if the empty pair (0                                                                                         |(guile): array-length #<procedure array-length (_)>
0)) for each axis inhabited by any cell, thus maintaining length                                                                                              |(guile): string-suffix-length #<procedure string-suffix-length (_ _ #:optional _ _ _ _)>
parity between neighbor lists.                                                                                                                                |(guile): string-length  #<procedure string-length (_)>
                                                                                                                                                              |scheme@(guile-user) [11]> ,a make-array
																																																																															|#                                                                                                                                                            |(guile): make-array #<procedure make-array (_ . _)>
																																																																															                                                                                                                                                              |scheme@(guile-user) [11]> (array-type (make-array 1 2))
(define (zz-mk cls)                                                                                                                                           |$28 = #(1)
  (zz-mk-aux cls))                                                                                                                                            |scheme@(guile-user) [11]> (pair? (make-array 1 2))
	|$29 = #(1 1)
(define (zz-mk-aux cls)                                                                                                                                       |scheme@(guile-user) [11]> (pair? (make-array 1 2))
  (zz-mk-acc cls '())                                                                                                                                         |$30 = #f
																				;(zz-mk-direct cls)                                                                                                                                         |scheme@(guile-user) [11]> ,a array?
	)                                                                                                                                                             |(guile): module-obarray-ref #<procedure module-obarray-ref (ob key)>
                                                                                                                                                              |(guile): array? #<procedure array? (_)>
(define (zz-mk-direct cls)                                                                                                                                    |(guile): set-module-obarray!  #<procedure 19eba20 at ice-9/boot-9.scm:2245:2 (module val)>
  (cond                                                                                                                                                       |(guile): shared-array-root  #<procedure shared-array-root (_)>
	[(null? cls) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty                                                                 |(guile): array-map-in-order!  #<procedure array-map-in-order! (_ _ . _)>
	   [(zz-validate cls)]))                                                                                                                                      |(guile): module-obarray-remove! #<procedure module-obarray-remove! (ob key)>
                                                                                                                                                              |(guile): array-copy!  #<procedure array-copy! (_ _)>
(define (zz-mk-acc cls acc)                                                                                                                                   |(guile): array-fill!  #<procedure array-fill! (_ _)>
  (cond                                                                                                                                                       |(guile): array-map! #<procedure array-map! (_ _ . _)>
	[(and (null? cls) (null? acc)) (list '((cell 0 0) (0 0)))] ;default zzstruct - a zzstruct can never be empty                                               |(guile): make-typed-array #<procedure make-typed-array (_ _ . _)>
	   [(null? cls) acc]                                                                                                                                          |(guile): typed-array? #<procedure typed-array? (_ _)>
   [(cell-record? (car cls))                                                                                                                                  |(guile): transpose-array  #<procedure transpose-array (_ . _)>
	     (zz-mk-acc (cdr cls) (list acc (car cls)))]))                                                                                                             |(guile): array-type #<procedure array-type (_)>
                                                                                                                                                              |(guile): make-obarray #<procedure make-obarray ()>
(define (cell-record? cr)                                                                                                                                     |(guile): obarray-for-each #<procedure obarray-for-each (_ #:optional _)>
  (and (cell? (car cr)) (and-map cell-ordered-pair? (cdr cr)))                                                                                                |(guile): array-type-code  #<procedure array-type-code (_)>
	)                                                                                                                                                             |(guile): shared-array-offset  #<procedure shared-array-offset (_)>
                                                                                                                                                              |(guile): shared-array-increments  #<procedure shared-array-increments (_)>
(define (cell? cl)                                                                                                                                            |(guile): array-copy-in-order! #<procedure array-copy-in-order! (_ _)>
  (and (eq? (car cl) 'cell) (number? (cadr cl)) (symbol? (caddr cl)))                                                                                         |(guile): module-obarray-set!  #<procedure module-obarray-set! (ob key val)>
	)                                                                                                                                                             |(guile): array-length #<procedure array-length (_)>
                                                                                                                                                              |(guile): array-contents #<procedure array-contents (_ #:optional _)>
(define (cell-ordered-pair? cp)                                                                                                                               |(guile): array-equal? #<procedure array-equal? (#:optional _ _ . _)>
  (and (ordered-pair? cp)http://tineye.com/search/39ebe6a4ae18a04f4822ce067bc8d424fa70495b/                                                                   |(guile): array-rank #<procedure array-rank (_)>
	)                                                                                                                                                             |(guile): array->list  #<procedure array->list (_)>
                                                                                                                                                              |(guile): array-for-each #<procedure array-for-each (_ _ . _)>
(define (ordered-pair? op)                                                                                                                                    |(guile): module-obarray #<procedure 19eba40 at ice-9/boot-9.scm:2245:2 (module)>
  (eq? (length op) 2) op)                                                                                                                                     |(guile): make-array #<procedure make-array (_ . _)>
	)                                                                                                                                                             |(guile): module-import-obarray  #<procedure 19eb880 at ice-9/boot-9.scm:2245:2 (module)>
                                                                                                                                                              |(guile): array-index-map! #<procedure array-index-map! (_ _)>
(define (atom? a)                                                                                                                                             |(guile): module-obarray-get-handle  #<procedure module-obarray-get-handle (ob key)>
  (or (symbol? a) (number? a))                                                                                                                                |(guile): array-shape  #<procedure array-shape (a)>
	)                                                                                                                                                             |(guile): list->typed-array  #<procedure list->typed-array (_ _ _)>
                                                                                                                                                              |(guile): list->array  #<procedure list->array (_ _)>
(define (zz-validate zs)                                                                                                                                      |(guile): %get-pre-modules-obarray #<procedure %get-pre-modules-obarray ()>
  (and (and-map cell-record? zs)                                                                                                                              |(guile): array-set! #<procedure array-set! (_ _ #:optional _ _ . _)>
	       (and-map (lambda (ze) (eq? (length (car zs)) (length ze))) zs))                                                                                        |(guile): make-shared-array  #<procedure make-shared-array (_ _ . _)>
)                                                                                                                                                             |(guile): array-in-bounds? #<procedure array-in-bounds? (_ . _)>
                                                                                                                                                              |(guile): array-dimensions #<procedure array-dimensions (_)>
(define (zz-generate-random) 'gr)                                                                                                                             |(guile): array-ref  #<procedure array-ref (_ #:optional _ _ . _)>
                                                                                                                                                              |scheme@(guile-user) [11]> (array-type-code (make-array 1 2))
(define (test)                                                                                                                                                |$31 = #t
  (zz-mk '())                                                                                                                                                 |scheme@(guile-user) [11]> (array-rank (mak
)

(define (ordered-pair? op)
	(eq? (length op) 2) op)
)

(define (atom? a)
	(or (symbol? a) (number? a))
)

(define (zz-validate zs)
	(and (and-map cell-record? zs) 
			 (and-map (lambda (ze) (eq? (length (car zs)) (length ze))) zs))
)

(define (zz-generate-random) 'gr)

(define (test)
	(zz-mk '())
	(zz-mk (zz-generate-random)))
