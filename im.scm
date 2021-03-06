;;im.scm
;;20180307Z
;;jpt4
;;Chez Scheme v9.5
;;another's thought experiment

(define (im mk-id)
  (define id mk-id)
  (define c-ls '()) ;'((tarn durn resn) (tar(n-1) dur(n-1) res(n-1)) ...)
  (define dlk #f) ;deadlock
  (define (get-id) id)
  (define (get-c-ls) c-ls)
  (define (get-dlk) dlk)
  (define (get-status) (list id c-ls dlk))
  (define (set-id new-id) (set! id new-id))
  (define (set-c-ls new-c-ls) (set! c-ls new-c-ls))
  (define (set-dlk new-dlk) (set! dlk new-dlk))
  (define (dec) 
    (set-c-ls (filter (lambda (c) (and (not (null? (cadr c)))
				       (> (cadr c) 0)))
		      (map (lambda (c)
			     (cons (car c) 
				   (cons (sub1 (cadr c))
					 (cddr c))))
			   c-ls))))			 
  (define (enter new-c)
    (if (valid-to-enter? new-c c-ls)
	(begin (set-c-ls (if (null? new-c)
			     c-ls
			     (cons new-c c-ls)))
	       (set-dlk #f))
	(set-dlk #t)))
  (lambda (msg . args)
    (case msg
      [(get-id) (get-id)]
      [(get-c-ls) (get-c-ls)]
      [(get-dlk) (get-dlk)]
      [(get-status) (get-status)]
      [(set-id) (set-id (car args))]
      [(set-c-ls) (set-c-ls (car args))]
      [(set-dlk) (set-dlk (car args))]
      [(dec) (dec)]
      [(enter) (enter (car args))]
      [else 'unknown-im-msg])))

(define (valid-to-enter? new-c c-ls)
  (let* ([tar (car new-c)] ;target
	 [res ;restraints
	  (fold-right
	   (lambda (a b) (append (cddr a) b))
	   '() c-ls)])
    (if (member tar res)
	#f
	#t)))

;;soc
(define (soc size)
  (define (get-size) size)
  (define i-ls (map (lambda (i) (im i)) (iota size)))
  (define (get-i-ls) i-ls)
  (define (get-nth-i i) (list-ref i-ls i))
  #;(define (pass-msg i m) (displaynl i) (displaynl m)) ;debug variant
  (define (pass-msg i m) ;pass message m to i (addressed by index)
    (let ([im (list-ref i-ls i)])
      (if (eq? (length m) 1)
	  (im (car m))
	  (im (car m) (cadr m)))))
  (define (update) (for-each (lambda (i) (i 'dec)) i-ls))
  (define (report) (map (lambda (i) (i 'get-status)) i-ls))
  (lambda (msg . args)
    (case msg
      [(get-size) (get-size)]
      [(get-i-ls) (get-i-ls)]
      [(get-nth-i) (get-nth-i (car args))]
      [(pass-msg) (pass-msg (car args) (cdr args))]
      [(update) (update)]
      [(report) (report)]
      [else 'unknown-soc-msg]
      )))

;;sim - pass already created soc, p-ent as 0.75 = 75%
(define (sim soc steps p-ent)
  (let ([size (soc 'get-size)])
    (let loop ([t steps] [log '((init))])
      (if (> t 0)
	  (begin
	    (if (< (random 1.0) p-ent) ;potentially gain new con
		(soc 'pass-msg (random size) 'enter (mk-con size steps)))
	    (soc 'update) ;update all cons
	    ;(displaynl `(t=,t soc=,(soc 'report))) ;display end-of-step status
	    (loop (sub1 t) (append log (list `(t=,t soc=,(soc 'report))))))
	    ;(displaynl `(t=,t soc=,(soc 'report)))
	  (append log (list `(t=,t soc=,(soc 'report))))))))
	  
(define (mk-con size steps)
  (cons (random size) ;tar
	(cons (random steps) ;dur
	      (random-w/o-replacement (random (add1 size)) (iota size))))) ;res

;;slv evaluation - ubiquitous, continuous, eternal deadlock potential

;The following do not obey common, consistent, set, list, or multi-set
;semantics. For example, list-difference deduplicates repeated values
;from l1 or l2, while list-intersection does not. The unwritten 
;(list-union) would be simply (append).
(define (list-difference l1 l2) ;in l1 and not in l2
  (filter (lambda (a) (not (member a l2))) l1))
(define (list-intersection l1 l2)
  (filter (lambda (a) (member a l2)) l1))

(define (random-w/o-replacement num ls)
  (let loop ([n num] [l ls] [acc '()])
    (if (> n 0)
	(let ([piv (random (length l))])
	  (loop (sub1 n) 
		(append (list-head l piv) (list-tail l (add1 piv))) 
		(cons (list-ref l piv) acc)))
	acc)))

(define (log-entry step log) (list-ref log (add1 step)))
(define (final-soc-state soc-log) (log-entry (- (length soc-log) 2) soc-log))
(define (soc-in-log-entry log-entry) (cadddr log-entry))
(define (im-in-log-entry log-entry im-id) 
  (list-ref (soc-in-log-entry log-entry) im-id))

(define (exclusive-slv-con? im-id con t-now pop) ;slv via one con
  (let* ([tar (car con)] [dur (cadr con)] [res-ls (cddr con)]
	 [non-res-ls (list-difference (iota pop) res-ls)])
    (and 
     ;(not (eq? tar im-id)) ;con cannot be with self
     (> dur t-now) ;past the time horizon
     (or (null? non-res-ls) ;entire pop
	 (null? (list-difference `(,im-id) non-res-ls)) ;entire pop less self
	 (null? (list-difference `(,tar) non-res-ls)) ;entire pop less tar
	 (null? (list-difference 
		 `(,im-id ,tar) non-res-ls)) ;entire pop less self and tar
	 ))))

(define (combined-slv-con? im-id c-ls t-now pop) ;slv via con combination
  (let* ([combined-res-ls (append (map (lambda (c) (if (> (cadr c) t-now) 
						       (cddr c)
						       '()))
				       c-ls))]
	 [non-res-ls (list-difference (iota pop) combined-res-ls)])
    (or (null? non-res-ls) (null? (list-difference `(,im-id) non-res-ls)))
    ))

(define (im-slv? im t-now pop) ;im printed in status-list form
  (let ([im-id (car im)] [c-ls (cadr im)])
    (or (ormap (lambda (con) (exclusive-slave-con? im-id con t-now pop)) c-ls)
	(combined-slv-con? im-id c-ls t-now pop))))

(define (slvs-in-log-entry log-entry max-steps pop) 
  (letrec* ([t-now (cadr log-entry)]
	    [im-ls (cadddr log-entry)]
	    [im-ls-with-c-ls (filter (lambda (a) (not (null? (cadr a)))) im-ls)])
    (filter (lambda (a) 
	      (im-slv? a max-steps t-now pop))
	    im-ls-with-c-ls)))

;;tests
(define (displaynl d) (begin (display d) (newline)))
(define (displaynl* d . res)
  (if (null? res)
      (displaynl d)
      (for-each displaynl (cons d res))))

#| Sample simulation test
(define soc0 (soc 100))
(define soc-log (sim soc0 100 0.5))
|#

#|
Notes
APL in Scheme:
>
(map length (map (lambda (t) (filter (lambda (a) (eq? a t)) (sort < (map length (map (lambda (a) (mk-con 10 12)) (iota 10000))
)))) (iota 13)))
=>
(0 0 960 886 894 900 914 940 869 956 893 933 914)

Improvements, alterations
Reverse t log to show time passed, not time remaining.
Record types instead of lists.
|#

#;(define (run-tests) ;FIXME not displaying in correct order
  (begin
    (define imtst (im 0))
    (displaynl* 
     (imtst 'get-id)
     (imtst 'get-c-ls)
     (imtst 'get-dlk)
     (imtst 'set-id 1)
     (imtst 'set-c-ls '((2 3 4)))
     (imtst 'set-dlk #t)
     (imtst 'get-id)
     (imtst 'get-c-ls)
     (imtst 'get-dlk) 
     (imtst 'enter '(1 4 1))
     (imtst 'get-c-ls)
     (imtst 'set-dlk #f)
     (imtst 'get-dlk)
     (imtst 'enter '(1 2 1))
     (imtst 'get-c-ls)
     (imtst 'get-dlk)
     (imtst 'dec)
     (imtst 'get-c-ls)
    )))
