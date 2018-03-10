;;im.scm
;;20180307Z
;;jpt4
;;Chez Scheme v9.5
;;another's thought experiment

(define (im mk-id)
  (define id mk-id)
  (define c-ls '()) ;'((tar0 dur0 res0) (tar1 dur1 res1) ...)
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
    (let loop ([t steps])
      (if (> t 0)
	  (begin
	    (if (< (random 1.0) p-ent) ;optionally gain new con
		(soc 'pass-msg (random size) 'enter (mk-con size steps)))
	    (soc 'update) ;update all cons
	    (displaynl `(t=,t soc=,(soc 'report))) ;display end-of-step status
	    (loop (sub1 t)))
	   (displaynl `(t=,t soc=,(soc 'report)))))))
	  
(define (mk-con size steps)
  (cons (random size) ;tar
	(cons (random steps) ;dur
	      (map (lambda (a) (random size)) (iota (random size)))))) ;res

;;tests
(define (displaynl d) (begin (display d) (newline)))
(define (displaynl* d . res)
  (if (null? res)
      (displaynl d)
      (for-each displaynl (cons d res))))

#| Sample simulation test
(define soc0 (soc 100))
(sim soc0 100 0.5)
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
