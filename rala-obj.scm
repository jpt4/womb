;;  rala-obj.scm  jpt4  20150809  
(define (test id)
	(let ([role 'stem]
				[ni 'null]
        [no 'null]
        [ei 'null]
				[eo 'null]
				[li 'null]
				[lo 'null]
				)
		(lambda (msg)
			(case msg
				['id? (lambda () id)]
				['id! (lambda (val) (set! id val))]
				['role? (lambda () role)]
				['role! (lambda (val) (set! role val))]
				['ni? (lambda () ni)]
				['ni! (lambda (val) (set! ni val))]
				['no? (lambda () no)]
				['no! (lambda (val) (set! no val))]
				['eo? (lambda () eo)]
				['eo! (lambda (val) (set! eo val))]
				['li? (lambda () li)]
				['li! (lambda (val) (set! li val))]
				['lo? (lambda () lo)]
				['lo! (lambda (val) (set! lo val))]
				['act (lambda ()
								(case role
									['stem (set-default-state)]
                  ['nand 
									 (set! lo (lognand (car lo) (cadr li)))]
									))]
))))

(define t1 (test 1))

;not working
(define (test-suite)

		((t1 'id?))
		((t1 'id!) 2)
		((t1 'id?))
		((t1 'role?))
		((t1 'role!) 'nand)
		((t1 'role?))
)

#|
why?

> (define ro `',(string->symbol (string-append (symbol->string 'role) "?")))
> ro
'role?
> ((t1 ro))
Exception: attempt to apply non-procedure #<void>
Type (debug) to enter the debugger.
> ((t1 'role?))
stem
> ro
'role?

|#	
	
(define (rala-cell id)
	(let* ([role 'stem]
				 [n-in 'null]
				 [n-out 'null]
				 [e-in 'null]
				 [e-out 'null]
				 [s-in 'null]
				 [s-out 'null]
				 [w-in 'null]
				 [w-out 'null]
				 [logic-in 'null]
				 [logic-out 'null]
				 )
		(lambda (msg)
      (case msg
				['get-id (lambda () id)]
        ['get-role (lambda () role)]
        ['set-role (lambda (val) (set! role val))]
				['get-n (lambda () n-out)]
				['set-n (lambda (val) (set! n-in val))]
        ['get-e (lambda () e-out)]
				['set-e (lambda (val) (set! e-in val))]
        ['get-s (lambda () s-out)]
				['set-s (lambda (val) (set! s-in val))]
				['get-w (lambda () w-out)]
				['set-w (lambda (val) (set! w-in val))]
        ['get-logic-in (lambda () logic-in)]
        ['set-logic-in (lambda (val) (set! logic-in val))]
        ['get-logic-out (lambda () logic-out)]
        ['set-logic-out (lambda (val) (set! logic-out val))]
        ['act (lambda ()
								(case role
									['stem (set-default-state)]
                  ['nand 
									 (set! logic-out (lognand (car logic-in) (cadr
logic-in)))] ))])))) #;(define (set-default-state)
  (set! n-in 'null)
  (set! n-out 'null)
	(set! e-in 'null)
	(set! e-out 'null)
	(set! s-in 'null)
	(set! s-out 'null)
	(set! w-in 'null)
	(set! w-out 'null)
  (set! logic-in 'null)
  (set! logic-out 'null)
)

(define (lognand a b)
	(lognot (logand a b)))

(define r1 (rala-cell 1))

(define (test-r1)
	(begin
		((r1 'set-role) 'nand)
		((r1 'set-logic-in) '(n-in e-in))
		((r1 'set-logic-out) '(n-out))
		((r1 'set-n) 1)
		((r1 'set-e) 0)
		((r1 'act))
		))
