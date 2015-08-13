;;  rala-obj.scm  jpt4  20150809  
(define (rala-cell id)
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
        ['ei? (lambda () ei)]
				['ei! (lambda (val) (set! ei val))]
				['eo? (lambda () eo)]
				['eo! (lambda (val) (set! eo val))]
				['li? (lambda () li)]
				['li! (lambda (val) (set! li val))]
				['lo? (lambda () lo)]
				['lo! (lambda (val) (set! lo val))]
				['act (lambda (cell)
								(case role
									['stem (set-default-state)]
                  ['nand
									 (set! lo (lognand ((cell ni ei))]
									))]
				))))

(define r1 (rala-cell 1))

(config-nand r1)

(define (config-nand cell)
	((cell 'role!) 'nand)
	((cell 'li!) '(ni ei))
	((cell 'lo!) '(no))
	((cell 'ni!) 1)
	((cell 'ei!) 0)
		)

(define (lognand a b)
	(lognot (logand a b)))

(define (set-default-state)
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

#|
why?

> (define ro `',(string->symbol (string-append (symbol->string 'role) "?")))
> ro
'role?
> ((cell ro))
Exception: attempt to apply non-procedure #<void>
Type (debug) to enter the debugger.
> ((cell 'role?))
stem
> ro
'role?

|#	
