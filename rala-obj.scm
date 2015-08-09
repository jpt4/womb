;;  rala-obj.scm  jpt4  20150809  
(define (rala-cell id)
	(let ([role 'stem]
        [n-in '()]
        [n-out '()]
				[e-in '()]
        [e-out '()]
				[s-in '()]
        [s-out '()]
				[w-in '()]
        [w-out '()]
				[logic-in '()]
        [logic-out '()]
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
        ['set-logic-in (lambda (val) (set! logic-in val))]
        ['set-logic-out (lambda (val) (set! logic-out val))]
        ['act (lambda ()
								(case role
									['stem (set-default-state)]
                  ['nand 
									 (set! logic-out (nand (car logic-in) (cadr logic-in)))]
									))]))))
                  
(define (set-default-state)
  (set! n-in '())
  (set! n-out '())
	(set! e-in '())
	(set! e-out '())
	(set! s-in '())
	(set! s-out '())
	(set! w-in '())
	(set! w-out '())
  (set! logic-in '()
  (set! logic-out '()
)

(define r1 (rala-cell 1))
