;;  rrala.scm  jpt4  20150814

(define (rrala-cell id)
	(let act ([role 000]
						[recv '(0 0 0 0 0 0)]
						[send '(0 0 0 0 0 0)]
						[pals '(x x x x x x)])
		(lambda (msg)
			(case msg
				[000 (lambda () role)]
				[001 (lambda (val) (act val recv send pals))]
				[else 'exit]
				))))

(define rr0 (rrala-cell 0))
					
					
						 
						 
