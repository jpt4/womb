;;  rrala.scm  jpt4  20150814

(define (rrala-cell id)
	(let* ([role 000]  ;; stem gate pass font sink
						[recv '(0 0 0 0 0 0)]  ;;  left to right, top to bottom
						[send '(0 0 0 0 0 0)]  ;;  left to right, top to bottom
						[pals '(x x x x x x)]  ;;  left to right, top to bottom
						[state (list id role recv send pals)]
						[report (lambda (stat)
											(case stat
												['id id]
												['role role]
												['recv recv]
												['send send]
												['pals pals]
												['state state]
												[else stat]
												))
										]
						[change (lambda (prev curr)
											(case prev
												['id (set! id curr)]
												['role (set! role curr)]
												['recv (set! recv curr)]
												['send (set! send curr)]
												['pals (set! pals curr)]))]
						[act (lambda ()
									 (cond
										[(eq? role 'gate) 
						)
					 (lambda (msg)
						 (case msg
							 ;;  report state
							 [? (lambda (diag) (report diag))]
							 ;;  change state
							 [! (lambda (old new) (change old new))]
							 ;;  
							 [else 'exit]
							 )
						 
						 )))

(define rr0 (rrala-cell 0))
					
					
						 
						 
