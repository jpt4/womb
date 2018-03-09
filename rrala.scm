;;  rrala.scm  jpt4  20150814


					;0 stem 1 gate 10 pass 11 font 100 sink 
					; 


(define (rrala-cell id)
  (let* ([role 000]  ;; stem gate pass font sink
	 ;; left to right, top to bottom
	 [recv '((c . 0) (a . 0) (b . 0) (n . 0) (n . 0) (n . 0))]  
         ;; left to right, top to bottom
	 [send '((n . 0) (n . 0) (n . 0) (c . 0) (a . 0) (b . 0))]
	 [pals '(x x x x x x)] ;; left to right, top to bottom
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
		  (let* ([out (fredkin (cdr (assq 'c recv))
				       (cdr (assq 'a recv))
				       (cdr (assq 'b recv)))]
			 
			 
			 []
			 ))]) 
		
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
	 
	 
	 
	 
