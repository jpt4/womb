;;mutate.scm
;;20180424Z
;;name mutation, to free your true one.

(define (string->ascii-num-list s)
  (map char->integer (string->list s)))

;All basic mutation definitions should be exploratorily operable,
;accepting and emitting raw numerical ascii, normalizing internally as
;needed.

(define maior (map (lambda (a) (+ 65 a)) (iota 26)))
(define minor (map (lambda (a) (+ 97 a)) (iota 26)))
(define maiorspace (cons 32 maior))
(define minorspace (cons 32 minor))
(define alphaspace (cons 32 (append maior minor)))

;assume clean initial base pair
(define (point-mutation bp)
  (if (member bp alphaspace)
      (list-ref alphaspace (random 54))))

(define (point-mutation-preserve-case bp)
  (cond 
   [(member bp minor) minor]
   [(member bp maior) maior]
   [(eq? bp 32) ])
            (random 26)))

;I reserve the write to interpret no-ops as I see fit.
(define (corrupt? bp) (not (member bp ascii)))


  
      
      
                     
