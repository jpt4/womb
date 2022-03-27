;; cards.rkt
;; 20220327Z
;; jpt4
;; card join code

(require racket/match)

;; card representation
; {0,1}^4
; '(0 0 0 0) ... '(1 1 1 1)

;; generate random card
(define (random-card)
  (list (random 0 2) (random 0 2) (random 0 2) (random 0 2)))

;; can be joined at all
(define (join-cards? c1 c2)
  (match (append c1 c2)
    [(list 1 _ _ _ _ _ 1 _) #t]
    [(list _ 1 _ _ _ _ _ 1) #t]
    [(list _ _ 1 _ 1 _ _ _) #t]
    [(list _ _ _ 1 _ 1 _ _) #t]
    [_ #f]))

;; can be joined along a certain edge
(define (join-cards-at? c1 c2 side)
  (let ([c (append c1 c2)])
    (match side
      ['n (match c [(list 1 _ _ _ _ _ 1 _) #t] [_ #f])]
      ['e (match c [(list _ 1 _ _ _ _ _ 1) #t] [_ #f])]
      ['s (match c [(list _ _ 1 _ 1 _ _ _) #t] [_ #f])]
      ['w (match c [(list _ _ _ 1 _ 1 _ _) #t] [_ #f])]
      [_ #f])))

;; auxiliary functions

;; maximum binary exponent
(define (max-e a)
  (cond
   [(zero? a) 0]
   [(= 1 a) 0]
   [(< (/ a 2.0) 1) 0]
   [(>= (/ a 2.0) 1) (+ 1 (max-e (/ a 2.0)))]))

;; convert decimal numbers to binary representation in list form
(define (dec->binls dec)
  (let loop ([d dec] [ls '()] [e (max-e dec)])
    (cond
     [(zero? e) (append ls (if (>= (- d (expt 2 e)) 0)
			       '(1)
			       '(0)))]
     [else (if (>= (- d (expt 2 e)) 0)
	       (loop (- d (expt 2 e)) (append ls '(1)) (- e 1))
	       (loop d (append ls '(0)) (- e 1)))])))

;; convert numbers in binary list representation to decimal
(define (binls->dec bls)
  (cond
   [(= (length bls) 1) (* (car bls) 1)]
   [else (+ (* (car bls) (expt 2 (- (length bls) 1)))
	    (binls->dec (cdr bls)))]))
    
