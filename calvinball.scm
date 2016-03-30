;;calvinball.scm
;;jpt4
;;A simple object-oriented implementation of Calvinball.
;;UTC20160330
;;Guile Scheme v2.0+

#|Game definition

Calvinball has no fixed set of rules of play, but it does have a
consistent internal schedule and scoring system.

A game of Calvinball is divided into 13 innings, each composed of an
upper and lower duoquarter (UDQ and ldq, respectively). The home
team (colloquially "Tiger") attempts to score during each UDQ, while
the away team ("Philosophe") attempts to score during each ldq.

The maximum number of points that can be scored during an entire
inning (UDQ + ldq) is 11. If Tiger scores all 11 points in the UDQ,
they receive 11 points plus the total score of the next
inning (Philosophe automatically receives a score of zero for the
current inning's ldq, but may choose to play anyway, for a maximum of
17.5 minutes, at their discretion). If the total score of an
inning (UQD + ldq) is 11, but Tiger did not score all 11 points, their
score is (UDQ - ldq), and Philosophe scores the number of points
earned in the ldq. If less than 11 points are scored during an inning,
each team receives the number of points earned in their respective
duoquarters.

In the special case of Tiger earning 11 points in the UDQ of the 13th
and last inning, Tiger scores 8 points while Philosophe scores 3.

Points are tallied in a running sum, and the winner of Calvinball is
the team with the most points at the end of the 13th inning.

Example 1:

Inn#     1     2     3
      +-----+-----+-----+
UDQ   | 11  |  2  |  3  |
lqd   |  0  |  9  |  5  |
      +-----+-----+-----+
Tiger | 22  |  15 |  18 |
Philo |  0  |   9 |  14 |
      +-----+-----+-----+

In the first inning, Tiger earns all 11 possible points in the UDQ, to
which are added the 11 points cumulatively earned in the second
inning, for a total 1st inning score of 22. Philosophe earns 0 points
for the first inning. In the second inning, Philosophe scores 9
points, while Tiger scores (9 - 2 = -7) points, for a running total of
9 and 15. Less than 11 points are earned in the third inning, so each
team simply adds their respective points to their running total.

Example 2:

Inn#     12    13
      +-----+-----+
UDQ   |  4  |  11 |
lqd   |  6  |   0 |
      +-----+-----+
Tiger | 59  |  67 |
Philo | 63  |  66 |
      +-----+-----+

Tiger has achieved the special case of earning 11 points in the UDQ of
the final inning, thus scoring 8 points to Philosophe's 3, for a final
score of (59 + 8 = 67) to (63 + 3 = 66), thus winning by one point.

Write a class "calvinball" which records each duoquartile's points
earned with the methods "upper-earned" and "lower-earned", and reports
the current score of each team with method "calculate-score".
|#


;;;semi-functional style - uses set!
;;initial scoreboard
;;scoreboard grows right to left, each member is a pair (UDQ ldq)
(define scoreboard '())
(define (clear-scoreboard) (set! scoreboard '()))

(define (upper-earned p) 
  (set! scoreboard (cons (list p) scoreboard)))
(define (lower-earned p) 
  (set! scoreboard (cons (cons (caar scoreboard) (list p)) (cdr scoreboard))))

(define (calculate-score)
  (cond
   [(not (eq? (length scoreboard) 13))
    (begin 
      (display "error: scoreboard is length ")
      (display (length scoreboard))
      (newline))]
   [(eq? (length scoreboard) 13)
    (let loop ([s (reverse scoreboard)] [accT 0] [accP 0])
      (cond
       [(null? s) (begin (display `(T: ,accT P: ,accP)) (newline))]
       [(and (eq? (length s) 1) (eq? (caar s) 11))
        (loop '() (+ accT 8) (+ accP 3))] ;13th inning edge case
       [(eq? (caar s) 11) ;T earns 11 in UDQ
        (loop (cdr s) (+ accT (+ (caar s) (caadr s) (cadadr s))) accP)]
       [(eq? (+ (caar s) (cadar s)) 11) ;inning total = 11
        (loop (cdr s) (+ accT (- (caar s) (cadar s))) (+ accP (cadar s)))]
       [else ;inning total < 11
        (loop (cdr s) (+ accT (caar s)) (+ accP (cadar s)))]))]))

;;tests
(define (func-test)
  (begin
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0)
           (0 0) (0 0) (0 0) (0 0) (0 0) (0 0)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (0 0) (11 0)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (11 0)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (0 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((0 0) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((7 2) (0 0) (0 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((7 2) (0 11) (0 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((7 2) (0 11) (11 0) (0 0) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((7 2) (0 11) (11 0) (0 11) (0 0) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((7 2) (0 11) (11 0) (0 11) (9 1) (0 0) (0 0) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    (for-each (lambda (s) (upper-earned (car s)) (lower-earned (cadr s)))
         '((7 2) (0 11) (11 0) (0 11) (9 1) (11 0) (2 9) 
           (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display scoreboard) (newline)
    (calculate-score)
    (clear-scoreboard)
    (display scoreboard) (newline)
    ))
    
;;;object-oriented style - repurposes functions as methods
(define (mk-game)
  ;;stored state
  (define scoreboard '())
  ;;method definitions
  (define (clear-scoreboard) (set! scoreboard '()))
  (define (upper-earned p) 
    (set! scoreboard (cons (list p) scoreboard)))
  (define (lower-earned p) 
    (set! scoreboard (cons (cons (caar scoreboard) (list p)) (cdr scoreboard))))
  (define (calculate-score)
    (cond
     [(not (eq? (length scoreboard) 13))
      (begin 
        (display "error: scoreboard is length ")
        (display (length scoreboard))
        (newline))]
     [(eq? (length scoreboard) 13)
      (let loop ([s (reverse scoreboard)] [accT 0] [accP 0])
        (cond
         [(null? s) (begin (display `(T: ,accT P: ,accP)) (newline))]
         [(and (eq? (length s) 1) (eq? (caar s) 11))
          (loop '() (+ accT 8) (+ accP 3))] ;13th inning edge case
         [(eq? (caar s) 11) ;T earns 11 in UDQ
          (loop (cdr s) (+ accT (+ (caar s) (caadr s) (cadadr s))) accP)]
         [(eq? (+ (caar s) (cadar s)) 11) ;inning total = 11
          (loop (cdr s) (+ accT (- (caar s) (cadar s))) (+ accP (cadar s)))]
         [else ;inning total < 11
          (loop (cdr s) (+ accT (caar s)) (+ accP (cadar s)))]))]))
  ;;the (self) function: exports an interface; handles incoming messages; 
  ;;dispatches methods according to message.
  (define (self msg . arg)
    (case msg
      [(get-sb) scoreboard]
      [(clear) (clear-scoreboard)]
      [(set-udq) (upper-earned (car arg))]
      [(set-ldq) (lower-earned (car arg))]
      [(calculate) (calculate-score)]))
  ;;expose the (self) function to the outside
  self
)

;;tests
(define game1 (mk-game))
(define (obj-test)
  (begin
    (display (game1 'get-sb)) (newline)
    (for-each (lambda (s) (game1 'set-udq (car s)) (game1 'set-ldq (cadr s)))
              '((7 2) (0 11) (11 0) (0 11) (9 1) (11 0) (2 9) 
                (0 0) (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display (game1 'get-sb)) (newline)
    (game1 'calculate)
    (game1 'clear)
    (display (game1 'get-sb)) (newline)
    (for-each (lambda (s) (game1 'set-udq (car s)) (game1 'set-ldq (cadr s)))
              '((7 2) (0 11) (11 0) (0 11) (9 1) (11 0) (2 9) 
                (0 0) (0 0) (0 0) (11 0) (3 8)))
    (display (game1 'get-sb)) (newline)
    (game1 'calculate)
    (game1 'clear)
    (display (game1 'get-sb)) (newline)
    ))
    
   
