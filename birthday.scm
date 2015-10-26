;;birthday.scm
;;naive code to calculate the Birthday Problem - of interest primarily
;;because my first instinct for solving the math problem was to
;;compose folds and maps over ranges of numbers, instead of performing
;;loop-analogue natural recursions.
;;jpt4
;;UTC20151025

;;probability that a group size g contains n distint dob-sharing pairs
(define (multiple-dobprob g n)
  (foldr (map dobprob
              (range g (- g (* 2 n)) -2))
         * 1))

;;guile needs a more powerful default (range) than (iota)
(define (range b e i)
  (range-acc b e i '()))

(define (range-acc b e i acc)
  (cond
   [(eq? b e) (reverse acc)]
   [(range-acc (+ b i) e i (cons b acc))]))

;;as well as folds that aren't for regular expressions of character
;;strings
(define (foldr ls op acc)
  (cond
   [(null? ls) acc]
   [(foldr (cdr ls) op (op (car ls) acc))]))

;;probability of a group of size g containing at least one pair with
;;the same day of the year of birth (dob)
(define (dobprob g)
  (- 1 (* 1.0 (/ (perm 365 g) (expt 365 g)))))

(define (perm n k)
  (/ (fac n) (fac (- n k))))

;;the basic Friedmanian (fac)
(define (fac n)
  (cond
   [(eq? n 0) 1]
   [(* n (fac (- n 1)))]))

#|
Examples:
> (fac 5)
120
> (dobprob 41)
0.9031516114817354
> (multiple-dobprob 41 6)
0.3103440531700096
|#
