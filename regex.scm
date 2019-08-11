;;regex.scm
;;20181209Z
;;jpt4

#|
(string pattern) (a-z . *) -> portion of string which matches

. = any one character
* = zero or more of the preceding character
|#

(define (ascii? a) 
  (member a '(a b c d e f g h i j k l m n o p q r s t u v w x y z)))

(define (ascii-exp? exp) (ascii? (car exp)))  

(define (wc-exp? exp) (equal? (car exp) '@))

(define (star-exp? exp) (equal? (car exp) '*))

(define (ascii-match pat-exp str)
  (if (equal? (car pat-exp) (car str))
      (cdr str)
      str))

(define (wildcard-match pat-exp str)
  (if (null? str)
      str
      (cdr str)))

(define (star-match pat-exp str)
  (let loop ([pat (cdr pat-exp)]
	     [s str])
    (let ([res (regex pat s)])
      (if (equal? res s)
	  s
	  (loop pat res)))))

;;(a a a (* b))
;;str is always a list
(define (regex pat-exp str stack)
  (if (null? stack)
      (cond
       [(null? pat-exp) str]
       [(ascii-exp? pat-exp) (ascii-match pat-exp str)]
       [(wc-exp? pat-exp) 
	(regex (cdr pat-exp) (wildcard-match pat-exp str))]
       [(star-exp? pat-exp) (star-match pat-exp str)];
       [(star-exp? (car pat-exp)) 
	(regex (cdr pat-exp) 
	       (star-match (car pat-exp) str))])
      
       
))

(define (print p)
  (begin 
    (display p)
    (newline)))
