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

(define (ascii-match pat-exp str)
  (if (equal? pat-exp (car str))
      (cdr str)
      str))

(define (wildcard-match str)
  (if (null? str)
      str
      (cdr str)))

(define (star-match pat-exp str)
  (let loop ([pat (cdr pat-exp)]
	     [s str]
	     [stack '(*)])
    

;;(a a a (* b))
;;str is always a list
(define (regex pat-exp str)
  (cond
   [(null? pat-exp) str]
   [(ascii? pat-exp) (ascii-match pat-exp str)]
   [(equal? (car pat-exp) '@) (wildcard-match str)]
   [(equal? (car pat-exp) '*) (star-match pat-exp str)]
))


  (cond
   [(null? pat-exp) str]
   [(equal? pat-exp str) 




   
   [(and (not (null? pat-exp)) (null? str)) #f]
   [(and (pair? pat-exp) (equal? (car pat-exp) (car str)))
    (regex (cdr pat-exp) (cdr str))]
   [(and (pair? pat-exp) (equal? '@ (car pat-exp)))
    (regex (cdr pat-exp) (cdr str))]
   [(and (pair? (car pat-exp))
	 (equal? '* (caar pat-exp)))
    (let loop ([res (regex (cdar pat-exp) str)])
      (cond
       [(null? res) res]
       [(and (not (equal? #f res)) (not (null? res)))
	(loop (regex (cdar pat-exp) res))]
       [(equal? res #f)
	(regex (cdr pat-exp) str)]))]
   [else str]
   ))

(define (print p)
  (begin 
    (display p)
    (newline)))
