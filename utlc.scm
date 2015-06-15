;;  utlc.scm  jpt4  UTC20150614
;;  untyped lambda calculus in scheme
;;  mostly wrong

(define id (lambda (x) x))
(define self (lambda (s) (s s)))

(define first (lambda (a) (lambda (b) a)))
(define second (lambda (a) (lambda (b) b)))

(define true first)
(define false second)

(define and (lambda (p) (lambda (q) ((p q) false))))
(define or (lambda (p) (lambda (q) ((p true) q))))

(define ifte (lambda (c) (lambda (a) (lambda (b) ((c a) b)))))

(define zero (lambda (f) (lambda (x) x)))
(define one (lambda (f) (lambda (x) (f x))))

(define plus (lambda (m) (lambda (n) (lambda (f) 
               (lambda (x) (m (f (n f x))))))))
(define succ (lambda (n) (lambda (f) (lambda (x) (f (n (f x)))))))

(define pair (lambda (x) (lambda (y) (lambda (z) (z (x y))))))
(define head (lambda (p) (p (lambda (x) (lambda (y) x)))))
(define tail (lambda (p) (p (lambda (x) (lambda (y) y)))))

(define Y (lambda (f) (lambda (x) (f (x x))) (lambda (x) (f (x x)))))
(define Yr (lambda (f) (self (lambda (x) (f (self x))))))

(define T ((lambda (x) (lambda (y) (y (x (x y))))) 
           (lambda (x) (lambda (y) (y (x (x y)))))))
(define Tr (self (lambda (x) (lambda (y) (y (x (x y)))))))
