;;  apl.scm 
;;  More fully featured APL implementation in Scheme
;;  http://www.cs.unm.edu/~williams/cs257/apl.html
;;  20130223 
;;  revised UTC20150610

(lambda (pred ls)
  (cond ((null? ls) '())
        ((pred (car ls))
         (cons (car ls) (filter pred (cdr ls))))
        (else
         (filter pred (cdr ls))))))

(define atom-map
  (lambda (proc . args)
    (cond ((null? (car args)) '())
          ((pair? (car args))
           (cons (apply atom-map proc (map car args))
                 (apply atom-map proc (map cdr args))))
          (else
           (apply proc args)))))

(define promote-scalars
  (lambda (mixed-args)
    (let ((non-scalars (filter pair? mixed-args)))
      (if (null? non-scalars)
          mixed-args
          (let ((template (car non-scalars)))
            (map (lambda (arg)
                   (if (pair? arg)
                       arg
                       (atom-map (lambda (x) arg) template)))
                 mixed-args))))))

(define apl-ize
  (lambda (proc)
    (lambda args
      (let ((foo (promote-scalars args)))
        (cond ((null? foo)
               (proc))
              ((pair? (car foo))
               (apply atom-map proc foo))
              (else
               (apply proc args)))))))

(define promote-booleans
  (lambda (ls)
    (map (lambda (x)
           (cond ((eq? x #t) 1)
                 ((eq? x #f) 0)
                 (else x)))
         ls)))

(define scheme+ +)
(define scheme* *)
(define scheme- -)
(define scheme/ /)

(define +
  (apl-ize
   (lambda args
     (if (null? args)
         0
         (apply scheme+ (promote-booleans args))))))

(define *
  (apl-ize
   (lambda args
     (if (null? args)
         1
         (apply scheme* (promote-booleans args))))))

(define -
  (apl-ize
   (lambda args
     (if (null? args)
         0
         (apply scheme- args)))))

(define /
  (apl-ize
   (lambda args
     (if (null? args)
         1
         (apply scheme/ args)))))

(define %  (apl-ize modulo))

(define ^
  (apl-ize
   (lambda (x y)
     (if (procedure? x)
         (iterate x y)
         (expt x y)))))

(define +.
  (apl-ize
   (letrec
       ((loop
         (lambda args
           (if (null? args)
               #f
               (if (car args)
                   #t
                   (apply loop (cdr args)))))))
     loop)))

(define *.
  (apl-ize
   (letrec
       ((loop
         (lambda args
           (if (null? args)
               #t
               (if (car args)
                   (apply loop (cdr args))
                   #f)))))
     loop)))

(define ~
  (apl-ize (lambda (x) (not x))))

(define scheme< <)
(define scheme> >)
(define scheme= =)

(define < (apl-ize scheme<))
(define > (apl-ize scheme>))
(define = (apl-ize scheme=))

(define <>
  (lambda (x y)
    (~ (= x y))))

(define <=
  (lambda (x y)
    (~ (> x y))))

(define >=
  (lambda (x y)
    (~ (< x y))))

;; (: + *) is dot-product
(define :
  (lambda (op1 op2)
    (lambda (u v) ((_ op1) (op2 u v)))))

;; (@ + *) is matrix-product
(define @
  (lambda (op . args)
    (letrec
        ((outer-product
          (lambda (u v)
            (map (lambda (u_i)
                   (map (lambda (v_j) (op u_i v_j))
                        v))
                 u))))
      (if (null? args)
          outer-product
          (lambda (A B)
            ((@ (: op (car args))) A (t B)))))))

;; iota
(define i
  (apl-ize
   (lambda (n)
     (letrec
         ((loop
           (lambda (m acc)
             (if (= m 0)
                 acc
                 (loop (sub1 m) (cons m acc))))))
       (loop n '())))))

;; select
(define $
  (lambda (u v)
    (cond ((null? v) '())
          ((car u)
           (cons (car v) ($ (cdr u) (cdr v))))
          (else
           ($ (cdr u) (cdr v))))))

(define _
  (lambda (op)
    (lambda (A)
      (if (pair? (car A))
          (map (lambda (row) (apply op row)) A)
          (apply op A)))))

(define compose
  (lambda (f g)
    (lambda (x) (f (g x)))))

(define iterate
  (lambda (op n)
    (if (= n 0)
        (lambda (x) x)
        (compose op (iterate op (sub1 n))))))

;; rho
(define r
  (lambda args
    (letrec
        ((make-row
          (lambda (v cols)
            (if (eq? cols 0)
                '()
                (cons (car v)
                      (make-row (cdr v)
                                (sub1 cols))))))
         (make-matrix
          (lambda (v rows cols)
            (if (eq? rows 0)
                '()
                (cons (make-row v cols)
                      (make-matrix ((iterate cdr cols) v)
                                   (sub1 rows)
                                   cols))))))
      (if (null? args)
          (lambda (A)
            (list (length A) (length (car A))))
          (lambda (v)
            (make-matrix v (car args) (cadr args)))))))

(define alt
  (lambda (ls)
    (letrec
        ((alt-row
          (lambda (j k ls)
            (if (null? ls)
                '()
                (cons (* j k (car ls))
                      (alt-row j (- k) (cdr ls))))))
         (alt-col
          (lambda (j k ls)
            (if (null? ls)
                '()
                (cons (alt-row j k (car ls))
                      (alt-col (- j) k (cdr ls)))))))
      (if (pair? (car ls))
          (alt-col 1 1 ls)
          (alt-row 1 1 ls)))))

(define !
  (apl-ize
   (lambda (n)
     (if (= n 0)
         1
         ((_ *) (i n))))))

(define ? (apl-ize (lambda (n) (random (add1 n)))))

(define flatten
  (lambda (ls)
    (apply append ls)))

(define remove
  (lambda (item ls)
    (filter (lambda (x) (not (eq? x item))) ls)))

(define p
  (lambda (s)
    (if (null? s)
        '(())
        (flatten
         (map (lambda (x)
                (map (lambda (q) (cons x q))
                     (p (remove x s))))
              s)))))

(define primes
  (lambda (n)
    ($ (= 2 ((_ +) (= 0 ((@ %) (i n) (i n)))))
       (i n))))

(define e
  ((_ +) (/ (! (- (i 100) 1)))))

(define sort
  (lambda (x)
    ($ ((_ <) (p x)) (p x))))

(define id
  (lambda (n)
    (* ((@ =) (i n) (i n)) 1)))

(define t
  (lambda (x)
    (apply map list x)))

(define norm
  (lambda (v)
    (^ ((_ +) ((_ +) (^ v 2))) 1/2)))

(define eigenvector
  (lambda (A)
    (let ((d (length A)))
      ((^ (lambda (x) ((@ + *) A (/ x (norm x)))) 100)
       ((r d 1) (i d))))))

(define matrix-power
  (lambda (A n)
    (cond ((= n 0) (id n))
          ((= n 1) A)
          (else
           ((@ + *) A (matrix-power A (sub1 n)))))))

(define column-vector
  (lambda args
    ((r (length args) 1) args)))
