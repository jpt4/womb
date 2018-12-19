;mu-geb.mk
;20181216Z
;jpt4
;Chez-Scheme v9.5
;MU-puzzle from _Goedel, Escher, Bach_

#|
xI->xIU
Mx->Mxx
xIIIy->xUy
xUUy->xy

MI -> MU
|#

(define (caro i o)
  (fresh (a b)
         (== `(,a . ,b) i) (== a o)))

(define (cdro i o)
  (fresh (a b)
	 (== `(,a . ,b) i) (== b o)))

(define appendo
  (lambda (l s out)
    (conde
      [(== '() l) (== s out)]
      [(fresh (a d res)
         (== `(,a . ,d) l)
         (== `(,a . ,res) out)
         (appendo d s res))])))

(define (mu-o i o)
  (fresh (x y res res1 res2)
    (conde
     [(appendo x 'I i) 
      (appendo x '(I U) o)
	#;(== `(,x I U) res) 
	#;(mu-o res o)]
     [(caro i 'M) (cdro i x)
      (== `(,x . ,x) res1) (== `(M . ,res1) o)
      #;(== `(M ,x ,x) res) 
      #;(mu-o res o)]
     [(== `(,x I I I ,y) i) 
      (== `(,x U ,y) o) 
      #;(== `(,x U ,y) res) 
      #;(mu-o res o)]
     [(== `(,x U U ,y) i) 
      (== `(,x ,y) o) 
      #;(== `(,x ,y) res) 
      #;(mu-o res o)])))

