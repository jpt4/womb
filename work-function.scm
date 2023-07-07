;; work-function.scm
;; 20230707Z
;; jpt4
;; Chez Scheme v9.8.5
;; per https://web.archive.org/web/20190106235708/http://ctrlcreep.tumblr.com/post/145852683132/the-suicide-mortgage

;p0 = initial population size
(define (work-function p0)
  (lambda (loss) (/ 1.0 (- p0 loss))))

;[l, h]
(define (range l h)
  (member l (iota (+ h 1))))

;[l, h]
(define (series e low high)
  (map e (range low high)))

;[low, high]
(define (sigmasum e low high)
  (fold-right +
	      0
	      (series e low high)))

;filter the partial sums for the first N < p0 that sum to at most 1.
(define (efficient-frontier p0)
  (let [(wf (work-function p0))]
    #;(map (lambda (a)
	   (series wf 0 a))
	 (iota p0))
    (filter (lambda (a) (<= a 1))
	    (map (lambda (b)
		   (sigmasum wf 0 b))
		 (iota p0)))
    ))

;ratio of population that works less than or equal to the time to earn
;a reward working individually
(define (efficient-ratio p0)
  (/ (length (efficient-frontier p0))
     (* 1.0 p0)))

(define (efficiency-across-populations min-pop max-pop)
  (map efficient-ratio (range min-pop max-pop)))

;take list of population efficient ratios
(define (avg-efficiency-across-populations efls)
  (/ (fold-right + 0 efls) (* 1.0 (length efls))))

;test
#|
(load "work-function.scm")
> (avg-efficiency-across-populations (efficiency-across-populations 1 1000))
0.6313666542467459
> (avg-efficiency-across-populations (efficiency-across-populations 2 1000))
0.6309976518986444
|#



