;;jt.scm
;;20180416Z
;;trace of a point in the Julia set of the quadratic map

(define (fcz z c)
  (+ (expt z 2) c))

(define (fczn zin cin nin)
  (let loop ([z zin] [n nin] [trace '()])
    (if (> n 0)
        (let ([zn+1 (fcz z cin)])
          (loop zn+1 (sub1 n) (append trace (list zn+1))))
        trace)))

(define (fczn-checked zin cin nin)
  (let loop ([z zin] [n nin] [trace '()])
    (if (or (and (> n 0) (< (complex-abs z) 2))
            (zero? z))
        (let ([zn+1 (fcz z cin)])
          (loop zn+1 (sub1 n) (append trace (list zn+1))))
        trace)))

(define (string->seed-coord st)
  (let* ([hsh (symbol-hash (string->symbol st))]
         [mag (/ (truncate (* (/ hsh (expt 10 (round (logb hsh 10)))) 1000.0)) 1000.0)]
         [rot (* (random 2.0) pi)]
         [x (* (cos rot) mag)]
         [y (* (sin rot) mag)]
         [seed (+ (* (if (even? hsh) 1.0 -1.0) x)
                  (* (if (even? (round hsh)) 1.0 -1.0) y i))])
    (begin
      (display hsh)
      (newline)
      (display mag)
      (newline)
      (display seed)
      ))
  )
;;auxiliary
(define i (sqrt -1.0))
(define pi 3.14159265)
(define (complex-abs c)
  (expt (+ (expt (real-part c) 2)
           (expt (imag-part c) 2))
        0.5))
(define (logb n b)
  (if (eq? b 2)
      (log n)
      (/ (log n) (log (* b 1.0)))))
