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

(define (complex-abs c)
  (expt (+ (expt (real-part c) 2)
           (expt (imag-part c) 2))
        0.5))

(define (fczn-checked zin cin nin)
  (let loop ([z zin] [n nin] [trace '()])
    (if (or (and (> n 0) (< (complex-abs z) 2))
            (zero? z))
        (let ([zn+1 (fcz z cin)])
          (loop zn+1 (sub1 n) (append trace (list zn+1))))
        trace)))
