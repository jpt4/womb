;;  randomwalks.scm
;;  jpt4
;;  revised UTC20150610

#|
(define unpack
  (lambda (ls)
    (cond
      [(pair? (car(car ls))) 
|#
    
(define full-graph
  (lambda (size)
    (cond
      [(zero? size) '()]
      [else (cons (full-graph-aux size
                    (sub1 size))
              (full-graph (sub1 size)))])))

(define full-graph-aux
  (lambda (ni nn)
    (cond
      [(zero? nn) (cons `(,ni . ,nn)
                    (cons `(,nn . ,ni) '()))]
      [else (cons `(,ni . ,nn)
              (cons `(,nn . ,ni)
                (full-graph-aux ni (sub1 nn))))])))
         
