;;  20150807
(define (obj state)
  (lambda (msg)
    (case msg
      ['get (lambda () state)]
      ['set (lambda (val) (set! state val))])))

