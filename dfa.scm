(define q0
  (lambda (ls)
    (cond
      [(null? ls) 'even]
      [else (q1 (cdr ls))])))

(define q1
  (lambda (ls)
    (cond
      [(null? ls) 'odd]
      [else (q0 (cdr ls))])))
