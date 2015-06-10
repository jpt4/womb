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

(define generate-genome
  (lambda (numq sigma sys-size)
    (let ([alph-size (length sigma)])
      (cond
        [(zero? sys-size) '()]
        [else (cons ((lookup (random alph-size) sigma)]
