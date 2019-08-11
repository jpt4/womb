;20190506Z
;crtldice.scm
;jpt4

;starting with an N-sided die, after each roll the die has the number
;of sides rolled, for the next roll. What is the average number of
;rolls needed until we arrive at a 1-Sided die? -@atroyn

(define (next-die die) (+ (random die) 1))

(define (count-until-one die)
  (let loop [(d die) (acc 0) (rec '())]
    (if (= d 1)
	acc
	;(list acc (reverse rec))
	(loop (next-die d) (+ 1 acc) (cons d rec)))))

(define (average-rolls die num)
  (let loop [(i num) (acc 0)]
    (if (zero? i)
	(/ (/ acc num) 1.0)
	(loop (- i 1) (+ acc (count-until-one die))))))
