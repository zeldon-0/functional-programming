#lang racket

(define (factorial value)
  (if (= value 0)
      1
      (* value (factorial (- value 1)))
  )
)

(define (c n k)
  (/(factorial (+ n k -1)) (*(factorial k) (factorial (- n 1))))
)

(define (result n)
  (*(c 3 n) 3)
)

(result (read))