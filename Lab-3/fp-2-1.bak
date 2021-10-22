#lang racket


(define error 1e-9)
(define max-iterations 1000)


(define (result-built-in value)
  (cond
    [(and(>= value 1)(<= value 2))
       (sqrt(- 15 (* value value )))]
    [(and(>= value -1)(< value  1))
       (/ 1 (sqrt (+ value (* value value))))]
  ))


(define (result value)
  (cond
    [(and(>= value 1)(<= value 2))
       (custom-sqrt(- 15 (* value value )) 0 0)]
    [(and(>= value -1)(< value  1))
       (/ 1 (custom-sqrt (+ value (* value value)) 0 0))]
  ))


(define (custom-sqrt x n previous-member )
  (define current-member (custom-sqrt-internal x n previous-member))
  (if (or (< (abs (- previous-member current-member)) error) (> n max-iterations))
      current-member
      (custom-sqrt x (+ n 1) current-member))
  )
(define (custom-sqrt-internal x n previous-member)
    (if (= n 0)
       1
        (* 0.5 (+ previous-member (/ x previous-member)))
  )
)

(map  result-built-in (inclusive-range -2 2 0.5))
(map  result (inclusive-range -2 2 0.5))
