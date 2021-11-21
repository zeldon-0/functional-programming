#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 8, завдання 14

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))
(define (make-difference a1 a2) (list '- a1 a2))
(define (make-product m1 m2) (list '* m1 m2))
(define (make-quotient d1 d2) (list '/ d1 d2))
(define (make-exponent p1 p2) (list 'expt p1 p2))
(define (make-cos t1) (list 'cos t1))
(define (make-sin t1) (list 'sin t1))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (difference? x)
  (and (pair? x) (eq? (car x) '-)))

(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (quotient? x)
  (and (pair? x) (eq? (car x) '/)))
(define (exponent? x)
  (and (pair? x) (eq? (car x) 'expt)))

(define (cos? x)
  (and (pair? x) (eq? (car x) 'cos)))
(define (sin? x)
  (and (pair? x) (eq? (car x) 'sin)))
(define (tan? x)
  (and (pair? x) (eq? (car x) 'tan)))
(define (cotan? x)
  (and (pair? x) (eq? (car x) 'cotan)))


(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(define (dividend p) (cadr p))
(define (divisor p) (caddr p))

(define (power-base p) (cadr p))
(define (power-exponent p) (caddr p))

(define (tan-base p) (cadr p))
(define (cotan-base p) (cadr p))


(define (deriv exp var)
  (cond
    [(number? exp) 0]
    [(variable? exp)
     (if (same-variable? exp var) 1 0)]
    [(sum? exp)
     (make-sum (deriv (addend exp) var)
               (deriv (augend exp) var)
     )
    ]
    [(difference? exp)
     (make-difference (deriv (addend exp) var)
               (deriv (augend exp) var)
     )
    ]
    [(product? exp)
     (make-sum
       (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
       (make-product (deriv (multiplier exp) var)
                     (multiplicand exp))
     )
    ]
    [(quotient? exp)
     (make-quotient
      (make-difference
       (make-product (deriv (dividend exp) var)
                     (divisor exp))
       (make-product (dividend exp)
                     (deriv (divisor exp) var))
      )
      (make-product (divisor exp) (divisor exp))
     )
    ]
    [(exponent? exp)
     (make-product
          (make-product (power-exponent exp)
                        (make-exponent (power-base exp)
                                       (make-difference (power-exponent exp) '1)
                        )
          )
          (deriv (power-base exp) var)
     )
    ]
    [(tan? exp)
     (make-product
          (deriv (tan-base exp) var)
          (make-exponent (make-cos (tan-base exp)) '-2)
     )
    ]
    [(cos? exp)
     (make-product
          (deriv (tan-base exp) var)
          (make-product
            '-1
            (make-sin (tan-base exp))
          )
     )
    ]
    [(sin? exp)
     (make-product
          (deriv (tan-base exp) var)
          (make-cos (tan-base exp))
     )
    ]
    [(tan? exp)
     (make-product
          (deriv (tan-base exp) var)
          (make-exponent (make-cos (tan-base exp)) '-2)
     )
    ]
    [else
     (display "unknown expression type - DERIV" )]
  )
)

(define (calculate exp var var-value)
  (cond
    [(number? exp) exp]
    [(variable? exp) var-value]
    [(sum? exp)
     (+ (calculate (addend exp) var var-value)
        (calculate (augend exp) var var-value)
     )
    ]
    [(difference? exp)
     (- (calculate (addend exp) var var-value)
        (calculate (augend exp) var var-value)
     )
    ]
    [(product? exp)
     (* (calculate (multiplier exp) var var-value)
        (calculate (multiplicand exp) var var-value)
     )
    ]
    [(quotient? exp)
     (/ (calculate (dividend exp) var var-value)
        (calculate (divisor exp) var var-value)
     )
    ]
    [(exponent? exp)
     (expt (calculate (power-base exp) var var-value)
        (calculate (power-exponent exp) var var-value)
     )
    ]
    [(cos? exp)
     (cos (calculate (tan-base exp) var var-value)
     )
    ]
    [(tan? exp)
     (tan (calculate (tan-base exp) var var-value)
     )
    ]
    [else
     (display "unknown expression type - DERIV" )]
  )
)

(define (second-derivative exp var)
   (deriv (deriv exp var)  var)
)

(deriv '(* (* x y) (+ x 3)) 'x)
(deriv '3 'x)
(deriv '(* x y) 'x)
(deriv '(expt x 3) 'x)
(deriv '(/ x (+ (* 2 x) y)) 'x)

;(define result (deriv (deriv '(/ (* 3  x) (+ (* 2 x) (* x x))) 'x) 'x))
;(calculate result 'x 1)

;(calculate (second-derivative '(expt x 3) 'x) 'x 4)


;(define result (second-derivative '(tan (* 2 x)) 'x))
(define result (second-derivative '(tan (* 2 x)) 'x))

(calculate result 'x 1)