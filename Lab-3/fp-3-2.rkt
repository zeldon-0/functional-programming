#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 3, завдання 14.2

(define start -1)
(define end pi)

(define (f x)
  (* x (exp (* -1 x)))
)


(define (right-rectangles a b n)
  (let* ([step (/(- b a)n)]
    [sum (rectangle-sum (+ a step) b step 0)])
    (* step sum)
    )
)

(define (left-rectangles a b n)
  (let* ([step (/(- b a)n)]
    [sum (rectangle-sum a (- b step) step 0)])
    (* step sum)
    )
)

(define (middle-rectangles a b n)
  (let* ([step (/(- b a)n)]
    [sum (middle-rectangle-sum a (- b step) step 0)])
    (* step sum)
    )
)

(define (rectangle-sum current-x end-x step current-sum)
   (let ((current-f (f current-x)))
     (if (> current-x end-x)
         current-sum
         (rectangle-sum (+ current-x step) end-x step (+ current-sum current-f))
     )
   )
)

(define (middle-rectangle-sum current-x end-x step current-sum)
   (let ((current-f (f (/(+ current-x current-x step) 2))))
     (if (> current-x end-x)
         current-sum
         (middle-rectangle-sum (+ current-x step) end-x step (+ current-sum current-f))
     )
   )
)

(define (simpson a b n)
  (let* ([step (/ (- b a) n)]
    [sum (simpson-step a b step 0)])
    (* (/ step 3) sum)
  )
)

(define (simpson-step current-x end-x step current-sum)
     (let* ([start-f (f current-x)]
            [middle-f (f (+ current-x step))]
            [end-f (f (+ current-x step step))]
            [step-sum (+ start-f (* 4 middle-f) end-f)])
     (if (> current-x end-x)
         current-sum
         (simpson-step (+ current-x step step) end-x step (+ current-sum step-sum))
     )
   )
)


(display (right-rectangles start end 20))
(display "\n")
(display (left-rectangles start end 20))
(display "\n")
(display (middle-rectangles start end 20))
(display "\n")
(display (simpson start end 20))
