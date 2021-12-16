#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 3, завдання 14.1

; точність, за якою визначається закінчення процедури 
(define error 1e-5)

; f(x) = e^x + 10x
(define (f x)
  (+ (exp x) (* 10 x))
)
; X = e^x / (-10)
(define (x-func x)
  (/ (exp x) -10)
)
;f'(x) = e^x + 10
(define (f-derivative x)
  (+ (exp x) 10)
)

; Процедура обчислення кореня методом простої ітерації
(define (fixed-point-iteration current-x)
  ; Обчислення наступного значення X, спираючись на канонічне рівняння для поточного
  (let ((next-x (x-func current-x)))
    ; Якщо різниця між поточним та наступним X менша за похибку, рекурсія припиняється, і повертається поточне значення
    (if (< (abs (- next-x current-x)) error)
      current-x
      ; Інакше процедура рекурсивно обчислює наступне наближене значення, використовуючи нове значення як поточне
      (fixed-point-iteration next-x)
      )
  )
)

; Процедура обчислення кореня методом дотичних
(define (newton-method current-x)
  ; Наступне значення X обчислюється як xk+1 = xk - (f(xk) / f'(xk))
  (let ((next-x (- current-x (/(f current-x) (f-derivative current-x)))))
    ; Якщо різниця між поточним та наступним X менша за похибку, рекурсія припиняється, і повертається поточне значення
    (if (< (abs (- next-x current-x)) error)
        current-x
      ; Інакше процедура рекурсивно обчислює наступне наближене значення, використовуючи нове значення як поточне        
      (newton-method next-x)
      )
  )
)

(define (bisection current-a current-b)
  ; Обчислення наступного значення X, спираючись на канонічне рівняння для поточного
  (let ((next-f (f (/ (+ current-a current-b) 2)))
        (f-a (f current-a))
        (f-b (f current-b)))
    ; Якщо різниця між поточним та наступним X менша за похибку, рекурсія припиняється, і повертається поточне значення
    (if (< (abs next-f) error)
      (/ (+ current-a current-b) 2)
      ; Інакше процедура рекурсивно обчислює наступне наближене значення, використовуючи нове значення як поточне
      (if (< (* f-a next-f) 0)
          (bisection current-a (/ (+ current-a current-b) 2))
          (bisection (/ (+ current-a current-b) 2) current-b)
      )
    )
  )
)

(define (secant-method current-a current-b)
  ; Обчислення наступного значення X, спираючись на канонічне рівняння для поточного
  (let* ([f-a (f current-a)]
        [f-b (f current-b)]
        [next-x (- current-a (* (/ f-a (- f-b f-a)) (- current-b current-a)))]
        [f-x (f next-x)])
    ; Якщо різниця між поточним та наступним X менша за похибку, рекурсія припиняється, і повертається поточне значення
    (if (< (abs f-x) error)
      next-x
      ; Інакше процедура рекурсивно обчислює наступне наближене значення, використовуючи нове значення як поточне
      (if (< (* f-a f-x) 0)
          (secant-method current-a next-x)
          (secant-method next-x current-b)
      )
    )
  )
)

(define fixed-point-iteration-solution (fixed-point-iteration 4))
(define newton-solution (newton-method 4))
(define bisection-solution (bisection -5 5))
(define secant-solution (secant-method -5 5))
(define solution-difference (abs (- fixed-point-iteration-solution newton-solution)))

(display "Solution using the Fixed-Point Iteration method: ")
(display fixed-point-iteration-solution)
(display "\n")
(display "Solution using the Newton method: ")
(display newton-solution)
(display "\n")
(display "Solution using the Bisection method: ")
(display (* 1.0 bisection-solution))
(display "\n")
(display "Solution using the Secant method: ")
(display secant-solution)
(display "\n")
(display "Absolute difference between the solutions: ")
(display solution-difference)