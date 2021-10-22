#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 3, завдання 14.1
; точність, за якою визначається закінчення процедури 
(define error 1e-5)

; f(x) = e^x + ln x - 10x
(define (f x)
  ;(+ (exp x) (log x) (* -10 x))
  (- (* 2 x) (cos x))
)
; X = (e^x + ln x) / 10
(define (x-func x)
  ;(/(+ (exp x) (log x))10)
  (/ (cos x) 2)
)
;f'(x) = e^x + 1/x - 10
(define (f-derivative x)
  ;(+ (exp x) (/ 1 x) -10)
  (+ (sin x) 2)
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

(define fixed-point-iteration-solution (fixed-point-iteration -3))
(define newton-solution (newton-method -3))
(define solution-difference (abs (- fixed-point-iteration-solution newton-solution)))

(display "Solution using the Fixed-Point Iteration method: ")
(display fixed-point-iteration-solution)
(display "\n")
(display "Solution using the Newton method: ")
(display newton-solution)
(display "\n")
(display "Absolute difference between the solutions: ")
(display solution-difference)