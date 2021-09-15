#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 1, завдання 14.1
(define (power b p depth) ; процедура піднесення до ступеня
  (if (= p 0)
      (begin (display "\n Глибина рекурсії: ") (display depth)  (display "\n") 1)
      (if(odd? p)
         (* b (power b (/ (- p 1) 2) (+ depth 1)) (power b (/ (- p 1) 2) (+ depth 1)))
         (* (power b (/ p 2)  (+ depth 1)) (power b (/ p 2)  (+ depth 1))) 
       )
  )
)

(define (func b p m) ; процедура обчислення кінцевого виразу
  (modulo (power b p 0) m))


(display "Завдання 14.1\n")
(display "Для введених значень змінних b, p та m отримано результат:\n")
(func (read) (read) (read))
      