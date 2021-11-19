#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 6, завдання 14.1

; фунекція пошуку найбільшого від'ємого числа
(define (find-maximum-negative vector)
  (find-maximum-negative-internal vector 0)
)

; внутрішня фунекція пошуку найбільшого від'ємого числа
(define (find-maximum-negative-internal vector index)
  (cond  [(< index (vector-length vector))
        (begin
         (define current-number (vector-ref vector index))
         (cond
           ; якщо максимальне число не визначене, а поточне число - від'ємне, присвоїти його значення максимсальному  
           [(and (not (pair? maximum-negative))  (< current-number 0))
                 (set! maximum-negative (cons index current-number))
                 ]
           ; якщо максимальне число визначене, а поточне число - від'ємне і більше за нього, присвоїти його значення максимсальному
           [(and (pair? maximum-negative) (< (cdr maximum-negative) current-number) (< current-number 0))
                 (set! maximum-negative (cons index current-number))
                 ]
          )
          (find-maximum-negative-internal vector (+ 1 index))
        )
    ]
   )
)

; фунекція пошуку найменшого додатного числа
(define (find-minimum-positive vector)
  (find-minimum-positive-internal vector 0)
)

; внутрішня фунекція пошуку найменшого додатного числа
(define (find-minimum-positive-internal vector index)
  (cond  [(< index (vector-length vector))
        (begin
         (define current-number (vector-ref vector index))
         (cond
           ; якщо мінімальне число не визначене, а поточне число - дадатне, присвоїти його значення мінімальному  
           [(and (not (pair? minimum-positive))  (> current-number 0))
                 (set! minimum-positive (cons index current-number))
                 ]
           ; якщо мінімальне число визначене, а поточне число - дадатне і менеше за нього, присвоїти його значення мінімальному
           [(and (pair? minimum-positive) (> (cdr minimum-positive) current-number) (> current-number 0))
                 (set! minimum-positive (cons index current-number))
                 ]
          )
          (find-minimum-positive-internal vector (+ 1 index))
        )
    ]
   )
)

; функція друку результатів
(define (print-results)
  (begin
    (display "The initial vector: ")
    (display number-vector)
    (display "\n")
    (cond  [(pair? maximum-negative)
              (begin
                (display "The maximum negative number: ")
                (display (cdr maximum-negative))
                (display ". Its index within the list: ")
                (display (car maximum-negative))
              )
            ]
           ; Окремий випадок, якщо від'ємних чисел немає взагалі
           [else
              (begin
                (display "Could not find the maximum negative number as there are no negative numbers in the vector.")
              )
            ]
     )
    (display "\n")
        (cond  [(pair? minimum-positive)
              (begin
                (display "The minimum positive number: ")
                (display (cdr minimum-positive))
                (display ". Its index within the list: ")
                (display (car minimum-positive))
              )
            ]
           ; Окремий випадок, якщо додатних чисел немає взагалі
           [else
              (begin
                (display "Could not find the minimum positive number as there are no positive numbers in the vector.")
              )
            ]
     )
 )
)

(define number-vector #(12 -32 5 3 -2 6 -15 4 -16))
(define maximum-negative '())
(define minimum-positive '())
(find-maximum-negative number-vector)
(find-minimum-positive number-vector)
(print-results)