#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 1, завдання 14.2
(define (odd-numbers current-number) ; процедура виведення всіх непарних чисел, менших за n та більших за 0
  (if (= current-number 1)
      (display current-number )
      (begin
        (if (odd? current-number)
          (display current-number)
          (display " ")
         )
         (odd-numbers (- current-number 1))
       )
   )
)
(display "Завдання 14.2\n")
(display "Для введеного значення n отримано послідовність:\n")
(odd-numbers (read))



