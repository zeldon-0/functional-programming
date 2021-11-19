#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 5, завдання 14.1

; процедура для отримання значення у списку за індексом
(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

; процедура для створеного модифікованого списка
(define (create-list n)
  ; допоки не дійшли до кінця списку, створювати поточний елемент як добуток двох вхідних
  (cond [(or (< n (/(length numbers) 2)) (= n (/(length numbers) 2)))
         (let ([first-fraction (get-list-element numbers (- (* 2 n) 1))]
           [second-fraction (get-list-element numbers (* 2 n))])
           ; результуючий список формується на основі щойно створеного елементу та списку, утвореного рекурсивиним викликом
           ; процедури для наступних елементів
           (append (list(cons (*(car first-fraction) (car second-fraction)) (*(cdr first-fraction) (cdr second-fraction))))
                   (create-list (+ n 1)))
          )
         ]
        ; у випадку, якщо вхідний список вичерпано, повернути порожній список
        [else
         '()]
  )
)

; процедура друкування списку
(define (print-fractions list n)
  (cond
    ; кожен некінцевий елемент друкувати як "a/b, "
    [(< n (length list))
        (begin
          (display (car (get-list-element list n)))
          (display "/")
          (display (cdr (get-list-element list n)))
          (display ", ")
          (print-fractions list (+ n 1))
        )
    ]
    ; кінцевий елемент друкувати як "a/b"
    [(= n (length list))
        (begin
          (display (car (get-list-element list n)))
          (display "/")
          (display (cdr (get-list-element list n)))
          (print-fractions list (+ n 1))
        )
    ]
    ; по закінченню списку надрукувати перехід на новий рядок
    [else
     (display "\n")
    ]
  )
)

(define numbers (list (cons 1 2) (cons 5 4) (cons 7 13) (cons 4 19) (cons -5 27) (cons 8 19)))
(display "The starting fractions list: ")
(print-fractions numbers 1)
(display "The new fractions list: ")
(print-fractions (create-list 1) 1)
