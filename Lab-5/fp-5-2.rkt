#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 5, завдання 14.2


; процедура для отримання значення у списку за індексом
(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

; процедура для створеного модифікованого списка
(define (create-list n)
  ; допоки не дійшли до кінця списку, створювати поточний елемент на основі двох вхідних
  (cond [(or (< n (/(length irrational-numbers) 2)) (= n (/(length irrational-numbers) 2)))
         (let* ([first-irrational-number (get-list-element irrational-numbers (- (* 2 n) 1))]
           [second-irrational-number (get-list-element irrational-numbers (* 2 n))]
           [a1 (car first-irrational-number)]
           [b1 (cdr first-irrational-number)]
           [a2 (car second-irrational-number)]
           [b2 (cdr second-irrational-number)]
           )
           ; поточний елемент обчислюється як:
           ; a = (a1*a2 + b1*b2) / (a2^2 + b2^2)
           ; b = -(a1*b2 - b1*a2) / (a2^2 + b2^2)
           (append (list(cons  (/ (+ (* a1 a2) (* b1 b2)) (+ (* a2 a2) (* b2 b2)))
                               (* -1 (/ (- (* a1 b2) (* b1 a2)) (+ (* a2 a2) (* b2 b2))))))
                   ; результуючий список формується на основі щойно створеного елементу та списку, утвореного рекурсивиним викликом
                   ; процедури для наступних елементів
                   (create-list (+ n 1)))
          )
         ]
        ; у випадку, якщо вхідний список вичерпано, повернути порожній список
        [else
         '()]
  )
)

; процедура друкування списку
(define (print-irrational list n)
  (cond
    ; кожен некінцевий елемент друкувати як "( [-]a +- b i ), "
    [(< n (length list))
        (begin
          (display "(")
          (if (< (car (get-list-element list n)) 0)
              (display " - ")
              (display " ")
          )
          (display (abs (car (get-list-element list n))))
          (if (< (cdr (get-list-element list n)) 0)
              (display " - ")
              (display " + ")
          )
          (display (abs (cdr (get-list-element list n))))
          (display "i ), ")
          (print-irrational list (+ n 1))
        )
    ]
    ; кінцевий елемент друкувати як "( [-]a +- b i )"
    [(= n (length list))
        (begin
          (display "(")
           (if (< (car (get-list-element list n)) 0)
              (display " - ")
              (display " ")
          )
          (display (abs (car (get-list-element list n))))
          (if (< (cdr (get-list-element list n)) 0)
              (display " - ")
              (display " + ")
          )
          (display (abs (cdr (get-list-element list n))))
          (display "i )")
          (print-irrational list (+ n 1))
        )
    ]
    ; по закінченню списку надрукувати перехід на новий рядок
   [else
    (display "\n")
   ]
  )
)


(define irrational-numbers (list (cons 7 -4) (cons 3 2) (cons 10 -3) (cons -4 7)))
(display "The starting irrational numbers list: ")
(print-irrational irrational-numbers 1)
(display "The new irrational numbers list: ")
(print-irrational (create-list 1) 1)
