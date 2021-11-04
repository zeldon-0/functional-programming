#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 4, завдання 14.1

; процедура обчислення значення n-ого члена послідовності
(define (hexagonal-number n)
  (-(* 2 n n) n)
)

; процедура створення списку 6-кутних чисел
(define (make-list size)
  (make-list-inner '() size)
)

; процедура для рекурсивного створення списку 6-кутних чисел по елементу за виклик
(define (make-list-inner current-list size)
  ; якщо поточний - початковий - список порожій, проініціювати його першим елементом послідовності
  (cond [(null? current-list)
         (make-list-inner (list (hexagonal-number 1)) size)]
        ; якщо поточний список менший за заданий розмір, доповнити його наступним елементом послідовності
        [(< (length current-list) size)
         (make-list-inner (append current-list (list (hexagonal-number (+ 1 (length current-list))))) size)]
        ; якщо поточний список - заданого розміру, повернути його
        [(= (length current-list) size)
         current-list]
 )
)

; процедура для обчислення суми парних елементів списку
(define (even-member-sum list)
  (even-member-sum-inner list 0)
)

; внутрішня процедура для обчислення суми парнихз елементів у поточному підсписку
(define (even-member-sum-inner list current-sum)
  ; якщо поточний - кінцевий - список порожій, повернути поточну суму
  (cond [(null? list)
         current-sum]
        ; якщо перший елемент поточного списку - парний, викликати процедуру для решти списку та поточної суми + перший елемент
        [(even? (car list))
         (even-member-sum-inner (cdr list) (+ (car list) current-sum))]
        ; якщо перший елемент поточного списку - не парний, викликати процедуру для решти списку та поточної суми
        [(odd? (car list))
         (even-member-sum-inner (cdr list) current-sum)]
  ) 
)

; процедура для заміни елементів згідно з умовою
(define (replace-elements current-list)
  ; якщо поточний - кінцевий - список порожій, повернути поточний список
  (cond [(null? current-list)
         current-list]
        ; якщо поточний список починається з числа, кратного 5, повернути список, утворений з початкового елементу, помноженого на 10, та списку, отриманого викликом процедури для решти списку 
        [(= (modulo (car current-list) 5) 0)
         (append (list (* 10 (car current-list))) (replace-elements (cdr current-list)))]
        ; інакше повернути список, утворений з початкового елементу та списку, отриманого викликом процедури для решти списку        
        [else
         (append (list (car current-list)) (replace-elements (cdr current-list)))]
  ) 
)

; процедура для формування підсписку згідно з умовою
(define (get-subset current-list)
  ; якщо поточний - кінцевий - список порожій, повернути поточний список
  (cond [(null? current-list)
         current-list]
        ; якщо поточний список починається з числа, кратного 10, повернути список, утворений з початкового елементу та списку, отриманого викликом процедури для решти списку 
        [(= (modulo (car current-list) 10) 0)
         (append (list (car current-list)) (get-subset (cdr current-list)))]
        ; інакше повернути список, отриманоий викликом процедури для решти списку         
        [else
         (get-subset (cdr current-list))]
  ) 
)


(define size 20)
(define starting-list (make-list size))
(display "The starting list:\n")
(display starting-list)
(display "\n")
(display "Sum of the list's even members:\n")
(display (even-member-sum starting-list))
(display "\n")
(display "The list with all its members that are multiples of 5, multiplied by 10:\n")
(display (replace-elements starting-list))
(display "\n")
(display "The elements of the list that are multiples of 10:\n")
(display (get-subset starting-list))