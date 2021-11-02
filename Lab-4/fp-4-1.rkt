#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 4, завдання 14.1

(define (hexagonal-number n)
  (-(* 2 n n) n)
)

(define (make-list size)
  (make-list-inner '() size)
)

(define (make-list-inner current-list size)
  (cond [(null? current-list)
         (make-list-inner (list (hexagonal-number 1)) size)]
        [(< (length current-list) size)
         (make-list-inner (append current-list (list (hexagonal-number (+ 1 (length current-list))))) size)]
        [(= (length current-list) size)
         current-list]
 )
)

(define (even-member-sum list)
  (even-member-sum-inner list 0)
)

(define (even-member-sum-inner list current-sum)
  (cond [(null? list)
         current-sum]
        [(even? (car list))
         (even-member-sum-inner (cdr list) (+ (car list) current-sum))]
        [(odd? (car list))
         (even-member-sum-inner (cdr list) current-sum)]
  ) 
)

(define (replace-elements current-list)
  (cond [(null? current-list)
         current-list]
        [(= (modulo (car current-list) 5) 0)
         (append (list (* 10 (car current-list))) (replace-elements (cdr current-list)))]
        [else
         (append (list (car current-list)) (replace-elements (cdr current-list)))]
  ) 
)


(define (get-subset current-list)
  (cond [(null? current-list)
         current-list]
        [(= (modulo (car current-list) 10) 0)
         (append (list (car current-list)) (get-subset (cdr current-list)))]
        [else
         (get-subset (cdr current-list))]
  ) 
)



(define size 20)
(define starting-list (make-list size))
(display starting-list)
(display "\n")
(display (even-member-sum starting-list))
(display "\n")
(display (replace-elements starting-list))
(display "\n")
(display (get-subset starting-list))