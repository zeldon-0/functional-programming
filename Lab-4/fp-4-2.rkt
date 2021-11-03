#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 4, завдання 14.2

(define store-stock (list 50 70 100 30 40 90))
(define store-restock-wait (list 0 0 0 0 0 0))
(define store-restock-wait-status (list #f #f #f #f #f #f))
(define store-restock-size 65)
(define store-stock-loss 10)
(define store-restock-delay 6)
(define warehouse-stock 300)
(define warehouse-restock-size 1000)
(define warehouse-restock-delay 90)
(define warehouse-restock-frequency 14)
(define warehouse-restock-queue '())
(define deficit 0)
(define overall-deficit 0)


(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

(define (set-list-element elements n value) 
  (if (= n 1) 
      (append (list value) (cdr elements))
         (append (list (car elements)) (set-list-element (cdr elements) (- n 1 ) value))
  )
)

(define (simulate days)
  (simulate-internal 1 days)
)

(define (simulate-internal current-day days)
  (cond [(< current-day days)
           (begin
             (sell-stock)
             (await-stock)
             (send-stock)
             (await-warehouse-restock)
             (send-warehouse-restock-order current-day)
             (print-status current-day)
             (simulate-internal (+ 1 current-day) days)
            )
         ]
        [else
         (begin
           (calculate-deficit 1)
           (print-deficit)
         )]
  )
)

(define (sell-stock)
  (sell-stock-internal 1)
)

(define (sell-stock-internal n)
  (cond[(and (< n 7) (or (> (get-list-element store-stock n)  store-stock-loss) (= (get-list-element store-stock n)  store-stock-loss)))
        (begin
        (set! store-stock (set-list-element store-stock n (- (get-list-element store-stock n) store-stock-loss)))
        (sell-stock-internal (+ n 1))
        )]
       [(and (< n 7) (< (get-list-element store-stock n)  store-stock-loss))
        (begin
        (set! overall-deficit (+ overall-deficit (- store-stock-loss (get-list-element store-stock n))))
        (set! store-stock (set-list-element store-stock n 0))
        (sell-stock-internal (+ n 1))
        )]
   )
)

(define (send-stock)
  (send-stock-internal 1)
)

(define (send-stock-internal n)
  (cond[(and (< n 7) (or (> warehouse-stock store-restock-size) (= warehouse-stock store-restock-size)) (= (get-list-element  store-restock-wait n) 0)
             (not(get-list-element  store-restock-wait-status n)))
        (begin
        (set! warehouse-stock (- warehouse-stock store-restock-size))
        (set! store-restock-wait (set-list-element store-restock-wait n  store-restock-delay))
        (set! store-restock-wait-status (set-list-element store-restock-wait-status n #t))
        (send-stock-internal (+ n 1))
        )]
   )
)

(define (await-stock)
  (await-stock-internal 1)
)

(define (await-stock-internal n)
  (cond[(and (< n 7) (> (get-list-element  store-restock-wait n) 0))
        (begin
        (set! store-restock-wait (set-list-element store-restock-wait n (- (get-list-element  store-restock-wait n) 1)))
        (await-stock-internal (+ n 1))
        )]
       [(and (< n 7) (= (get-list-element  store-restock-wait n) 0) (get-list-element  store-restock-wait-status n))
        (begin
        (set! store-stock (set-list-element store-stock n (+ (get-list-element  store-stock n) store-restock-size)))
        (set! store-restock-wait-status (set-list-element store-restock-wait-status n #f))
        (await-stock-internal (+ n 1))
        )]
   )
)

(define (send-warehouse-restock-order current-day)
  (cond[(= (modulo current-day warehouse-restock-frequency) 0)
        (send-warehouse-restock-order-internal)]
  )
)

(define (send-warehouse-restock-order-internal)
  (set! warehouse-restock-queue (append warehouse-restock-queue (list warehouse-restock-delay)))
)

(define (await-warehouse-restock)
  (await-warehouse-restock-internal 1)
)

(define (await-warehouse-restock-internal n)
  (cond[(and (< n (+ (length warehouse-restock-queue) 1)) (> (get-list-element  warehouse-restock-queue n) 0))
        (begin
        (set! warehouse-restock-queue (set-list-element warehouse-restock-queue n (- (get-list-element  warehouse-restock-queue n) 1)))
        (await-warehouse-restock-internal  (+ n 1))
        )]
       [(and (= n 1) (not (null? warehouse-restock-queue)) (= (get-list-element warehouse-restock-queue n) 0))
        (begin
        (set! warehouse-stock (+ warehouse-stock warehouse-restock-size))
        (set! warehouse-restock-queue (cdr warehouse-restock-queue))
        (await-warehouse-restock-internal  (+ n 1))
        )]
   )
)

(define (calculate-deficit n)
    (cond[(and (< n 7) (< (get-list-element  store-stock n) store-stock-loss))
          (begin 
          (set! deficit (+ deficit (- store-stock-loss (get-list-element  store-stock n))))
          (calculate-deficit (+ n 1))
          )]
         [(< n 7)
          (calculate-deficit (+ n 1))]
    )
)
  
(define (print-deficit)
  (begin
    (display "The amout of demand that will not be satisfied the next day: ")
    (display deficit)
    (display "\n")
    (display "Overall amout of demand that has not been satisfied: ")
    (display overall-deficit)
    (display "\n")
  )
)

  
(define (print-status current-day)
  (begin
    (display "Day ")
    (display current-day)
    (display "\n")
    (display "Current stock among the stores: ")
    (display store-stock)
    (display "\n")
    (display "Days left until the stores are restocked: ")
    (display store-restock-wait)
    (display "\n")
    (display "Current amount in-stock at the warehouse: ")
    (display warehouse-stock)
    (display "\n")
    (display "Pending factory orders: ")
    (display warehouse-restock-queue )
    (display "\n")
    (display "==============================================================\n")
  )
)

(display "\n")
(simulate 150)
(display "\n")




