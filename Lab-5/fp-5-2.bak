#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 4, завдання 14.2

; кількість товару в кожній точці
(define store-stock (list 50 70 100 30 40 90))
; тривалість очікування кожною точкою наступної поставки товару
(define store-restock-wait (list 0 0 0 0 0 0))
; статус відправки товару в кожну точку
(define store-restock-wait-status (list #f #f #f #f #f #f))
; кількість товару, що поставляєтсья в кожну точку
(define store-restock-size 65)
; кількість товару, що продається точкою за день
(define store-stock-loss 10)
; тривалість очікуання на поставку товару в точку
(define store-restock-delay 6)
; кількість товару в оптовому магазині
(define warehouse-stock 300)
; кількість товару, що поставляється в оптовий магазин за раз
(define warehouse-restock-size 1000)
; тривалість поставки товару в оптовий магазин
(define warehouse-restock-delay 90)
; тривалість часу між замовленнями поставок товару в оптовий магазин
(define warehouse-restock-frequency 14)
; черга з тривалості часу до поставки товару в оптовий магазин
(define warehouse-restock-queue '())
; дефіцит товару в оптових точках за день
(define deficit 0)
; накопичений дефіцит товару в оптових точках за тривалість симуляції
(define overall-deficit 0)

; процедура для отримання значення у списку за індексом
(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

; процедура для встановлення значення у списку за індексом
(define (set-list-element elements n value) 
  (if (= n 1) 
      (append (list value) (cdr elements))
         (append (list (car elements)) (set-list-element (cdr elements) (- n 1 ) value))
  )
)

; основна процедура для симуляції роздрібної торгівлі
(define (simulate days)
  (simulate-internal 1 days)
)

; внутрішня процедура для симуляції роздрібної торгівлі за заданий день
(define (simulate-internal current-day days)
  ; поки не настав кінецвий день, викликаються процедури для симуляції окремих процесів торгівлі
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
        ; якщо настав кінецвий день, викликається процедура обчислення незадоволеного попиту за останній день та виведення інформації про незадоволений попит в цілому 
        [else
         (begin
           (calculate-deficit 1)
           (print-deficit)
         )]
  )
)
; процедура симуляції продажу товару за день
(define (sell-stock)
  (sell-stock-internal 1)
)

; внутрішня процедура симуляції продажу однією точкою товару за день
(define (sell-stock-internal n)
  ; товар продається доти, доки поточний індекс належить списку, і поточна точка має товар
  (cond[(and (< n 7) (or (> (get-list-element store-stock n)  store-stock-loss) (= (get-list-element store-stock n)  store-stock-loss)))
        (begin
        (set! store-stock (set-list-element store-stock n (- (get-list-element store-stock n) store-stock-loss)))
        (sell-stock-internal (+ n 1))
        )]
       ; якщо поточний індекс належить списку, але поточна точка має дефіцит товару, незадоволений попит додається до поточного показника незадоволеного попиту
       [(and (< n 7) (< (get-list-element store-stock n)  store-stock-loss))
        (begin
        (set! overall-deficit (+ overall-deficit (- store-stock-loss (get-list-element store-stock n))))
        (set! store-stock (set-list-element store-stock n 0))
        (sell-stock-internal (+ n 1))
        )]
   )
)

; процедура симуляції поставки товару в точки
(define (send-stock)
  (send-stock-internal 1)
)

; внутрішня процедура симуляції поставки товару в поточну точку
(define (send-stock-internal n)
  ; якщо поточна точка не очікує на поставку та час очікування становить 0, встановити показник очікування в істине значення, а тривалість очікування - у задане значення
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

; процедура симуляції очікування поставки товару в точки
(define (await-stock)
  (await-stock-internal 1)
)

; внутрішня процедура симуляції очікування поставки товару в задану точку
(define (await-stock-internal n)
  ; якщо тривалість очікування на товар не є нульовою, зменшити тривалість на 1
  (cond[(and (< n 7) (> (get-list-element  store-restock-wait n) 0))
        (begin
        (set! store-restock-wait (set-list-element store-restock-wait n (- (get-list-element  store-restock-wait n) 1)))
        (await-stock-internal (+ n 1))
        )]
      ; якщо тривалість очікування на товар є нульовою, і показник очікування поставлено в істине значення, додати до кількості товару на точці задану кількість, а показник зробити хибним 
       [(and (< n 7) (= (get-list-element  store-restock-wait n) 0) (get-list-element  store-restock-wait-status n))
        (begin
        (set! store-stock (set-list-element store-stock n (+ (get-list-element  store-stock n) store-restock-size)))
        (set! store-restock-wait-status (set-list-element store-restock-wait-status n #f))
        (await-stock-internal (+ n 1))
        )]
   )
)

; процедура симуляції відправлення товару в оптовий магазин
(define (send-warehouse-restock-order current-day)
  ; з заданою періодичністю викликається внутрішня процедура
  (cond[(= (modulo current-day warehouse-restock-frequency) 0)
        (send-warehouse-restock-order-internal)]
  )
)

; внутрішня процедура симуляції відправлення товару в оптовий магазин
(define (send-warehouse-restock-order-internal)
  ; до списку очікуваних поставок додається нова - з заданою тривалістю
  (set! warehouse-restock-queue (append warehouse-restock-queue (list warehouse-restock-delay)))
)

; процедура симуляції очікування оптовим магазином на поставку товару
(define (await-warehouse-restock)
  (await-warehouse-restock-internal 1)
)

; внутрішня процедура симуляції очікування оптовим магазином на поставку товару
(define (await-warehouse-restock-internal n)
  ; якщо очікування на поточну поставку не скінчилось, зменшити його тривалість на 1
  (cond[(and (< n (+ (length warehouse-restock-queue) 1)) (> (get-list-element  warehouse-restock-queue n) 0))
        (begin
        (set! warehouse-restock-queue (set-list-element warehouse-restock-queue n (- (get-list-element  warehouse-restock-queue n) 1)))
        (await-warehouse-restock-internal  (+ n 1))
        )]
       ; якщо очікування на поточну поставку скінчилось, прибрати перший елемент списку очікування, а кількість товару в оптовому магазині збільшити
       [(and (= n 1) (not (null? warehouse-restock-queue)) (= (get-list-element warehouse-restock-queue n) 0))
        (begin
        (set! warehouse-stock (+ warehouse-stock warehouse-restock-size))
        (set! warehouse-restock-queue (cdr warehouse-restock-queue))
        (await-warehouse-restock-internal  (+ n 1))
        )]
   )
)

; процедура обчислення кількості незадоволеного попиту між точками за день 
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

; процедура друку інформації про незадоволений попит  
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

; процедура друку інформації про результати продажів за поточний день 
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