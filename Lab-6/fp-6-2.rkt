#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 6, завдання 14.2

; функція пошуку елемента за його індексом
(define (get-list-element elements n) 
  (if (= n 1) 
      (car elements)
         (get-list-element (cdr elements) (- n 1 ))
  )
)

; функція вилучення першого елемента з черги
(define (pop-queue)
  (begin
    (define popped-number (car queue))
    (display "Removing ")
    (display popped-number)
    (display " from the queue.\n")
    (set! queue (cdr queue))
    (display "Current queue state: ")
    (display queue)
    (display "\n")
    popped-number
  )
)

; функція додавання останнього елемента в чергу
(define (push-queue n)
  (begin
    (display "Adding ")
    (display n)
    (display " to the queue.\n")
    (set! queue (append queue (list n)))
    (display "Current queue state: ")
    (display queue)
    (display "\n")
  )
)

; функція вилучення першого елемента зі стеку
(define (pop-stack)
  (begin
    (define popped-number (get-list-element stack (length stack)))
    (display "Removing ")
    (display popped-number)
    (display " from the stack.\n")
    (set! stack (pop-stack-inner stack))
    (display "Current stack state: ")
    (display stack)
    (display "\n")
    popped-number
  )
)

; функція отримання стеку без останнього його елемента
(define (pop-stack-inner current-stack)
  (cond [(> (length current-stack) 1)
           (append (list (car current-stack)) (pop-stack-inner (cdr current-stack)))
         ]
        [else
         '()
        ]
   )
)

; функція додавання останнього елемента в стек
(define (push-stack n)
  (begin
    (display "Adding ")
    (display n)
    (display " to the stack.\n")
    (set! stack (append stack (list n)))
    (display "Current stack state: ")
    (display stack)
    (display "\n")
  )
)

; функція, що підраховує кількість елементів, що відповідають заданому числу в списку
(define (number-of-occurences n current-list count)
  (cond
   [(not (null? current-list))
    ; якщо поточний елемент відповідає заданому, збільшити лічильник на 1, інакше з поточним лічильнимнком перевіряти наступні елементи
    (if (= n (car current-list))
        (number-of-occurences n (cdr current-list) (+ 1 count))
                (number-of-occurences n (cdr current-list) count)
    )
   ]
   [else
      count
    ]
  )
)

; функція вилучення елементів стеку і заповнення проміжного списку
(define (empty-the-stack)
  ; якщо стек вже порожній, вивести відповідне повідомлення
 (cond [(empty? stack)
           (begin
             (display "The resulting stack is: ")
             (display stack)
             (display "\n")
           )
          ]
         ; інакше прибрати наступний елемент
          [else
           (begin
             (define current-number (pop-stack))
             (set! temp-list (append temp-list (list current-number)))
             (empty-the-stack)
           )
          ]
  )
)

; функція наповнення вихідної черги, спираючись на список проміжних значень
(define (fill-the-queue source)
 ; Якщо всі еоементи вже обійшли, вивести відповідне повідомлення
 (cond [(empty? source)
           (begin
             (display "The resulting queue is: ")
             (display queue)
             (display "\n")
           )
          ]
          [else
           (begin
             (define current-number (car source))
             ; Якщо поточний елемент зустрічається у вихідному списку лише раз, записати його у чергу  
             (cond [(= ( number-of-occurences current-number temp-list 0) 1)
                    (push-queue current-number)
                    ]
                   ; Інакше вивести повідомлення про наявність дублікатів
                   [else
                    (begin
                      (display "Skipping ")
                      (display current-number)
                      (display " as it is used more than once.\n")
                    )
                   ]
             )
              ; Продовжити обхід для решти елементів
             (fill-the-queue (cdr source))
           )
          ]
  )
)

(define queue (list))
(define stack (list))
(define temp-list (list))
(push-stack 1)
(push-stack 2)
(push-stack 1)
(push-stack 4)
(push-stack 5)
(push-stack 3)
(push-stack 7)
(push-stack 11)
(push-stack 3)
(push-stack 12)
(push-stack 8)
(push-stack 1)
(empty-the-stack)
(fill-the-queue temp-list)