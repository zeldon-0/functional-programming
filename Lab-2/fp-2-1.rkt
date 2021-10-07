#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 2, завдання 14.1
; точність, за якою визначається закінчення процедури 
(define error 1e-9)
; кількість ітерацій, за якими припиняється процедура, у разі не досягнення бажаної точності
(define max-iterations 1000)

; процедура для обчислення заданої функції, спираючись на вбудовану поцедуру "sqrt"
(define (result-built-in value)
  (cond
    ; перша умова для обчислення значення
    [(and(>= value 1)(<= value 2))
       (sqrt(- 15 (* value value )))]
    ; друга умова для обчислення значення
    [(and(>= value -1)(< value  1))
       (/ 1 (sqrt (+ value (* value value))))]
  ))

; процедура для обчислення заданої функції, спираючись на власну реалізацію
(define (result value)
  (cond
    ; перша умова для обчислення значення
    [(and(>= value 1)(<= value 2))
       (custom-sqrt(- 15 (* value value )) 0 0)]
    ; друга умова для обчислення значення
    [(and(>= value -1)(< value  1))
       (/ 1 (custom-sqrt (+ value (* value value)) 0 0))]
  ))

; процедура для обчислення похибки при обчисленні заданої функції, спираючись на вбудовану та власну реалізації
(define (method-difference value)
  ; значення має повертатись лише для точок, що входять в межі, задані умовою, та для дійсних значень
  (cond[(and(not(void? (result-built-in value)))(real? (result-built-in value)))
  (abs(- (result value) (result-built-in value)))]
 )
)

; процедура для обчислення квадратного кореня, згідно з заданою формулою
(define (custom-sqrt x n previous-member )
  ; на кожному кроці обчислюється значення поточного члена послідовності
  (define current-member (custom-sqrt-internal x n previous-member))
  ; рекурсія завершується у разі, якщо досягнуто бажану точність, або ж виконано достатню кількість ітерацій
  (if (or (< (abs (- previous-member current-member)) error) (> n max-iterations))
      ; в разі закінчення рекурсії повертається значення поточного члена ряду
      current-member
      ; інакше рекурсивно викликається поточна процедура для наступного члена ряду
      (custom-sqrt x (+ n 1) current-member))
  )

; внутрішня процедура для обчислення значення поточного члена ряду
(define (custom-sqrt-internal x n previous-member)
    ; виняткове значення повертається для нульового члена ряду
    (if (= n 0)
       1
       ; інакше значення обчислюється згідно з формулою 
        (* 0.5 (+ previous-member (/ x previous-member)))
  )
)

(display "Результат з використанням вбудованої функції:\n")
(map  result-built-in (inclusive-range -2 2 0.5))
(display "Результат з використанням власної реалізації:\n")
(map  result (inclusive-range -2 2 0.5))
(display "Похибки:\n")
(map  method-difference (inclusive-range -2 2 0.5))
