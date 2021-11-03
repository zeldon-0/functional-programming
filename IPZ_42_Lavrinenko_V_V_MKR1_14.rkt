#lang racket

; Лавріненко В.В.
; ІПЗ-42
; МКР №1, практична частина
; IDE: DrRacket

; Початок проміжку
(define start 0)
; Кінець проміжку
(define end pi)
; Процедура обчислення значення заданої функції
(define (f x)
  (sin (* 2 (cos x)))
)

; Процедура обчислення інтегралу за методом середніх прямокутників
(define (middle-rectangles a b n)
  ; Визначення величини кроку
  (let* ([step (/(- b a)n)]
    ; Обчислення суми значень f((xk + xk+1)/2), для точок на проміжку i=0...n-1 
    [sum (middle-rectangle-sum a (- b step) step 0)])
    ; Повернення кінцевого значення як добутку кроку на суму значень функції 
    (* step sum)
    )
)

; Процедура обчислення суми значень f((xk + xk+1)/2) для всіх x на заданому проміжку
(define (middle-rectangle-sum current-x end-x step current-sum)
   ; Поточне значення визначається як f(x + step / 2)
   (let ((current-f (f (+ current-x  (/ step 2)))))
     ; Якщо x вийшло за заданий проиіжок, повертається сума, отримана попереднім викликом процедури
     (if (> current-x end-x)
         current-sum
         ; Інакше рекурсивно обчислюється сума для подальших x на проміжку
         (middle-rectangle-sum (+ current-x step) end-x step (+ current-sum current-f))
     )
   )
)
; Процедура для обчислення інтегралу за методом Сімпсона на заданому проміжку
(define (simpson a b n)
  ; Визначення величини кроку
  (let* ([step (/ (- b a) n)]
    ; Обчислення суми виразу, наведеного у формулі, для всіх x на проміжку від a до b
    [sum (simpson-step a b step 0)])
    ; Повернення кінцевого результату як вищезгадана сума, помножена на третину кроку 
    (* (/ step 3) sum)
  )
)

; Процедура для обчислення суми виразу, наведеного у формулі методу Сімпсона
(define (simpson-step current-x end-x step current-sum)
     ; Визначення величини в заданій точці
     (let* ([start-f (f current-x)]
            ; Визначення величини в точці, отриманій додаванням кроку до заданої
            [middle-f (f (+ current-x step))]
            ; Визначення величини в точці, отриманій додаванням 2 величин кроку до заданої
            [end-f (f (+ current-x step step))]
            ; Визначення величини суми для поточної ітерації
            [step-sum (+ start-f (* 4 middle-f) end-f)])
     ; Якщо x вийшло за заданий проиіжок, повертається сума, отримана попереднім викликом процедури
     (if (> current-x end-x)
         current-sum
         ; Інакше рекурсивно обчислюється сума для подальших x на проміжку
         (simpson-step (+ current-x step step) end-x step (+ current-sum step-sum))
     )
   )
)

; Кількість проміжків, для яких виконуватимуться обидва методи
(define steps 1000)
; Обчислення інтегралу за методом середніх прямокутників
(define middle-rectangle-solution (middle-rectangles start end steps))
; Обчислення інтегралу за методом Сімпсона
(define simpson-solution (simpson start end steps))


(display "Solution using the Middle Rectangle method: ")
(display middle-rectangle-solution)
(display "\n")
(display "Solution using the Simpson method: ")
(display simpson-solution)
(display "\n")
(display "Solutions difference: ")
(display (abs (-  middle-rectangle-solution simpson-solution)))

; При встановленій кількості проміжків 1000 результати відрізняються  на 0.005713318639527355, що, в цілому, є відносно хорошим показником точності.
; Тим не менш, можемо судити, що метод Сімпсона, ймовірно, дає менш точні результати, в той час як метод середніх прямокутників дає результат, ща наближаєтсья до нуля,
; що відповідає графіку: 2 ділянки, площа яких обчислюється, та додається з протилежними знаками, є візулаьно симетричними, отже можемо судити, що результат дійсно має бути 0