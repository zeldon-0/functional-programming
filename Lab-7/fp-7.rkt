#lang racket
; Лавріненко В.В.
; ІПЗ-42
; Л.р. 7, завдання 14

(define (is-vowel char)  
  (cond
    [(eq? char #\a) #t]
    [(eq? char #\e) #t]	
    [(eq? char #\i) #t]
    [(eq? char #\o) #t]
    [(eq? char #\u) #t]
    [(eq? char #\y) #t]
    [else #f]
  )
)

(define (count-vowels word current-count index)
  (cond [(and (< index (string-length word)) (is-vowel (string-ref word index)) )
         (count-vowels word (+ 1 current-count) (+ 1 index))
         ]
        [(and (< index (string-length word)) (not (is-vowel (string-ref word index))))
         (count-vowels word current-count (+ 1 index))
         ]
        [else
         current-count]
  )
)

(define (find-maximum-vowel-word line index maximum-vowel-word)
  (cond [(and (< index (length line)) (> (count-vowels (list-ref line index) 0 0) (cdr maximum-vowel-word)) )
         (find-maximum-vowel-word line (+ 1 index) (cons index (count-vowels (list-ref line index) 0 0)))
         ]
        [(and (< index (length line)) (not (> (count-vowels (list-ref line index) 0 0) (cdr maximum-vowel-word))))
         (find-maximum-vowel-word line (+ 1 index) maximum-vowel-word)
         ]
        [else
         maximum-vowel-word]
  )
)

(define (create-number-string length [current-string ""])
  (cond [(< (string-length current-string) length)
         (create-number-string length (string-append current-string (number->string (modulo (+ 1 (string-length current-string)) 10))))]
        [else
         current-string]
  )
)

(define (create-modified-string line)
  (begin
    (define maximum-vowel-word (find-maximum-vowel-word line 0 (cons 0 0)))
    (define character-count (string-length (list-ref line (car maximum-vowel-word))))
    (define number-string (create-number-string character-count))
    (define modified-string (append (take line (car maximum-vowel-word)) (list number-string) (list-tail line (+ 1(car maximum-vowel-word)))))
    modified-string
  )
)

(delete-file "E:\\FP-files\\output.txt")
(delete-file "E:\\FP-files\\output1.txt")

(define output-port-initial (open-output-file "E:\\FP-files\\input.txt"))
(write '(The preach like a lullaby) output-port-initial)
(write '(A dark shadow equals an apocalyptic catastrophe) output-port-initial)
(write '(Distressed at the future disaster) output-port-initial)
(write '(The whole world is a fireball) output-port-initial)
(close-output-port output-port-initial)

(display "The initial rows:\n")
(define in (open-input-file "E:\\FP-files\\input.txt"))
(define str1 (map symbol->string (read in)))
(display (string-join str1 " "))
(newline)
(define str2 (map symbol->string (read in)))
(display (string-join str2 " "))
(newline)
(define str3 (map symbol->string (read in)))
(display (string-join str3 " "))
(newline)
(define str4 (map symbol->string (read in)))
(display (string-join str4 " "))
(newline)
(close-input-port in)

(delete-file "E:\\FP-files\\input.txt")

(display "================================\n")
(display "The modified rows:\n")
(define str1-modified (create-modified-string str1))
(display (string-join str1-modified " "))
(newline)
(define str2-modified (create-modified-string str2))
(display (string-join str2-modified " "))
(newline)
(define str3-modified (create-modified-string str3))
(display (string-join str3-modified " "))
(newline)
(define str4-modified (create-modified-string str4))
(display (string-join str4-modified " "))
(newline)

(define output-port-result (open-output-file "E:\\FP-files\\output.txt"))
(write (map string->symbol str1-modified) output-port-result)
(write (map string->symbol str2-modified) output-port-result)
(write (map string->symbol str3-modified) output-port-result)
(write (map string->symbol str4-modified) output-port-result)
(close-output-port output-port-result)

(display "================================\n")
(display "The combined initial rows:\n")
(define str-combined (append str1 str2 str3 str4))
(display str-combined)
(newline)
(display "================================\n")
(display "The modified combination of rows:\n")
(define str-combined-modified (create-modified-string str-combined))
(display str-combined-modified)

(define output-port-result-combined (open-output-file "E:\\FP-files\\output1.txt"))
(write (map string->symbol str-combined-modified) output-port-result-combined)
(close-output-port output-port-result-combined)
