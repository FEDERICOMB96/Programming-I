;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 9

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; fibnat : Natural -> Natural
;; Toma un número natural y devuelve el valor correspondiente a la secuencia de
;; Fibonacci para ese valor:
;; - fibnat(0) = 1
;; - fibnat(1) = 1
;; - fibnat(n+2) = fibnat(n) + fibnat(n+1)

(check-expect (fibnat 0) 1)
(check-expect (fibnat 1) 1)
(check-expect (fibnat 4) 5)

(define (fibnat n)
  (cond [(zero? n) 1]
        [(= n 1) 1]
        [else (+ (fibnat (sub1 n)) (fibnat (sub1 (sub1 n))))]))

;; list-fibonacci : Natural -> List(Natural)
;; Dado un número n devuelve una lista con los primeros n+1 valores
;; de la serie de fibonacci, ordenados de mayor a menor.
;; Es decir, (list-fibonacci n) = (list (fib n) (fib (- n 1)) ... (fib 0))

(check-expect (list-fibonacci 0) (list 1))
(check-expect (list-fibonacci 4) (list 5 3 2 1 1))

(define (list-fibonacci n)
  (cond [(zero? n) (list 1)]
        [else (cons (fibnat n) (list-fibonacci (sub1 n)))]))