;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 8

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
        [(= n 1) 1]  ; (= n 1) es equivalente (zero? (sub1 n))
        [else (+ (fibnat (sub1 n)) (fibnat (sub1 (sub1 n))))]))