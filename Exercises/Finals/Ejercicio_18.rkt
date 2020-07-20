;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_18) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 18

;; perf-sqrt : List(Number) -> Number
;; Dada una lista l de números enteros devuelva el producto de las raíces cuadradas de aquellos
;; elementos de l que son cuadrados perfectos.

(check-expect (perf-sqrt empty) 1)
(check-expect (perf-sqrt (list 1 16 9)) 12)
(check-expect (perf-sqrt (list 1 4 49)) 14)
(check-expect (perf-sqrt (list 12 25 19 144 7)) 60)

(define (perf-sqrt l)
  (foldr * 1 (map sqrt (filter (lambda (n) (integer? (sqrt n))) l))))