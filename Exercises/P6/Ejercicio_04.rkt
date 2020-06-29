;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 4

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; sigma: Natural (Natural -> Number) -> Number
;; Dados un número natural n y una función f, devuelve la sumatoria de f
;; para los valores de 0 hasta n.

(check-expect (sigma 0 sqrt) 0)
(check-expect (sigma 4 sqr) 30)
(check-expect (sigma 10 identity) 55)

(define (sigma n f)
  (cond [(zero? n) (f 0)]
        [else (+ (f n) (sigma (sub1 n) f))]))