;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 7

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; factnat : Natural -> Natural
;; Toma un número natural y devuelve su factorial.
;; (El factorial de un número natural n se calcula haciendo 1 x 2 x ... x n.)

(check-expect (factnat 0) 1)
(check-expect (factnat 4) 24)
(check-expect (factnat 5) 120)

(define (factnat n)
  (cond [(zero? n) 1]
        [else (* n (factnat (sub1 n)))]))