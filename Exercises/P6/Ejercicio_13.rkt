;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 13

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; g : Natural (Natural -> Boolean) -> Boolean
;; Dado un número natural n y una función f: Natural -> Boolean,
;; devuelve #true si y sólo si alguno de los valores (f 0) ,(f 1) ,..., (f n) es #true.

(check-expect (g 0 positive?) #false)
(check-expect (g 3 negative?) #false)
(check-expect (g 7 even?) #true)

(define (g n f)
  (cond [(zero? n) (f 0)]
        [else (or (f n) (g (sub1 n) f))]))