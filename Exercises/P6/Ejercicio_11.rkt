;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 11

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; componer : (Number -> Number) Natural Number -> Number
;; Dados una función f, un natural n, y un número x,
;; devuelve el resultado de aplicar n veces la función f a x.

(check-expect (componer sqr 0 3) 3)
(check-expect (componer sqr 2 5) 625)
(check-expect (componer add1 5 13) 18)
(check-expect (componer identity 4 7) 7)

(define (componer f n x)
  (cond [(zero? n) x]
        [else (f (componer f (sub1 n) x))]))