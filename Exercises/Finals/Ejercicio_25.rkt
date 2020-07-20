;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 25

;; sumandos : Natural -> List(Number)
;; Dado un número natural n, devuelve la lista de sumandos de la serie en orden inverso.

(check-expect (sumandos 0) empty)
(check-expect (sumandos 5) (list (/ 1 25) (/ 1 16) (/ 1 9) (/ 1 4) 1))

(define (sumandos n)
  (cond [(zero? n) empty]
        [else (cons (/ 1 (sqr n)) (sumandos (sub1 n)))]))

;; aprox-pi : Natural -> Number
;; Dado un número natural n, aproxime el valor de pi calculando los n primeros sumandos de la serie.

(check-expect (aprox-pi 0) 0)
(check-within (aprox-pi 100) pi 0.01)

(define (aprox-pi n)
  (sqrt (* 6 (foldr + 0 (sumandos n)))))