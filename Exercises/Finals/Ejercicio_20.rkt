;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_20) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 20

;; suma-cubos : List(Number) -> Number
;; Dada una lista de números l, devuelva la suma de los cubos de los números positivos de l.

(check-expect (suma-cubos '()) 1)
(check-expect (suma-cubos (list 1 2 3 -1)) 36)
(check-expect (suma-cubos (list 1 0 -3 5 -12 -16)) 126)

(define (suma-cubos l)
  (cond [(empty? l) 1]
        [else (foldr + 0 (map (lambda (x) (expt x 3)) (filter positive? l)))]))