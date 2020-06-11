;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 7

;; positivos : List(Number) -> List(Number)
;; Dada una lista de números l, devuelve una lista con aquellos mayores a 0.

(check-expect (positivos empty) empty)
(check-expect (positivos (list -1 2 -3 -4 5)) (list 2 5))

(define (positivos l)
  (filter positive? l))

;; prod : List(Number) -> Number
;; Dada una lista de números, multiplica los elementos de la lista entre sí.
;; Para la lista vacía, devuelve 1.

(check-expect (prod empty) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)

(define (prod l)
  (foldr * 1 l))

;; multPos : List(Number) -> Number
;; Dada una lista de números l, multiplica entre sí los números positivos de l.

(check-expect (multPos empty) 1)
(check-expect (multPos (list 3 -2 4 0 1 -5)) 12)

(define (multPos l)
  (prod (positivos l)))