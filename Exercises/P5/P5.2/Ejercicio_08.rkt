;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 8

;; absolutos : List(Number) -> List(Number)
;; Dada una lista de números l, devuelve una lista con los valores absolutos
;; de cada uno de los elementos de l.

(check-expect (absolutos empty) empty)
(check-expect (absolutos (list -1 2 -3 -4 5)) (list 1 2 3 4 5))

(define (absolutos l)
  (map abs l))

;; suma : List(Number) -> Number
;; Suma todos los elementos de una lista de números.

(check-expect (suma (list 1 2 3 4 5)) 15)
(check-expect (suma empty) 0)
(check-expect (suma (list 11 13 9)) 33)

(define (suma l) (foldr + 0 l))

;; sumAbs : List(Number) -> Number
;; Dada una lista de números, devuelve la suma de sus valores absolutos.

(check-expect (sumAbs empty) 0)
(check-expect (sumAbs (list 3 -2 4 0 1 -5)) 15)

(define (sumAbs l)
  (suma (absolutos l)))