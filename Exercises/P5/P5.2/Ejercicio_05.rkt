;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 5

; cuadrados : ListN -> ListN
; Calcula los cuadrados de todos los elementos de una lista de números.

(check-expect (cuadrados (list 1 2 3 4 5)) (list 1 4 9 16 25))
(check-expect (cuadrados empty) empty)
(check-expect (cuadrados (list 11 13 9)) (list 121 169 81))

(define (cuadrados l) (map sqr l))

; suma : ListN -> Number
; Suma todos los elementos de una lista de números.

(check-expect (suma (list 1 2 3 4 5)) 15)
(check-expect (suma empty) 0)
(check-expect (suma (list 11 13 9)) 33)

(define (suma l) (foldr + 0 l))

; sumcuad : ListN -> Number
; suma los cuadrados de una lista de números

(check-expect (sumcuad (list 1 2 3 4 5)) 55)
(check-expect (sumcuad empty) 0)
(check-expect (sumcuad (list 11 13 9)) 371)

(define (sumcuad l) (suma (cuadrados l)))