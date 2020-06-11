;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 9

;; no-negativo? : Number -> Bool
;; Dado un número, devuelve si es no negativo.

(check-expect (no-negativo? 0) #t)
(check-expect (no-negativo? 132) #t)
(check-expect (no-negativo? -21) #f)

(define (no-negativo? x)
  (not (negative? x)))

;; no-negativos? : List(Number) -> List(Number)
;; Dada una lista de números l, devuelve una lista con los números
;; no negativos de l.

(check-expect (no-negativos? empty) empty)
(check-expect (no-negativos? (list -1 2 -3 5 -4)) (list 2 5))

(define (no-negativos? l)
  (filter no-negativo? l))

;; raices : List(Number) -> List(Number)
;; Dada una lista de números l, devuelve una lista con las raíces cuadradas
;; de los números no negativos de l.

(check-expect (raices (list 16 -4 9 0)) (list 4 3 0))

(define (raices l)
  (map sqrt (no-negativos? l)))