;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_21) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 21

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 45 32 -23 0 12))
(define LISTA2 (list 0 -12 321 1 -2))

;; sgn2 : Number -> Number
;; Dado un número, devuelve:
;; - Si es mayor a 0: 1
;; - Si es igual a 0: 0
;; - Si es menor a 0: -1

(check-expect (sgn2 31) 1)
(check-expect (sgn2 0) 0)
(check-expect (sgn2 -3) -1)

(define (sgn2 x)
  (cond [(< x 0) -1]
        [(= x 0) 0]
        [(> x 0) 1]))

;; signos : Lista-de-numeros -> Lista-de-numeros
;; Dada una lista de números, devuelve una lista con el resultado
;; de aplicarle a cada elemento la función sgn2.

(check-expect (signos LISTA1) (list 1 1 -1 0 1))
(check-expect (signos LISTA2) (list 0 -1 1 1 -1))

(define (signos l)
  (cond [(empty? l) empty]
        [else (cons (sgn2 (first l)) (signos (rest l)))]))