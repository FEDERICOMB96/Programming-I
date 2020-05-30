;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 16

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list -5 37 -23 0 12))
(define LISTA2 (list -1 0 -2 -5 -31))

;; positivos : Lista-de-numeros -> Lista-de-numeros
;; Recibe una lista de números y se quede sólo con aquellos que son mayores a 0.

(check-expect (positivos empty) empty)
(check-expect (positivos LISTA1) (list 37 12))
(check-expect (positivos LISTA2) empty)

(define (positivos l)
  (cond [(empty? l) empty]
        [else (if (positive? (first l))
                  (cons (first l) (positivos (rest l)))
                  (positivos (rest l)))]))