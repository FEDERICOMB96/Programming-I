;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_25) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 25

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 1 2 3 4 5))
(define LISTA2 (list 2 2 2 2 2))

;; prod : Lista-de-numeros -> Number
;; Dada una lista de números, multiplica sus elementos entre sí.
;; Para la lista vacía, devuelve 1.

(check-expect (prod empty) 1)
(check-expect (prod LISTA1) 120)
(check-expect (prod LISTA2) 32)

(define (prod l)
  (cond [(empty? l) 1]
        [else (* (first l) (prod (rest l)))]))