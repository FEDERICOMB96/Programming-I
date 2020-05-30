;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_27) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 27

;; Una Lista-de-naturales es:
;; - empty
;; - (cons Number Lista-de-naturales)
;; Interpretación: una lista cuyos elementos son números naturales.

;; Ejemplos:
(define LISTA1 (list 23 543 325 0 75))
(define LISTA2 (list 42 420 666 69 0))

;; maximo : Lista-de-naturales -> Number
;; Devuelve el máximo de una lista de números naturales.
;; Para la lista vacía, devuelve 0.

(check-expect (maximo empty) 0)
(check-expect (maximo LISTA1) 543)
(check-expect (maximo LISTA2) 666)

(define (maximo l)
  (cond [(empty? l) 0]
        [else (max (first l) (maximo (rest l)))]))