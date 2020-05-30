;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 22

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 1 2 3))
(define LISTA2 (list 6 5 4))

;; cuadrados : Lista-de-numeros -> Lista-de-numeros
;; Dada una lista de números, devuelve la lista que resulta de
;; elevar al cuadrado cada uno de los elementos de la lista original.

(check-expect (cuadrados LISTA1) (list 1 4 9))
(check-expect (cuadrados LISTA2) (list 36 25 16))

(define (cuadrados l)
  (cond [(empty? l) empty]
        [else (cons (sqr (first l)) (cuadrados (rest l)))]))