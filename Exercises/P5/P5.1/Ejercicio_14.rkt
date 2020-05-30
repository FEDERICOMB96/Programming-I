;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 14

;; Una Lista-de-numeros es:
;; – empty
;; – (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 3 1 4 1 5))
(define LISTA2 (list 1 3 2 6 9))

;; mayores : Lista-de-numeros Number -> Lista-de-numeros
;; Dada una lista de números l y un número n, devuelve una lista
;; con aquellos elementos de l que son mayores a n.

(check-expect (mayores empty 1) empty)
(check-expect (mayores LISTA1 10) empty)
(check-expect (mayores LISTA2 5) (list 6 9))

(define (mayores l n)
  (cond [(empty? l) empty]
        [else (if (> (first l) n)
                  (cons (first l) (mayores (rest l) n))
                  (mayores (rest l) n))]))
