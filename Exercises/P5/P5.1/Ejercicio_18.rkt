;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 18

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 9 16 4))
(define LISTA2 (list 25 4 1))

;; raices : Lista-de-numeros -> Lista-de-numeros
;; Dada una lista de números, devuelve una lista con las raíces
;; cuadradas de sus elementos.

(check-expect (raices empty) empty)
(check-expect (raices LISTA1) (list 3 4 2))
(check-expect (raices LISTA2) (list 5 2 1))

(define (raices l)
  (cond [(empty? l) empty]
        [else (cons (sqrt (first l)) (raices (rest l)))]))