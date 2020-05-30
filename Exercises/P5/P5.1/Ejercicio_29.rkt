;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_29) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 29

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 1 2 3 4 5))
(define LISTA2 (list -1 1 -1 3))

;; sumcuad : Lista-de-numeros -> Number
;; Dada una lista de números, devuelve la suma de sus cuadrados.
;; Para la lista vacía, devuelve 0.

(check-expect (sumcuad empty) 0)
(check-expect (sumcuad LISTA1) 55)
(check-expect (sumcuad LISTA2) 12)

(define (sumcuad l)
  (cond [(empty? l) 0]
        [else (+ (sqr (first l)) (sumcuad (rest l)))]))