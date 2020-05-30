;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 12

;; Una Lista-de-numeros es:
;; – empty
;; – (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 0 3 2 5 7 6))
(define LISTA2 (list 1 3 5 7 9))

;; pares : Lista-de-numeros -> Lista-de-numeros
;; Dada una lista de números l,
;; devuelve una lista con los números pares de l.

(check-expect (pares empty) empty)
(check-expect (pares LISTA1) (list 0 2 6))
(check-expect (pares LISTA2) empty)

(define (pares l)
  (cond [(empty? l) empty]
        [else (if (even? (first l))
                  (cons (first l) (pares (rest l)))
                  (pares (rest l)))]))