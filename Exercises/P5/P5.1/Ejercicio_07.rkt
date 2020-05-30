;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 7

;; Una Lista-de-montos es:
;; – empty
;; – (cons NumeroPositivo Lista-de-montos)
;; Lista-de-montos representa una lista con montos de dinero.

;; Ejemplos:
(define LISTA1 (list 150 120 200 80 50))
(define LISTA2 (list 50 100 450 200 50))

;; suma : Lista-de-montos -> Number
;; Dada una lista de montos, devuelve la suma de los mismos.

(check-expect (suma empty) 0)
(check-expect (suma LISTA1) 600)
(check-expect (suma LISTA2) 850)

(define (suma l)
  (cond [(empty? l) 0]
        [else (+ (first l) (suma (rest l)))]))