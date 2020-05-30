;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 11

;; Una Lista es:
;; - empty
;; - (cons Any Lista)
;; Una lista cualquiera.

;; Ejemplos:
(define LISTA1 (list 1 2 3 4 5 6 7 8 9))
(define LISTA2 (list 100 300 200 200 0))

;; -------------------------------------
;; suma : Lista-de-numeors -> Number
;; Dada una lista de numeros, devuelve la suma de los mismos.

(check-expect (suma empty) 0)
(check-expect (suma LISTA1) 45)
(check-expect (suma LISTA2) 800)

(define (suma l)
  (cond [(empty? l) 0]
        [else (+ (first l) (suma (rest l)))]))

;; -------------------------------------
;; cant-elementos : Lista -> Number
;; Dada una lista, devuelve la cantidad de elementos de la misma.

(check-expect (cant-elementos empty) 0)
(check-expect (cant-elementos LISTA1) 9)
(check-expect (cant-elementos LISTA2) 5)

(define (cant-elementos l)
  (cond [(empty? l) 0]
        [else (+ 1 (cant-elementos (rest l)))]))

;; -------------------------------------
;; promedio : Lista -> Number
;; Dada una lista, devuelve el promedio de sus elementos.

(check-expect (promedio empty) 0)
(check-expect (promedio LISTA1) 5)
(check-expect (promedio LISTA2) 160)

(define (promedio l)
  (cond [(empty? l) 0]
        [else (/ (suma l) (cant-elementos l))]))