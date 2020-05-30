;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 10

;; Una Lista es:
;; - empty
;; - (cons Any Lista)
;; Una lista cualquiera.

(define LISTA1 (list "Hello" "Friend?" "That's" "Lame"))
(define LISTA2 (list #t #f #f #t #t))
(define LISTA3 (list 0 3 0 3 4 5 6))

;; cant-elementos : Lista -> Number
;; Dada una lista, devuelve la cantidad de elementos de la misma.

(check-expect (cant-elementos empty) 0)
(check-expect (cant-elementos LISTA1) 4)
(check-expect (cant-elementos LISTA2) 5)
(check-expect (cant-elementos LISTA3) 7)

(define (cant-elementos l)
  (cond [(empty? l) 0]
        [else (+ 1 (cant-elementos (rest l)))]))