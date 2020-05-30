;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 8

;; Una Lista-de-numeros es:
;; – empty
;; – (cons Numero Lista-de-numeros)

;; Ejemplos:
(define LISTA1 (list -1 2 -3 4 -5))
(define LISTA2 (list 1 5 10 15 20))
(define LISTA3 (list 0 1 10 15 20))

;; pos? : Lista-de-numeros -> Boolean
;; Dada una lista de numeros, determina si todos los elementos de la lista
;; son positivos.

(check-expect (pos? empty) #t)
(check-expect (pos? LISTA1) #f)
(check-expect (pos? LISTA2) #t)
(check-expect (pos? LISTA3) #f)

(define (pos? l)
  (cond [(empty? l) #t]
        [else (if (positive? (first l)) (pos? (rest l)) #f)]))

;; Evaluar las siguientes expresiones:
;; (pos? (cons 5 '()))
;; (pos? (cons -1 '()))

;; -------------------------------------
;; suma : Lista-de-numeros -> Number
;; Dada una lista de numeros, devuelve la suma de los mismos.

(check-expect (suma empty) 0)
(check-expect (suma LISTA1) -3)
(check-expect (suma LISTA2) 51)
(check-expect (suma LISTA3) 46)

(define (suma l)
  (cond [(empty? l) 0]
        [else (+ (first l) (suma (rest l)))]))

;; -------------------------------------
(define MENSAJE-ERROR-LISTA-MONTOS "La lista NO pertenece a Lista-de-montos")

;; checked-suma : Lista-de-numeros -> Number/String
;; Recibe como entrada una Lista-de-numeros y devuelve la suma de sus elementos
;; si la lista pertenece a Lista-de-montos. Caso contrario deberá devolver un
;; string indicando un error.

(check-expect (checked-suma empty) 0)
(check-expect (checked-suma LISTA1) MENSAJE-ERROR-LISTA-MONTOS)
(check-expect (checked-suma LISTA2) 51)
(check-expect (checked-suma LISTA3) MENSAJE-ERROR-LISTA-MONTOS)

(define (checked-suma l)
  (if (pos? l) (suma l) MENSAJE-ERROR-LISTA-MONTOS))