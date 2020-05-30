;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 9

;; ListaB es:
;; - empty
;; - (cons Boolean ListaB)
;; Interpretación: un elemento de ListaB es una lista
;; cuyos elementos pertenecen al tipo Boolean.

;; todos-verdaderos : ListaB -> Boolean
;; Recibe como entrada una lista de valores booleanos y
;; devuelve #true únicamente si todos los elementos de la lista son #true.

(check-expect (todos-verdaderos empty) #t)
(check-expect (todos-verdaderos (list #t #t #t)) #t)
(check-expect (todos-verdaderos (list #t #t #f)) #f)

(define (todos-verdaderos l)
  (cond [(empty? l) #t]
        [else (and (first l) (todos-verdaderos (rest l)))]))

;; uno-verdadero : ListaB -> Boolean
;; Recibe como entrada una lista de valores booleanos y
;; devuelve #true si algún elemento de la lista es #true.

(check-expect (uno-verdadero empty) #f)
(check-expect (uno-verdadero (list #t #f #f)) #t)
(check-expect (uno-verdadero (list #f #f #f)) #f)

(define (uno-verdadero l)
  (cond [(empty? l) #f]
        [else (or (first l) (uno-verdadero (rest l)))]))