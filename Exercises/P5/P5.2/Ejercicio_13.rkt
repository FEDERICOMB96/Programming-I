;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_13) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 13

;; aux : Bool Bool -> Bool
;; Dados dos valores booleanos, devuelve si ambos son #true.

(check-expect (aux #t #t) #t)
(check-expect (aux #f #t) #f)

(define (aux e1 e2)
  (and e1 e2))

;; todos-verdaderos : List(Bool) -> Bool
;; Dada una lista de valores booleanos, devuelve #true unicamente si todos los
;; elementos de la lista son #true.

(check-expect (todos-verdaderos empty) #t)
(check-expect (todos-verdaderos (list #t #f #f #t #t)) #f)
(check-expect (todos-verdaderos (list #t #t #t #t #t)) #t)

(define (todos-verdaderos l)
  (foldr aux #t l))

;; todos-true : List(Any) -> Bool
;; Dada una lista de valores de cualquier tipo, devuelve #true si y sólo si
;; todos los valores booleanos de la lista son verdaderos.
;; Caso contrario, devuelve #false.

(check-expect (todos-true empty) #t)
(check-expect (todos-true (list 5 #true "abc" #true "def")) #t)
(check-expect (todos-true (list #true 1 "hawai" 50 '())) #t)
(check-expect (todos-true (list #f "42" "CSI MIAMI" 69)) #f)
(check-expect (todos-true (list 1 #true (circle 10 "solid" "red") -12 #false)) #f)

(define (todos-true l)
  (todos-verdaderos (filter boolean? l)))