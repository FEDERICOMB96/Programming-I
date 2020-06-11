;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 12

;; mayor-a-4? : List(Any) -> Bool
;; Dada una lista cualquiera, devuelve si su longitud es mayor a 4.

(check-expect (mayor-a-4? empty) #f)
(check-expect (mayor-a-4? (list 1 2 3 4 5)) #t)

(define (mayor-a-4? l)
  (> (length l) 4))

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

;; long-lists : List(List(Any)) -> Bool
;; Dada una lista de listas, devuelve #true si y sólo si las
;; longitudes de todas las sublistas son mayores a 4.

(check-expect (long-lists empty) #t)
(check-expect (long-lists (list '() '() (list 1 2 3))) #f)
(check-expect (long-lists (list (list 1 2 3 4 5) empty)) #f)
(check-expect (long-lists (list (list 1 2 3 4 5)
                                (list 1 2 3 4 5 6)
                                (list 87 73 78 83 33))) #t)

(define (long-lists l)
  (todos-verdaderos (map mayor-a-4? l)))