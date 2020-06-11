;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 11

;; suma : List(Number) -> Number
;; Suma todos los elementos de una lista de números.

(check-expect (suma (list 1 2 3 4 5)) 15)
(check-expect (suma empty) 0)
(check-expect (suma (list 11 13 9)) 33)

(define (suma l) (foldr + 0 l))

;; -----------------
;; aux : Bool Bool -> Bool
;; Dados dos valores booleanos, devuelve si alguno de los dos es #true.

(check-expect (aux #t #f) #t)
(check-expect (aux #f #f) #f)

(define (aux e1 e2)
  (or e1 e2))

;; uno-verdadero : List(Bool) -> Bool
;; Dada una lista de valores booleanos, devuelve #true unicamente si al menos
;; uno de los elementos de la lista es #true.

(check-expect (uno-verdadero empty) #f)
(check-expect (uno-verdadero (list #f #f #f #f #f)) #f)
(check-expect (uno-verdadero (list #t #f #t #f #f)) #t)

(define (uno-verdadero l)
  (foldr aux #f l))

;; algun-pos : List(List(Number)) -> Bool
;; Dada una lista de listas de números, devuelve #true si y sólo si para alguna
;; lista la suma de sus elementos es positiva.

(check-expect (algun-pos empty) #f)
(check-expect (algun-pos (list (list 1 3 -4 -2) (list 1 2 3 -5) (list 4 -9 -7 8 -3))) #t)
(check-expect (algun-pos (list empty (list 1 2 3))) #t)
(check-expect (algun-pos (list (list -1 2 -3 4 -5) empty (list -3 -4))) #f)

(define (algun-pos l)
  (uno-verdadero (map (compose positive? suma) l))) ; tambien se podria usar
                                                    ;(map positive? (map suma l))