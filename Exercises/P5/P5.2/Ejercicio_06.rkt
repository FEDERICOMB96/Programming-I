;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 6

;; distancia : Posn -> Number
;; Dado un punto del plano, devuelve su distancia al origen.

(check-expect (distancia (make-posn 0 0)) 0)
(check-expect (distancia (make-posn 3 4)) 5)

(define (distancia p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; distancias : List(Posn) -> List(Number)
;; Dada una lista de puntos del plano,
;; devuelve una lista con la distancia al origen de cada uno.

(check-expect (distancias empty) empty)
(check-expect (distancias (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5)))
              (list 5 4 13))

(define (distancias l)
  (map distancia l))

;; suma : List(Number) -> Number
;; Suma todos los elementos de una lista de números.

(check-expect (suma (list 1 2 3 4 5)) 15)
(check-expect (suma empty) 0)
(check-expect (suma (list 11 13 9)) 33)

(define (suma l) (foldr + 0 l))

;; sumdist : List(Posn) -> Number
;; Dada una lista l de estructuras posn, devuelve la suma de las distancias
;; al origen de cada elemento de l.

(check-expect (sumdist empty) 0)
(check-expect (sumdist (list (make-posn 3 4) (make-posn 0 3))) 8)

(define (sumdist l)
  (suma (distancias l)))