;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 1

;; -----------------
;; Ejercicio 12

;; pares : List(Number) -> List(Number)
;; Dada una lista de números l, devuelve una lista con los números pares de l.

(check-expect (pares empty) empty)
(check-expect (pares (list 1 2 3 4 5)) (list 2 4))

(define (pares l)
  (filter even? l))

;; -----------------
;; Ejercicio 13

(define MAX 5)

;; corta? : String -> Bool
;; Dado un string, devuelve si su longitud es menor a MAX.

(check-expect (corta? "azul") #t)
(check-expect (corta? "hello") #f)

(define (corta? s)
  (< (string-length s) MAX))

;; cortas : List(String) -> List(String)
;; Dada una lista de strings l,
;; devuelve una lista con aquellas palabras de longitud menor a MAX.

(check-expect (cortas empty) empty)
(check-expect (cortas (list "Lista" "de" "palabras" "sin" "sentido")) (list "de" "sin"))
(check-expect (cortas (list "haha" "money" "printer" "go" "brr")) (list "haha" "go" "brr"))

(define (cortas l)
  (filter corta? l))

;; -----------------
;; Ejercicio 15

(define MAX-ORIGEN 5)

;; distancia : Posn -> Number
;; Dado un punto del plano, devuelve su distancia al origen.

(check-expect (distancia (make-posn 0 0)) 0)
(check-expect (distancia (make-posn 3 4)) 5)

(define (distancia p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; distancia-menor-MAX-ORIGEN? : Posn -> Bool
;; Dado un punto del plano,
;; devuelve si su distancia al origen es menor a la constante MAX-ORIGEN.

(check-expect (distancia-menor-MAX-ORIGEN? (make-posn 1 1)) #t)
(check-expect (distancia-menor-MAX-ORIGEN? (make-posn 3 4)) #f)

(define (distancia-menor-MAX-ORIGEN? p)
  (< (distancia p) MAX-ORIGEN))

;; cerca : List(Posn) -> List(Posn)
;; Dada una lista de puntos del plano,
;; devuelve la lista de aquellos puntos que están a distancia menor a MAX-ORIGEN.

(check-expect (cerca empty) empty)
(check-expect (cerca (list (make-posn 3 5) (make-posn 1 2) (make-posn 0 1) (make-posn 5 6)))
              (list (make-posn 1 2) (make-posn 0 1)))

(define (cerca l)
  (filter distancia-menor-MAX-ORIGEN? l))

;; -----------------
;; Ejercicio 16

;; positivos : List(Number) -> List(Number)
;; Dada una lista de números l, devuelve una lista con aquellos mayores a 0.

(check-expect (positivos empty) empty)
(check-expect (positivos (list -1 2 -3 -4 5)) (list 2 5))

(define (positivos l)
  (filter positive? l))