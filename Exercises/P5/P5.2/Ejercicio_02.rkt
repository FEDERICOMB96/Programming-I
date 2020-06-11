;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_02) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 2

;; -----------------
;; Ejercicio 18

;; raices : List(Number) -> List(Number)
;; Dada una lista de números,
;; devuelve una lista con las raíces cuadradas de sus elementos.

(check-expect (raices empty) empty)
(check-expect (raices (list 1 4 9 16 25)) (list 1 2 3 4 5))

(define (raices l)
  (map sqrt l))

;; -----------------
;; Ejercicio 19

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

;; -----------------
;; Ejercicio 20

;; anchos : List(Image) -> List(Number)
;; Dada una lista de imágenes, devuelve una lista con el ancho de cada una.

(check-expect (anchos empty) empty)
(check-expect (anchos (list (circle 20 "solid" "blue") (rectangle 30 20 "solid" "red")))
              (list 40 30))

(define (anchos l)
  (map image-width l))

;; -----------------
;; Ejercicio 21

;; sgn2 : Number -> Number
;; Dado un número, devuelve:
;; - Si es mayor a 0: 1
;; - Si es igual a 0: 0
;; - Si es menor a 0: -1

(check-expect (sgn2 31) 1)
(check-expect (sgn2 0) 0)
(check-expect (sgn2 -3) -1)

(define (sgn2 x)
  (cond [(< x 0) -1]
        [(= x 0) 0]
        [(> x 0) 1]))

;; signos : List(Number) -> List(Number)
;; Dada una lista de números, devuelve una lista con el resultado de
;; aplicarle a cada elemento la función sgn2.

(check-expect (signos empty) empty)
(check-expect (signos (list -3 0 2 3 -5 4)) (list -1 0 1 1 -1 1))

(define (signos l)
  (map sgn2 l))

;; -----------------
;; Ejercicio 22

;; cuadrados : List(Number) -> List(Number)
;; Dada una lista de números, devuelve la lista que resulta de elevar al
;; cuadrado cada uno de los elementos de la lista original.

(check-expect (cuadrados empty) empty)
(check-expect (cuadrados (list 1 2 3)) (list 1 4 9))

(define (cuadrados l)
  (map sqr l))

;; -----------------
;; Ejercicio 23

;; longitudes : List(String) -> List(Number)
;; Dada una lista de cadenas, devuelve la lista de sus longitudes.

(check-expect (longitudes empty) empty)
(check-expect (longitudes (list "Cuantas" "copas" "tenes?")) (list 7 5 6))

(define (longitudes l)
  (map string-length l))

;; -----------------
;; Ejercicio 24

;; Fahrenheit-a-Celsius : Number -> Number
;; Dada una temperatura en grados Fahrenheit, devuelve
;; su equivalente en grados Celsius.

(check-expect (Fahrenheit-a-Celsius 32) 0)
(check-expect (Fahrenheit-a-Celsius 212) 100)

(define (Fahrenheit-a-Celsius t)
  (* (- t 32) (/ 5 9)))

;; convertirFC : List(Number) -> List(Number)
;; Dada una lista de temperaturas en grados Fahrenheit,
;; devuelve esta lista de temperaturas convertidas en grados Celsius.

(check-expect (convertirFC empty) empty)
(check-expect (convertirFC (list 32 140 212)) (list 0 60 100))

(define (convertirFC l)
  (map Fahrenheit-a-Celsius l))