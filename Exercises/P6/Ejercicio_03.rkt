;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 3

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; sumanat : Natural Natural -> Natural
;; Toma dos números naturales y devuelve un natural que es la suma de ambos.

(check-expect (sumanat 1 0) 1)
(check-expect (sumanat 0 2) 2)
(check-expect (sumanat 3 2) 5)

(define (sumanat m n)
  (cond [(zero? n) m]
        [else (sumanat (add1 m) (sub1 n))]))

;; multiplicar : Natural Natural -> Natural
;; Toma como entrada dos números naturales y devuelve su multiplicacion.

(check-expect (multiplicar 3 0) 0)
(check-expect (multiplicar 0 2) 0)
(check-expect (multiplicar 3 2) 6)

(define (multiplicar m n)
  (cond [(zero? n) 0]
        [else (sumanat m (multiplicar m (sub1 n)))]))

;; powernat : Natural Natural -> Natural
;; Toma dos números naturales y devuelve el resultado de elevar el primero
;; a la potencia del segundo.

(check-expect (powernat 0 0) 1)
(check-expect (powernat 2 0) 1)
(check-expect (powernat 0 2) 0)
(check-expect (powernat 4 2) 16)

(define (powernat m n)
  (cond [(zero? n) 1]
        [else (multiplicar m (powernat m (sub1 n)))]))