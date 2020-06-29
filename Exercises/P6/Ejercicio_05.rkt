;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 5

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; sigma: Natural (Natural -> Number) -> Number
;; Dados un número natural n y una función f, devuelve la sumatoria de f
;; para los valores de 0 hasta n.

(check-expect (sigma 0 sqrt) 0)
(check-expect (sigma 4 sqr) 30)
(check-expect (sigma 10 identity) 55)

(define (sigma n f)
  (cond [(zero? n) (f 0)]
        [else (+ (f n) (sigma (sub1 n) f))]))

;; ----------------------
;; r : Natural -> Number

(check-expect (r 0) 1)
(check-expect (r 1) (/ 1 2))

(define (r i)
  (/ 1 (expt 2 i)))

;; R : Natural -> Number

(check-expect (R 0) 1)
(check-expect (R 1) (/ 3 2))

(define (R n) (sigma n r))

;; ----------------------
;; s : Natural -> Number

(check-expect (s 0) 0)
(check-expect (s 1) (/ 1 2))

(define (s i)
  (/ i (+ i 1)))

;; S : Natural -> Number

(check-expect (S 0) 0)
(check-expect (S 2) (/ 7 6))

(define (S n) (sigma n s))

;; ----------------------
;; t : Natural -> Number

(check-expect (t 0) 1)
(check-expect (t 1) (/ 1 2))

(define (t i)
  (/ 1 (+ i 1)))

;; T : Natural -> Number

(check-expect (T 0) 1)
(check-expect (T 2) (/ 11 6))

(define (T n) (sigma n t))