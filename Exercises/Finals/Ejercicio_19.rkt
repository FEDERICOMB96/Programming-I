;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 19

;; xor-even? : Number Boolean -> Boolean
;; Dado un número n y un booleano b, devuelve el resultado de aplicar xor a (even? n) y b.

(define (xor-even? n b)
  (or (and (even? n) (not b))
      (and (not (even? n)) b)))

#| Generalizanción de la función anterior.

(define (xor-f f)
  (lambda (n b)
    (or (and (f n) (not b))
      (and (not (f n)) b))))

|#

;; par-par? : List(Number) -> Boolean
;; Dada una lista l de números enteros devuelva #t si hay una cantidad par de números pares y #f en
;; caso contrario.

(check-expect (par-par? empty) #t)
(check-expect (par-par? (list 2 5 6)) #t)
(check-expect (par-par? (list 2 5 6 0 7)) #f)
(check-expect (par-par? (list 2 5 6 0 7 8 9 10)) #f)

(define (par-par? l)
  (foldr xor-even? #t l))

#| Otra solución posible, sin utilizar foldr

(define (par-par? l)
  (even? (length (filter even? l))))

|#