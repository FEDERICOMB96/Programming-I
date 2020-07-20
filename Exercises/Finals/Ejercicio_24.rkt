;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_24) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 24

;; f : Natural Natural -> Boolean

(check-expect (f 4 2) #f)
(check-expect (f 4 3) #t)
(check-expect (f 9 4) #t)

(define (f n c)
  (cond [(< n (* c c)) #t]
        [(zero? (modulo n c)) #f]
        [else (f n (add1 c))]))

;; prime? : Natural -> Boolean
;; Dado un número natural, devuelve si es primo o no.

(check-expect (prime? 5) #t)
(check-expect (prime? 22) #f)

(define (prime? n) (f n 2))

;; lista-primos : Natural
;; Dado un número natural n, devuelve una lista con todos los primos desde 2 a n inclusive
;; (en caso que lo fuere) en orden decreciente.

(check-expect (lista-primos 8) (list 7 5 3 2))
(check-expect (lista-primos 23) (list 23 19 17 13 11 7 5 3 2))

(define (lista-primos n)
  (cond [(zero? (sub1 n)) empty]
        [(prime? n) (cons n (lista-primos (sub1 n)))]
        [else (lista-primos (sub1 n))]))

;; goldbach-aux : Natural List(Natural)-> posn
;; Dado un número par n >= 4 y una lista de numeros primos entre 2 y n inclusive
;; (en caso que lo fuere), devuelve dos números primos cuya suma resulten n.

(check-expect (goldbach-aux 18 (list 2 3 5 7 11 13 17)) (make-posn 5 13))
(check-expect (goldbach-aux 22 (list 2 3 5 7 11 13 17 19)) (make-posn 3 19))

(define (goldbach-aux n l)
  (cond [(prime? (- n (first l))) (make-posn (first l) (- n (first l)))]
        [else (goldbach-aux n (rest l))]))

;; goldbach : Natural -> posn
;; Dado un número par n >= 4 encuentra dos números primos cuya suma resulten n.

(check-expect (goldbach 14) (make-posn 3 11))
(check-expect (goldbach 18) (make-posn 5 13))
(check-expect (goldbach 22) (make-posn 3 19))

(define (goldbach n)
  (goldbach-aux n (reverse (lista-primos n))))