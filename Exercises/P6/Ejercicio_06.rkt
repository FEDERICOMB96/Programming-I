;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 6

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; intervalo-aux : Natural -> List(Natural)
;; Dado un número natural n, devuelve la lista (list n (n-1) ... 1).
;; Para 0 devuelve '().

(check-expect (intervalo-aux 0) '())
(check-expect (intervalo-aux 3) (list 3 2 1))

(define (intervalo-aux n)
  (cond [(zero? n) '()]
        [else (cons n (intervalo-aux (sub1 n)))]))

;; intervalo : Natural -> List(Natural)
;; Dado un número natural n, devuelve la lista (list 1 2 ... n).
;; Para 0 devuelve '().

(check-expect (intervalo 0) '())
(check-expect (intervalo 3) (list 1 2 3))

(define (intervalo n)
  (reverse (intervalo-aux n)))