;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 22

;; f : Natural -> Natural
;; f(0) = 0
;; f(1) = 2
;; f(2) = 5
;; f(n) = f(n-1) + 2 * f(n-3), para todo n mayor o igual a 3.

(check-expect (f 0) 0)
(check-expect (f 1) 2)
(check-expect (f 2) 5)
(check-expect (f 3) 5)

(define (f n)
  (cond [(zero? n) 0]
        [(= n 1) 2]
        [(= n 2) 5]
        [else (+ (f (sub1 n)) (* 2 (f (sub1 (sub1 (sub1 n))))))]))

;; Fs-aux : Natural -> List(Natural)
;; Dado un número n devuelve la lista con todos los valores entre (f n) y (f 0).

(check-expect (Fs-aux 0) (list 0))
(check-expect (Fs-aux 5) (list 19 9 5 5 2 0))

(define (Fs-aux n)
  (cond [(zero? n) (cons (f 0) empty)]
        [else (cons (f n) (Fs-aux (sub1 n)))]))

;; Fs : Natural -> List(Natural)
;; Dado un número n devuelve la lista con todos los valores entre (f 0) y (f n).

(check-expect (Fs 0) (list 0))
(check-expect (Fs 5) (list 0 2 5 5 9 19))

(define (Fs n)
  (reverse (Fs-aux n)))