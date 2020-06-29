;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 12

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; multiplos : Natural Natural -> List(Natural)
;; Toma dos naturales n y m, y devuelve una lista con los primeros n múltiplos
;; positivos de m, en orden inverso: m * n, m * (n-1), ... , m * 2, m.

(check-expect (multiplos 0 11) '())
(check-expect (multiplos 3 3) (list 9 6 3))
(check-expect (multiplos 4 7) (list 28 21 14 7))

(define (multiplos n m)
  (cond [(zero? n) '()]
        [else (cons (* n m) (multiplos (sub1 n) m))]))