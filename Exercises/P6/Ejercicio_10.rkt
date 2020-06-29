;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 10

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; g : Natural -> Natural
;; - g(0) = 1
;; - g(1) = 2
;; - g(2) = 3
;; - g(n) = g(n-1) * g(n-2) * g(n-3) para todo n mayor o igual a 3

(check-expect (g 0) 1)
(check-expect (g 1) 2)
(check-expect (g 2) 3)
(check-expect (g 3) 6)

(define (g n)
  (cond [(zero? n) 1]
        [(= n 1) 2] 
        [(= n 2) 3]
        [else (* (g (sub1 n)) (g (sub1 (sub1 n))) (g (sub1 (sub1 (sub1 n)))))]))

;; list-g : Natural -> List(Natural)
;; Dado un número n devuelve la lista con los valores que resulta de evaluar a
;; g en n, n-1, n-2,...,0.
;; Es decir (list-g n) = (list (g n) (g (- 1 n)) ... (g 1) (g 0)).

(check-expect (list-g 0) (list 1))
(check-expect (list-g 4) (list 36 6 3 2 1))

(define (list-g n)
  (cond [(zero? n) (list 1)]
        [else (cons (g n) (list-g (sub1 n)))]))