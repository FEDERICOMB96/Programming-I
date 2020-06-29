;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 16

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números natural

(define (coeficiente-interes n i)
  (+ 1 (* (/ i (* 100 12)) n)))

(define (lista-de-coeficientes n i)
  (cond [(zero? n) '()]
        [else (cons (coeficiente-interes n i) (lista-de-coeficientes (sub1 n) i))]))

;; cuotas-aux : Number Natural Natural -> List(Number)
;; Dado un importe total de un préstamo, un valor n correspondiente al número de cuotas,
;; una tasa i de interés, devuelva una lista con las cuotas a pagar ordenadas de forma decreciente.

(check-expect (cuotas-aux 10000 0 18) '())
(check-expect (cuotas-aux 10000 1 12) (list 10100))
(check-expect (cuotas-aux 30000 3 12) (list 10300 10200 10100))
(check-expect (cuotas-aux 100000 4 18) (list 26500 26125 25750 25375))

;; cuotas : Number Natural Natural -> List(Number)
;; Dado un importe total de un préstamo, un valor n correspondiente al número de cuotas,
;; una tasa i de interés, devuelva una lista con las cuotas a pagar ordenadas de forma creciente.

(check-expect (cuotas 10000 0 18) '())
(check-expect (cuotas 10000 1 12) (list 10100))
(check-expect (cuotas 30000 3 12) (list 10100 10200 10300))
(check-expect (cuotas 100000 4 18) (list 25375 25750 26125 26500))

(define (cuotas total n i)
  (reverse (cuotas-aux total n i)))