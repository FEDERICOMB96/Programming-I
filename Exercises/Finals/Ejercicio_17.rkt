;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_17) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 17

;; suma-positivos-es-mayor? : List(X) -> Boolean
;; Toma una lista l con objetos de cualquier clase, y devuelve #t si el valor absoluto de la suma de
;; los números positivos es mayor al valor absoluto de la suma de los números negativos.

(check-expect (suma-positivos-es-mayor? (list 5 "abc" 2 #t -3 "def")) #t)
(check-expect (suma-positivos-es-mayor? (list 0 -1 "Joder" "Tio" -2)) #f)
(check-expect (suma-positivos-es-mayor? (list 1 3 -3 "ab" (square 20 "solid" "cyan"))) #t)
(check-expect (suma-positivos-es-mayor? (list (circle 10 "solid" "blue") 12 1 -2 -45)) #f)

(define (suma-positivos-es-mayor? l)
  (> (abs (foldr + 0 (filter positive? (filter number? l))))
     (abs (foldr + 0 (filter negative? (filter number? l))))))

#| Con solo un filter

(define (suma-positivos-es-mayor? l)
  (> (abs (foldr + 0 (filter (lambda (x) (and (number? x) (positive? x))) l)))
     (abs (foldr + 0 (filter (lambda (x) (and (number? x) (negative? x))) l)))))

|#