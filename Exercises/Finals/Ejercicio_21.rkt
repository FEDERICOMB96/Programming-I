;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_21) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 21

;; long-lists : List(List(X)) -> Boolean
;; toma una lista de listas y devuelve #true si y sólo si las longitudes de todas las sublistas
;; son mayores a 4.

(check-expect (long-lists (list (list 1 2 3 4 5) (list 1 2 3 4 5 6) (list 87 73 78 83 33))) #t)
(check-expect (long-lists empty) #t)
(check-expect (long-lists (list '() '() (list 1 2 3))) #f)
(check-expect (long-lists (list (list 1 2 3 4 5) '())) #f)

(define (long-lists l)
  (andmap (lambda (x) (> (length x) 4)) l))

#| Otras posibles soluciones:

(define (long-lists l)
  (cond [(empty? l) #t]
        [(> (length (first l)) 4) (long-lists (rest l))]
        [else #f]))

(define (long-lists l)
  (= (length (filter (lambda (x) (> x 4)) (map length l))) (length l)))

|#