;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicio_04) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 4

(define MAX 10000)
(define LISTA-DE-PRIMOS (map string->number (read-lines "primos.txt")))

;; menor-o-igual-n : Number List(Natural) -> List(Natural)
;; Dado un numero n y una lista de naturales ORDENADA, devuelve una lista
;; con todos los elementos menores o iguales que n.

(check-expect (menor-o-igual-n 5 (list 1 2 3)) (list 1 2 3))
(check-expect (menor-o-igual-n 5 empty) empty)
(check-expect (menor-o-igual-n 5 (list 6 7 8)) empty)

(define (menor-o-igual-n n l)
  (cond [(empty? l) empty]
        [else (if (<= (first l) n)
                  (cons (first l) (menor-o-igual-n n (rest l)))
                  empty)])) ; Porque l esta ordenada

;; es-primo?-aux : Natural -> Bool
;; Dado un natural m menor o igual que MAX^2 y una lista de primos menores o iguales que sqrt(m)
;; determina si este es primo o compuesto.

(check-expect (es-primo?-aux 16 (list 2 3)) #false)
(check-expect (es-primo?-aux 61 (list 2 3 5 7)) #true)

(define (es-primo?-aux m l)
  (cond [(empty? l) #true]
        [else (if (integer? (/ m (first l)))
                  #false
                  (es-primo?-aux m (rest l)))]))

;; es-primo? : Natural -> Bool
;; Dado un natural m menor o igual que MAX^2 determina si este es primo o compuesto.

(check-expect (es-primo? 16) #false)
(check-expect (es-primo? 61) #true)

(define (es-primo? m)
  (es-primo?-aux m (menor-o-igual-n (sqrt m) LISTA-DE-PRIMOS)))