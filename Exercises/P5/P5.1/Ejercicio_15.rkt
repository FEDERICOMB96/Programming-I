;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_15) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 15

(define MAX 5)

;; Una Lista-de-puntos es:
;; - empty
;; - (cons Posn Lista-de-puntos)
;; Interpretación: una lista cuyos elementos son puntos del plano.

;; Ejemplos:
(define LISTA1 (list (make-posn 3 5) (make-posn 1 2) (make-posn 0 1) (make-posn 5 6)))
(define LISTA2 (list (make-posn 0 0) (make-posn 5 5) (make-posn 6 8) (make-posn 5 6)))

;; distancia-al-origen : Posn -> Number
;; Dado un punto del plano, devuelve su distancia al origen.

(check-expect (distancia-al-origen (make-posn 0 0)) 0)
(check-expect (distancia-al-origen (make-posn 3 4)) 5)

(define (distancia-al-origen p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; cerca : Lista-de-puntos -> Lista-de-puntos
;; Toma una lista de puntos del plano y devuelve la lista de aquellos puntos que están
;; a distancia menor a MAX de origen, donde MAX es una constante de su programa.

(check-expect (cerca empty) empty)
(check-expect (cerca LISTA1) (list (make-posn 1 2) (make-posn 0 1)))
(check-expect (cerca LISTA2) (list (make-posn 0 0)))

(define (cerca l)
  (cond [(empty? l) empty]
        [else (if (< (distancia-al-origen (first l)) MAX)
                  (cons (first l) (cerca (rest l)))
                  (cerca (rest l)))]))