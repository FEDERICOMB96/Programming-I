;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_19) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 19

;; Una Lista-de-puntos es:
;; - empty
;; - (cons Posn Lista-de-puntos)
;; Interpretación: una lista cuyos elementos son puntos del plano.

;; Ejemplos:
(define LISTA1 (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5)))
(define LISTA2 (list (make-posn 0 9) (make-posn 4 3)))

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA-A (list 5 4 13))
(define LISTA-B (list 9 5))

;; distancia-al-origen : posn -> Number
;; Dado un punto del plano, devuelve su distancia al origen.

(check-expect (distancia-al-origen (make-posn 0 0)) 0)
(check-expect (distancia-al-origen (make-posn 3 4)) 5)

(define (distancia-al-origen p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; distancias : Lista-de-puntos -> Lista-de-numeros
;; Dada una lista de puntos del plano, devuelve una lista con la
;; distancia al origen de cada uno.

(check-expect (distancias empty) empty)
(check-expect (distancias LISTA1) LISTA-A)
(check-expect (distancias LISTA2) LISTA-B)

(define (distancias l)
  (cond [(empty? l) empty]
        [else (cons (distancia-al-origen (first l)) (distancias (rest l)))]))