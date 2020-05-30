;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_28) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 28

;; Una Lista-de-puntos es:
;; - empty
;; - (cons Posn Lista-de-puntos)
;; Interpretación: una lista cuyos elementos son puntos del plano.

;; Ejemplos:
(define LISTA1 (list (make-posn 3 4) (make-posn 0 4) (make-posn 12 5)))
(define LISTA2 (list (make-posn 0 0) (make-posn 5 12)))

;; distancia-al-origen : posn -> Number
;; Dado un punto del plano, devuelve su distancia al origen.

(check-expect (distancia-al-origen (make-posn 0 0)) 0)
(check-expect (distancia-al-origen (make-posn 3 4)) 5)

(define (distancia-al-origen p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; sumdist : Lista-de-puntos -> Number
;; Dada una lista de puntos del plano,
;; devuelve la suma de sus distancias al origen.

(check-expect (sumdist empty) 0)
(check-expect (sumdist LISTA1) 22)
(check-expect (sumdist LISTA2) 13)

(define (sumdist l)
  (cond [(empty? l) 0]
        [else (+ (distancia-al-origen (first l)) (sumdist (rest l)))]))