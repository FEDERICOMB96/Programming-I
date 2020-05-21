;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 4

(define MSJ-ERROR "Tipos incorrectos para la función.")

;; distancia: (Number, Number) (Number, Number) -> Number
;; Dado dos puntos en el plano calcula la distancia entre ellos

(check-expect (distancia (make-posn 0 0) (make-posn 3 4)) 5)
(check-expect (distancia (make-posn -3 -4) (make-posn 3 4)) 10)
(check-expect (distancia 0 (make-posn 3 4)) MSJ-ERROR)

(define (distancia p1 p2)
  (if (and (posn? p1) (posn? p2))
      (sqrt (+ (sqr (- (posn-x p2) (posn-x p1)))
               (sqr (- (posn-y p2) (posn-y p1)))))
      MSJ-ERROR))