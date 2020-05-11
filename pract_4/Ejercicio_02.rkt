;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; --------------------------------------
; Ejercicio 2

; dist-origen: (Number, Number) -> Number
; Calcula la distancia al origen del punto dado

(check-expect (dist-origen (make-posn 3 4)) 5)

(define (dist-origen p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

(dist-origen (make-posn (/ 6 2) 4))
(+ (dist-origen (make-posn 12 5)) 4)