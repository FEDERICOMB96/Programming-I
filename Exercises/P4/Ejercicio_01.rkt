;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_01) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 1 
(define p (make-posn 3 4))
(define q (make-posn -2 0.5))

#|
(posn-x p)
== definicion de p
(posn-x (make-posn 3 4))
== (ley 1)
3
|#

#|
(- (posn-y p) (posn-y q))
== definicion de p
(- (posn-y (make-posn 3 4)) (posn-y q))
== (ley 2)
(- 4 (posn-y q))
== definicion de q
(- 4 (posn-y (make-posn -2 0.5)))
== (ley 2)
(- 4 0.5)
== definicion de -
3.5
|#

#|
(posn-y (make-posn (posn-x p) (posn-x q)))
== definicion de p
(posn-y (make-posn (posn-x (make-posn 3 4)) (posn-x q)))
== (ley 1)
(posn-y (make-posn 3 (posn-x q)))
== definicion de q
(posn-y (make-posn 3 (posn-x (make-posn -2 0.5))))
== (ley 1)
(posn-y (make-posn 3 -2))
== (ley 2)
-2
|#