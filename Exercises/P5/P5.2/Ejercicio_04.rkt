;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_04) (read-case-sensitive #t) (teachpacks ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "universe.rkt" "teachpack" "2htdp") (lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 4

;; fold : (X X -> X) X List(X) -> X
;; Dadas una funcion f, un elemento c en X y una lista de objetos en X,
;; devuelve el resultado de aplicar f entre todos los elementos de la lista,
;; siendo c el valor devuelto para la lista vacia.

(define (fold f c l)
  (cond [(empty? l) c]
        [else (f (first l) (fold f c (rest l)))]))

(define CIRCULOS (list (circle 10 "solid" "blue")
                       (circle 20 "solid" "yellow")
                       (circle 30 "solid" "blue")
                       (circle 40 "solid" "yellow")
                       (circle 50 "solid" "blue")
                       (circle 60 "solid" "yellow")))
 
(define FONDO (empty-scene 200 200))

(fold overlay FONDO CIRCULOS)