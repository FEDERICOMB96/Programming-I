;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_23) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 23

(define ANCHO 300)
(define ALTO 300)
(define FONDO (empty-scene ANCHO ALTO))

;; graficar-elipse : Natural -> Image
;; Toma un numero natural n y dibuja una elipse de ancho (10 * n), de alto (5 * n) y con un angulo
;; de rotacion de (* 2 n).

(define (graficar-elipse n)
  (rotate (* 2 n) (ellipse (* 10 n) (* 5 n) "outline" "blue")))

;; dibujar-elipses : Natural -> Image
;; Toma un número natural n y devuelva una imagen de 300 x 300 con n elipses de contorno azul
;; centradas en el centro de la imagen. Los tamaños de las elipses serán: (10 * n) * (5 * n) ,
;; (10 * (n - 1)) * (5 * (n - 1)), ..., 20 * 10, 10 * 5.
;; El ángulo de rotación de cada elipse coincide con el valor del doble de n.

(define (dibujar-elipses n)
  (cond [(zero? n) FONDO]
        [else (place-image (graficar-elipse n)
                           (/ ANCHO 2) (/ ALTO 2)
                           (dibujar-elipses (sub1 n)))]))

(dibujar-elipses 30)