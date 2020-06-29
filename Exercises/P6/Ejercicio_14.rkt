;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_14) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 14

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números natural

(define FONDO (empty-scene 200 200))

;; graficar-circulo : Natural -> Image
;; Toma un numero natural n y dibuja un circulo de radio n.

(define (graficar-circulo r)
  (circle r "outline" "blue"))

;; circulos : Natural -> Image
;; Toma un número natural m y devuelva una imagen cuadrada de lado 2*m^2 con m
;; círculos azules centrados y radios: m^2, (m-1)^2, ... , 2^2, 1 respectivamente.

(define (circulos m)
  (cond [(zero? m) FONDO]
        [else (place-image (graficar-circulo (sqr m))
                           100 100
                           (circulos (sub1 m)))]))