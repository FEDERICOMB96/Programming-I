;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 10

;; gorda? : Image -> Bool
;; Dada una imágen, devuelve si es "gorda", es decir si su ancho
;; es mayor a su alto.

(check-expect (gorda? (rectangle 40 20 "solid" "blue")) #t)
(check-expect (gorda? (circle 30 "solid" "blue")) #f)

(define (gorda? img)
  (> (image-width img) ( image-height img)))

;; area : Image -> Number
;; Dada una imagen, calcula su área.

(check-expect (area (rectangle 20 30 "solid" "red")) 600)

(define (area img)
  (* (image-width img) (image-height img)))

;; suma : List(Number) -> Number
;; Suma todos los elementos de una lista de números.

(check-expect (suma (list 1 2 3 4 5)) 15)
(check-expect (suma empty) 0)
(check-expect (suma (list 11 13 9)) 33)

(define (suma l) (foldr + 0 l))

;; sag : List(Image) -> Number
;; Dada una lista de imágenes, devuelva la suma de las áreas de aquellas
;; imágenes "Gordas".

(check-expect (sag empty) 0)
(check-expect (sag (list (circle 20 "solid" "red")
                         (rectangle 40 20 "solid" "blue")
                         (rectangle 10 20 "solid" "yellow")
                         (rectangle 30 20 "solid" "green"))) 1400)

(define (sag l)
  (suma (map area (filter gorda? l))))