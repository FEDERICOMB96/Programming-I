;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 5
; El estado del sistema es un string, que representa el color
; del circulo en pantalla

; dimensiones de la escena:
(define ANCHO 300)
(define ALTO 300)

; fondo:
(define COLOR-FONDO "white")
(define FONDO (rectangle ANCHO ALTO "solid" COLOR-FONDO))

; radio inicial del circulo:
(define RADIO-INICIAL 100)

; estado inicial:
(define ESTADO-INICIAL "yellow")

; circulo a dibujar
(define (circulo colorCirculo) (circle RADIO-INICIAL "solid" colorCirculo))

; interpretar : String -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar colorCirculo)
  (place-image (circulo colorCirculo) (/ ANCHO 2) (/ ALTO 2) FONDO))

; cambiar-color: String -> String
; recibe el color actual del circulo y devuelve el color al cual
; el debe cambiar

(check-expect (cambiar-color "yellow") "red")
(check-expect (cambiar-color "red") "green")
(check-expect (cambiar-color "green") "blue")
(check-expect (cambiar-color "blue") "yellow")

(define (cambiar-color colorCirculo)
  (cond [(string=? colorCirculo "yellow") "red"]
        [(string=? colorCirculo "red") "green"]
        [(string=? colorCirculo "green") "blue"]
        [(string=? colorCirculo "blue") "yellow"]))

; expresión big-bang

(big-bang ESTADO-INICIAL
  [to-draw interpretar]
  [on-tick cambiar-color 1])