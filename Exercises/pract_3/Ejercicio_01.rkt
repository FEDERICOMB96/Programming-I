;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 1
; El estado es un string, que representa el color del circulo a dibujar.
; Posibles valores:
; - "blue"
; - "red"
; - "green"
; - "black"

; dimensiones de la escena:
(define ALTO 300)
(define ANCHO 300)

; estado inicial:
(define ESTADO-INICIAL "blue")

; escena vacia:
(define FONDO (empty-scene ANCHO ALTO))

; radio del circulo:
(define RADIO 10)

; circulo: String -> Image
; dado un string que indica el color del circulo, devuelve una imagen de dicha
; figura del color especificado

(check-expect (circulo "blue") (circle RADIO "solid" "blue"))
(check-expect (circulo "red") (circle RADIO "solid" "red"))

(define (circulo estado) (circle RADIO "solid" estado))

; interpretar: Estado -> Image
; dado un estado, devuelve la imagen a mostrar por el programa

(define (interpretar estado)
   (place-image (circulo estado) (/ ANCHO 2) (/ ALTO 2) FONDO))

; manejarTeclado: Estado String -> Estado
; recibe el estado actual y la tecla presionada, y devuelve el nuevo estado

(check-expect (manejarTeclado "blue" " ") ESTADO-INICIAL)
(check-expect (manejarTeclado "blue" "a") "blue")
(check-expect (manejarTeclado "blue" "r") "red")
(check-expect (manejarTeclado "blue" "v") "green")
(check-expect (manejarTeclado "blue" "n") "black")
(check-expect (manejarTeclado "blue" "h") "blue")

(define (manejarTeclado estado k)
  (cond [(key=? k " ") ESTADO-INICIAL]
        [(key=? k "a") "blue"]
        [(key=? k "r") "red"]
        [(key=? k "v") "green"]
        [(key=? k "n") "black"]
        [else estado]))

; expresión big-bang

(big-bang ESTADO-INICIAL   ; estado inicial del sistema
  [to-draw interpretar]    ; dibuja en la pantalla el círculo en el estado actual
  [on-key manejarTeclado]) ; cuando se presiona una tecla,
                           ; la función manejarTeclado
                           ; se invoca para manejar el evento

