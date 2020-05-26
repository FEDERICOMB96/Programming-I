;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 4

; El estado del sistema es un numero que representa la posicion vertical del
; circulo en pantalla

; dimensiones de la escena:
(define ALTO 300)
(define ANCHO 300)

; radio del circulo a representar:
(define RADIO 20)

; estado inicial:
(define POSY-INICIAL (/ ALTO 2))

; cantidad de pixeles que se desplaza el circulo:
(define DELTA 5)

; objeto a dibujar:
(define OBJETO (circle RADIO "solid" "blue"))

; escena vacia:
(define FONDO (empty-scene ANCHO ALTO))

; interpretar : Estado -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar posY)
  (place-image OBJETO (/ ANCHO 2) posY FONDO))

; manejarTeclado: Estado String -> Estado
; dado el estado actual y un código de tecla, calcula el estado siguiente:
; - tecla "up": decrementa el estado DELTA unidades
; - tecla "down": incrementa el estado DELTA unidades
; - tecla " ": posicion inicial

(check-expect (manejarTeclado POSY-INICIAL "up") (- POSY-INICIAL DELTA))
(check-expect (manejarTeclado POSY-INICIAL "down") (+ POSY-INICIAL DELTA))
(check-expect (manejarTeclado POSY-INICIAL " ") POSY-INICIAL)
(check-expect (manejarTeclado POSY-INICIAL "r") POSY-INICIAL)

(define (manejarTeclado posY k)
  (cond [(key=? k "up") (acomodar (- posY DELTA))]
        [(key=? k "down") (acomodar (+ posY DELTA))]
        [(key=? k " ") POSY-INICIAL]
        [else posY]))

; acomodar : Estado -> Estado
; dado un estado, asegura que este se encontrará en el
; intervalo [RADIO, ALTO - RADIO]
; para posiciones menores a RADIO (el objeto sobrepasa el límite superior),
;  devuelve RADIO
; para posiciones mayores a ALTO-RADIO (el objeto sobrepasa el límite inferior),
;  devuelve ALTO-RADIO

(check-expect (acomodar 0) RADIO)
(check-expect (acomodar POSY-INICIAL) POSY-INICIAL)
(check-expect (acomodar ALTO) (- ALTO RADIO))

(define (acomodar posY)
  (cond [(< posY RADIO) RADIO]
        [(> posY (- ALTO RADIO))(- ALTO RADIO)]
        [else posY]))

; manejarMouse: Estado Number Number String -> Estado
; dados el estado actual y un evento del mouse, devuelve
; el nuevo estado de acuerdo al tipo de evento:
; -"button-down": posición vertical del mouse
; cualquier otro evento: no modifica el estado

(check-expect (manejarMouse POSY-INICIAL 100 RADIO "button-down") RADIO)
(check-expect (manejarMouse POSY-INICIAL 100 ALTO "button-down") (- ALTO RADIO))
(check-expect (manejarMouse POSY-INICIAL 100 100 "move") POSY-INICIAL)

(define (manejarMouse posY x y event)
  (cond [(string=? event "button-down") (acomodar y)]
        [else posY]))

; expresión big-bang

(big-bang POSY-INICIAL
  [to-draw interpretar]
  [on-key manejarTeclado]
  [on-mouse manejarMouse])

