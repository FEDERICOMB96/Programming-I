;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 7
; El estado del sistema es un número que representa la distancia en píxeles
; desde el borde izquierdo de la pantalla hasta el centro del auto
; (coordenada horizontal del auto)

; dimensiones de la escena:
(define ANCHO 800)
(define ALTO 60)

; objeto a representar: 
(define AUTO (bitmap "img/auto.png"))

; estado inicial
(define POSX-INICIAL (/ (image-width AUTO) 2))

; desplazamiento del auto, en píxeles, cada tick del reloj
(define DESPLAZAMIENTO-TICK 3)

; desplazamiento del auto, en píxeles, cada vez que se presiona la tecla "right"
; o la tecla "left":
(define DELTA 20)

; escena vacia:
(define FONDO (empty-scene ANCHO ALTO))

; interpretar : Estado -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar posX)
  (place-image AUTO posX (/ ALTO 2) FONDO))

; acomodar: Estado -> Estado
; devuleve un estado que se encuentra en el intervalo que garantiza
; que la imagen se dibujará completa en la escena

(define (acomodar posX)
  (cond [(< posX (/ (image-width AUTO) 2))
         (/ (image-width AUTO) 2)]
        [(> posX (- ANCHO (/ (image-width AUTO) 2)))
         (- ANCHO (/ (image-width AUTO) 2))]
        [else posX]))

; desplazar: Estado -> Estado
; recibe la posicion actual del auto y devuelve la posicion del auto luego de un
; tick del reloj

(check-expect (desplazar POSX-INICIAL) (+ POSX-INICIAL DESPLAZAMIENTO-TICK))

(define (desplazar posX)
  (acomodar (+ posX DESPLAZAMIENTO-TICK)))

; manejarTeclado: Estado String -> Estado
; dado el estado actual y un código de tecla, calcula el estado siguiente:
; - tecla "right": incrementa el estado DELTA unidades
; - tecla "left": decrementa el estado DELTA unidades
; - tecla " ": posicion inicial

(check-expect (manejarTeclado POSX-INICIAL "right") (acomodar (+ POSX-INICIAL DELTA)))
(check-expect (manejarTeclado POSX-INICIAL "left") (acomodar (- POSX-INICIAL DELTA)))
(check-expect (manejarTeclado POSX-INICIAL " ") POSX-INICIAL)
(check-expect (manejarTeclado POSX-INICIAL "r") POSX-INICIAL)

(define (manejarTeclado posX k)
  (cond [(key=? k " ") POSX-INICIAL]
        [(key=? k "right") (acomodar (+ posX DELTA))]
        [(key=? k "left") (acomodar (- posX DELTA))]
        [else posX]))

; manejarMouse: Estado Number Number String -> Estado
; dados el estado actual y un evento del mouse, devuelve
; el nuevo estado de acuerdo al tipo de evento:
; -"button-down": posición horizontal del mouse
; cualquier otro evento: no modifica el estado

(check-expect (manejarMouse POSX-INICIAL 100 50 "button-down") 100)
(check-expect (manejarMouse POSX-INICIAL 100 50 "move") POSX-INICIAL)

(define (manejarMouse posX x y event)
  (cond [(string=? event "button-down") (acomodar x)]
        [else posX]))

; expresión big-bang
(big-bang POSX-INICIAL
  [to-draw interpretar]
  [on-tick desplazar]
  [on-key manejarTeclado]
  [on-mouse manejarMouse])

