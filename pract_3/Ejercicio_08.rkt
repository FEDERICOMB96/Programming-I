;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 8
; El estado del sistema es una imagen, que representa un cielo estrellado

; dimensiones de la escena:
(define ANCHO 300)
(define ALTO 300)

; escena vacia:
(define COLOR-CIELO "black")
(define FONDO (rectangle ANCHO ALTO "solid" COLOR-CIELO))

; estrella:
(define COLOR-ESTRELLA "yellow")
(define LADO-ESTRELLA 30)
(define estrella (star LADO-ESTRELLA "solid" COLOR-ESTRELLA))

(define (interpretar estado)
  (place-image estado (/ ANCHO 2) (/ ALTO 2) FONDO))

; manejarTeclado: Estado String -> Estado
; dado el estado actual y un código de tecla, calcula el estado siguiente:
; - tecla " ": se devuelve un cielo despejado
; cualquier otra tecla: no se modifica el estado

(check-expect (manejadorTeclado FONDO " ") FONDO)
(check-expect (manejadorTeclado FONDO "r") FONDO)

(define (manejadorTeclado estado k)
  (cond [(key=? k " ") FONDO]
        [else estado]))








; manejarMouse: Estado Number Number String -> Estado
; dados el estado actual y un evento del mouse, devuelve
; el nuevo estado de acuerdo al tipo de evento:
; -"button-down": posición horizontal del mouse
; cualquier otro evento: no modifica el estado

;(check-expect (manejarMouse POSX-INICIAL 100 50 "button-down") 100)
;(check-expect (manejarMouse POSX-INICIAL 100 50 "move") POSX-INICIAL)




(define (manejadorMouse estado x y evento)
  (cond [(string=? evento "button-down")
         (dibujarEstrella estado x y)]
        [else estado]))

(define (dibujarEstrella estado x y)
  (if (or (< x LADO-ESTRELLA)
          (> x (- ANCHO LADO-ESTRELLA)))
      estado
      (place-image estrella x y estado)))

; expresión big-bang
(big-bang FONDO
  [to-draw interpretar]
  [on-key manejadorTeclado]
  [on-mouse manejadorMouse])
