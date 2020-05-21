;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; --------------------------------------
; Ejercicio 7

(define-struct auto [hpos vel])
; Auto es (Number, Number)
; Intepretación: El primer elemento es la posición horizontal del auto,
; mientras que el segundo representa la velocidad expresada en píxeles sobre tick.

; Dimensiones de la escena:
(define ANCHO 800)
(define ALTO 60)

; Objeto a representar: 
(define AUTO (bitmap "img/auto.png"))

; Desplazamiento del auto, en píxeles, cada vez que se presiona la tecla "right"
; o la tecla "left":
(define DELTA 20)

; Velocidad inicial:
(define VELOCIDAD-INICIAL 3)

; Delta velocidad:
(define DELTA-VEL 1)

; Estado inicial:
(define INICIAL (make-auto (/ (image-width AUTO) 2) VELOCIDAD-INICIAL))

; Segundos entre ticks:
(define SEGUNDOS-ENTRE-TICKS 0.1)

; Escena vacia:
(define FONDO (empty-scene ANCHO ALTO))

; --------------------------------------
; interpretar : Estado -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar a)
  (place-image AUTO (auto-hpos a) (/ ALTO 2) FONDO))

; --------------------------------------
; acomodar: Estado -> Estado
; devuleve un estado que se encuentra en el intervalo que garantiza
; que la imagen se dibujará completa en la escena

(define (acomodar posX)
  (cond [(< posX (/ (image-width AUTO) 2))
         (/ (image-width AUTO) 2)]
        [(> posX (- ANCHO (/ (image-width AUTO) 2)))
         (- ANCHO (/ (image-width AUTO) 2))]
        [else posX]))

; --------------------------------------
; desplazar: Estado -> Estado

(define (desplazar a)
  (make-auto (acomodar (+ (auto-hpos a) (auto-vel a))) (auto-vel a)))

; --------------------------------------
; manejar-teclado: Estado String -> Estado
; dado el estado actual y un código de tecla, calcula el estado siguiente:
; - tecla "right": incrementa el estado DELTA unidades
; - tecla "left": decrementa el estado DELTA unidades
; - tecla "up": incrementa la velocidad DELTA-VEL unidades
; - tecla "down": decrementa la velocidad DELTA-VEL unidades
; - tecla " ": posicion inicial

(define (manejar-teclado a k)
  (cond [(key=? k "right") (make-auto (acomodar (+ (auto-hpos a) DELTA)) (auto-vel a))]
        [(key=? k "left") (make-auto (acomodar (- (auto-hpos a) DELTA)) (auto-vel a))]
        [(key=? k "up") (make-auto (auto-hpos a) (+ (auto-vel a) DELTA-VEL))]
        [(key=? k "down") (make-auto (auto-hpos a) (- (auto-vel a) DELTA-VEL))]
        [(key=? k " ") INICIAL]
        [else a]))

; --------------------------------------
; manejar-mouse: Estado Number Number String -> Estado
; dados el estado actual y un evento del mouse, devuelve
; el nuevo estado de acuerdo al tipo de evento:
; -"button-down": posición horizontal del mouse
; cualquier otro evento: no modifica el estado

(define (manejar-mouse a x y event)
  (cond [(string=? event "button-down") (make-auto (acomodar x) (auto-vel a))]
        [else a]))

; --------------------------------------
; Expresión big-bang:

(big-bang INICIAL
  [to-draw interpretar]
  [on-tick desplazar SEGUNDOS-ENTRE-TICKS]
  [on-key manejar-teclado]
  [on-mouse manejar-mouse]
  )