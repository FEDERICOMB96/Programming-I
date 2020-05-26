;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 10

; Dimensiones de la escena:
(define ANCHO 800)
(define ALTO 300)

; Escena:
(define ESCENA (empty-scene ANCHO ALTO))

; Distancia (vertical) entre los pies:
(define DISTANCIA-PIES 50)

; Posiciones verticales pies:
(define POSY-PIEIZQ (/ ALTO 2))
(define POSY-PIEDER (+ (/ ALTO 2) DISTANCIA-PIES))

; Pies
(define PIEIZQ (rotate 270 (bitmap "img/pieizq100x100.png")))
(define PIEDER (rotate 270 (bitmap "img/pieder100x100.png")))

; Distancia desplazamiento por paso:
(define DELTA-PASO 55)

; Segundos entre ticks:
(define SEGUNDOS-ENTRE-TICKS 1)

; Estado Inicial:
(define ESTADO-INICIAL 10)

; dibujar: Estado -> Imagen

(define (dibujar estado)
    (if (zero? (modulo estado 2))
      (place-image PIEIZQ estado POSY-PIEIZQ ESCENA)
      (place-image PIEDER estado POSY-PIEDER ESCENA)))

; caminar: Estado -> Estado

(define (caminar estado)
  (+ estado DELTA-PASO))

; manejador-teclado: Estado KeyEvent -> Estado
; al presionar la barra espaciadora, la huella vuelve a la posición inicial.

(define (manejador-teclado estado k)
  (cond [(key=? k " ") ESTADO-INICIAL]
        [else estado]))

; manejador-mouse: Estado Number Number MouseEvent -> Estado
; al hacer click sobre la imagen, se dibuja una huella en la coordenada horizontal
; donde se produjo el evento (dependiendo de la paridad se debera dibujar uno u otro pie)

(define (manejador-mouse estado x y event)
  (cond [(mouse=? event "button-down") x]
        [else estado]))

; Expresión big-bang:

(big-bang ESTADO-INICIAL
  [to-draw dibujar]
  [on-tick caminar SEGUNDOS-ENTRE-TICKS]
  [on-key manejador-teclado]
  [on-mouse manejador-mouse]
  )