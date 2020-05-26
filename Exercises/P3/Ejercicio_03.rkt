;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 3
; El estado del sistema es numero, que representa el radio del circulo.

; dimensiones de la escena:
(define ALTO 300)
(define ANCHO 300)

; estado inicial:
(define RADIO-INICIAL 100)

; escena vacia:
(define FONDO (empty-scene ANCHO ALTO))

; color-circulo: Number -> String
; dado un numero que indica el radio del circulo, devuelve el color
; correspodiente segun dicha magnitud. 

(check-expect (color-circulo 5) "yellow")
(check-expect (color-circulo 55) "red")
(check-expect (color-circulo 105) "green")

(define (color-circulo radio)
  (cond [(and (>= radio 0) (<= radio 50)) "yellow"]
        [(and (> radio 50) (<= radio 100)) "red"]
        [else "green"]))

; circulo: Number -> Image
; dado un string que indica el radio del circulo, devuelve una imagen de dicha
; figura del radio especificado y del color correspondiente

(check-expect (circulo 5) (circle 5 "solid" "yellow"))
(check-expect (circulo 55) (circle 55 "solid" "red"))
(check-expect (circulo 105) (circle 105 "solid" "green"))

(define (circulo radio) (circle radio "solid" (color-circulo radio)))

; interpretar : Number -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar estado)
  (place-image (circulo estado) (/ ANCHO 2) (/ ALTO 2) FONDO))

; incrementar : Number -> Number
; suma uno a su argumento sólo si el círculo resultante cabe completamente
; en la pantalla, sino devuelve 0

(check-expect (incrementar 1) 2)
(check-expect (incrementar (+ 1 ANCHO)) 0)

(define (incrementar n)
  (if (<= n (/ ANCHO 2))
      (+ n 1)
      0))

; manejarTeclado: Estado String -> Estado
; recibe el estado actual y la tecla presionada, y devuelve el nuevo estado

(check-expect (manejarTeclado 3 "p") 3)
(check-expect (manejarTeclado 3 "1") 10)

(define (manejarTeclado s k)
  (cond [(integer? (string->number k)) (* 10 (string->number k))]
        [else s]))

; terminar: Number -> Bool
; dado un numero que representa el radio del circulo, devuelve si no esta en
; el intervalo [10, 110]

(check-expect (terminar 50) #f)
(check-expect (terminar 5) #t)
(check-expect (terminar 111) #t)

(define (terminar n)
  (or (> n 110) (< n 10)))

; expresión big-bang

(big-bang RADIO-INICIAL
  [to-draw interpretar]
  [on-tick incrementar 0.1]
  [on-key manejarTeclado]
  [stop-when terminar])