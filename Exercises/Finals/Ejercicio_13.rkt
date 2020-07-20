;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_13) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require lang/posn)
(require 2htdp/image)
(require 2htdp/universe)

;; -------------------------------------------------------------------------------------------------
;; Ejercicio 13

;; El Estado está compuesto por:
(define-struct estado (imagen radio color))
;; estado es (make-struct Image Number String)
;; Interpretacion: el primer campo es una imagen (que representará la escena completa en un momento
;; dado), el segundo campo es un número que indica el radio de círculo que se dibujará al hacer click
;; con el mouse y el tercero un string que indica de qué color se dibujarán los círculos.

;; definiciones constantes, COMPLETE si es necesario
(define ANCHO 500)
(define ALTO 500)
(define COLOR-FONDO "gray")
(define FONDO (empty-scene ANCHO ALTO COLOR-FONDO))

;; Estado inicial. Escena vacía del color del fondo, radio 20, color "red"
(define INICIAL (make-estado FONDO 20 "blue"))

;; pantalla: State -> Image
;; pantalla: dedicado a la cláusula to-draw de la función big-bang

(define (pantalla e) (estado-imagen e))


;; agregar-circulo : State -> Image
;; Dado un estado e y dos numeros x y,
;; devuelve un circulo de radio (estado-radio e) y color (estado-color e).

(define (agregar-circulo e x y)
  (place-image (circle (estado-radio e) "solid" (estado-color e)) x y (estado-imagen e)))

;; mouse-handler: State Number Number MouseEvent -> State
;; mouse-handler es el handler dedicado a la cláusula on-mouse de la función big-bang

(define (mouse-handler e x y m)
  (cond [(mouse=? m "button-down")
         (make-estado (agregar-circulo e x y) (estado-radio e) (estado-color e))]
        [else e]))

;; key-handler: State Key -> State
;; key-handler es el handler dedicado a la cláusula on-key de la función big-bang

(define (key-handler e k)
  (cond [(key=? k "b") (make-estado (estado-imagen e) (estado-radio e) "blue")]        
        [(key=? k "g") (make-estado (estado-imagen e) (estado-radio e) "green")]
        [(key=? k "m") (make-estado (estado-imagen e) (estado-radio e) "magenta")]
        [(key=? k "up") (make-estado (estado-imagen e) (* (estado-radio e) 2) (estado-color e))]
        [(key=? k "down") (make-estado (estado-imagen e) (/ (estado-radio e) 2) (estado-color e))]
        [(key=? k "r") INICIAL]
        [else e]))

; Expresión big-bang para el programa interactivo.
(big-bang INICIAL
  [to-draw pantalla]
  [on-key key-handler]
  [on-mouse mouse-handler])