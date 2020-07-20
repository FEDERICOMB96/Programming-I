;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Estado del sistema:
(define-struct estado [color angulo rotacion])
;; estado es (make-estado String Number String).
;; Interpretación: El primer campo es un color, que sera el color correspondiente al
;; cuadrado en el centro de la escena, el segundo campo sera un angulo que indica la
;; rotacion con la cual se dibuja el cuadrado en el centro de la escena y el tercer
;; campo sera un string que indica si el cuadrado rota automaticamente con cada tick
;; o no. El string "on" indica una rotacion automatica. El string "off" indica que
;; el cuadrado no se mueve.

;; Constantes
(define ALTO 300)
(define ANCHO 300)
(define GRADOS-POR-TICK 5)
(define COLOR-FONDO "green")
(define INIT (make-estado "red" 0 "on")) ; estado inicial
(define LADO-CUADRADO 50)
(define CENTRO (make-posn (/ ANCHO 2) (/ ALTO 2)))
(define ESCENA (empty-scene ANCHO ALTO COLOR-FONDO))

;; dibujar : estado -> imagen
(define (dibujar estado)
  (place-image (rotate (estado-angulo estado)
                       (square LADO-CUADRADO "solid" (estado-color estado)))
               (posn-x CENTRO) (posn-y CENTRO)
               ESCENA))

;; key-handler : estado key -> estado
;; Al presionar la barra espaciadora el movimiento tenia un valor "on" el valor
;; pasara a ser "off". Y viceversa.

(define (key-handler e k)
  (cond [(string=? k "y") (make-estado "yellow" (estado-angulo e) (estado-rotacion e))]
        [(string=? k "b") (make-estado "blue" (estado-angulo e) (estado-rotacion e))]
        [(string=? k "r") (make-estado "red" (estado-angulo e) (estado-rotacion e))]
        [(string=? k " ") (make-estado (estado-color e)
                                       (estado-angulo e)
                                       (cambiar-rotacion (estado-rotacion e)))]
        [else e]))


;; actualizar-angulo : estado -> estado
;; responde al tick del reloj

(define (actualizar-angulo e)
  (cond [(string=? (estado-rotacion e) "on")
         (make-estado (estado-color e)
                      (cambiar-angulo (estado-angulo e))
                      (estado-rotacion e))]
        [else e]))

;; Auxiliares

;; cambiar-rotacion : String -> String
(check-expect (cambiar-rotacion "on") "off")
(check-expect (cambiar-rotacion "off") "on")

(define (cambiar-rotacion s)
  (if (string=? s "on") "off" "on"))

;; cambiar-angulo : Number -> Number
(check-expect (cambiar-angulo 180) 185)
(check-expect (cambiar-angulo 355) 0)

(define (cambiar-angulo ang)
  (modulo (+ ang GRADOS-POR-TICK) 360))

;; Expresion big-bang
(big-bang INIT
  [on-tick actualizar-angulo]
  [on-key  key-handler]
  [to-draw dibujar])