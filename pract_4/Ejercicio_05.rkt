;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; -------------------------------------
;; Ejercicio 5

;; El estado del sistema es una estructura de dos campos que representa la posicion
;; del circulo en pantalla

(define ANCHO 300)                      ; Ancho de la escena
(define ALTO 300)                       ; Alto de la escena
(define FONDO (empty-scene ANCHO ALTO)) ; Escena vacia

(define RADIO 20)                             ; Radio del circulo a representar
(define OBJETO (circle RADIO "solid" "blue")) ; Objeto a dibujar

(define LIMITE-IZQUIERDO RADIO)         ; Posición válida mínima sobre el eje horizontal
(define LIMITE-DERECHA (- ANCHO RADIO)) ; Posición válida máxima sobre el eje horizontal
(define LIMITE-SUPERIOR RADIO)          ; Posición válida mínima sobre el eje vertical
(define LIMITE-INFERIOR (- ALTO RADIO)) ; Posición válida máxima sobre el eje vertical

(define DELTA 5) ; Cantidad de pixeles que se desplaza el circulo
(define POS-INICIAL (make-posn (/ ANCHO 2) (/ ALTO 2))) ; Estado inicial

;; -------------------------------------
;; interpretar : Estado -> Image
;; transforma el estado del sistema en una imagen a mostrar a través
;; de la cláusula to-draw

(define (interpretar p)
  (place-image OBJETO (posn-x p) (posn-y p) FONDO))

;; -------------------------------------
;; manejar-teclado: Estado String -> Estado
;; dado el estado actual y un código de tecla, calcula el estado siguiente:
;; - tecla "up": decrementa la coordenada y DELTA unidades
;; - tecla "down": incrementa la coordenada y DELTA unidades
;; - tecla "left": decrementa la coordenada x DELTA unidades
;; - tecla "right": incrementa la coordenada x DELTA unidades
;; - tecla " ": posicion inicial
;; para cualquier otra tecla, el estado no cambia.

(check-expect (manejar-teclado POS-INICIAL "up")
              (make-posn (posn-x POS-INICIAL) (- (posn-y POS-INICIAL) DELTA)))
(check-expect (manejar-teclado POS-INICIAL "down")
              (make-posn (posn-x POS-INICIAL) (+ (posn-y POS-INICIAL) DELTA)))
(check-expect (manejar-teclado POS-INICIAL "left")
              (make-posn (- (posn-x POS-INICIAL) DELTA) (posn-y POS-INICIAL)))
(check-expect (manejar-teclado POS-INICIAL "right")
              (make-posn (+ (posn-x POS-INICIAL) DELTA) (posn-y POS-INICIAL)))
(check-expect (manejar-teclado POS-INICIAL " ") POS-INICIAL)
(check-expect (manejar-teclado POS-INICIAL "r") POS-INICIAL)

(define (manejar-teclado p k)
  (cond [(key=? k "up") (acomodar (make-posn (posn-x p) (- (posn-y p) DELTA)))]
        [(key=? k "down") (acomodar (make-posn (posn-x p) (+ (posn-y p) DELTA)))]
        [(key=? k "left") (acomodar (make-posn (- (posn-x p) DELTA) (posn-y p)))]
        [(key=? k "right") (acomodar (make-posn (+ (posn-x p) DELTA) (posn-y p)))]
        [(key=? k " ") POS-INICIAL]
        [else p]))

;; -------------------------------------
;; acomodar : Estado -> Estado
;; dado un estado, asegura que este se encontrará en el
;; intervalo [RADIO, ALTO - RADIO] y en el intervalo [RADIO, ANCHO - RADIO]
;; para posiciones menores a RADIO (el objeto sobrepasa el límite superior o el
;; objeto sobrepasa el límite lateral izquierdo),
;;  devuelve RADIO
;; para posiciones mayores a ANCHO-RADIO (el objeto sobrepasa el límite lateral
;; derecho) devuelve ANCHO-RADIO
;; para posiciones mayores a ALTO-RADIO (el objeto sobrepasa el límite inferior),
;;  devuelve ALTO-RADIO

(check-expect (acomodar (make-posn 0 RADIO)) (make-posn RADIO RADIO))
(check-expect (acomodar (make-posn RADIO 0)) (make-posn RADIO RADIO))
(check-expect (acomodar (make-posn ANCHO RADIO)) (make-posn (- ANCHO RADIO) RADIO))
(check-expect (acomodar (make-posn RADIO ALTO)) (make-posn RADIO (- ALTO RADIO)))

(define (acomodar p)
  (cond [(< (posn-x p) LIMITE-IZQUIERDO) (make-posn LIMITE-IZQUIERDO (posn-y p))]
        [(< (posn-y p) LIMITE-SUPERIOR) (make-posn (posn-x p) LIMITE-SUPERIOR)]
        [(> (posn-x p) LIMITE-DERECHA) (make-posn LIMITE-DERECHA (posn-y p))]
        [(> (posn-y p) LIMITE-INFERIOR) (make-posn (posn-x p) LIMITE-INFERIOR)]
        [else p]))

;; -------------------------------------
;; manejar-mouse: Estado Number Number String -> Estado
;; dados el estado actual y un evento del mouse, devuelve
;; el nuevo estado de acuerdo al tipo de evento:
;; -"button-down": posición del mouse
;; cualquier otro evento: no modifica el estado

(check-expect (manejar-mouse POS-INICIAL (/ ANCHO 2) (/ ALTO 2) "button-down")
              (acomodar (make-posn (/ ANCHO 2) (/ ALTO 2))))
(check-expect (manejar-mouse POS-INICIAL (/ ANCHO 2) (/ ALTO 2) "move")
              POS-INICIAL)

(define (manejar-mouse p x y event)
  (cond [(string=? event "button-down") (acomodar (make-posn x y))]
        [else p]))

;; -------------------------------------
;; Expresión big-bang:

(big-bang POS-INICIAL
  [to-draw interpretar]
  [on-key manejar-teclado]
  [on-mouse manejar-mouse]
  )
