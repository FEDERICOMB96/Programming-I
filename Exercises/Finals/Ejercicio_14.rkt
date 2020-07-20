;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_14) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 14

;; El estado del sistema es la estructura definida a continuacion:
(define-struct estado (figura posicion color))
;; estado es (make-struct (String posn String))
;; Interpretacion: el primer campo es un string, que representa la figura a representar ("square"
;; o "circle"), el segundo campo es un posn, que representa la posicion donde se representara la
;; figura, y el tercer campo es un string, que representa el color de la figura a representar
;; ("red", "blue" o "green").

;; Constantes
(define ANCHO 500)
(define ALTO 500)
(define FONDO (empty-scene ANCHO ALTO))

(define RADIO-CIRCULO 50)
(define LADO-CUADRADO 100)

(define INICIAL (make-estado "square" (make-posn (/ ANCHO 2) (/ ALTO 2)) "blue"))

;; interpretar : State -> Image
;; Dado un estado del sistema e, devuelve una imagen de la figura especificada
;; por (estado-figura e), en la posicion (estado-posicion e) y de color (estado-color e).

(define (interpretar e)
  (place-image (dibujar-figura (estado-figura e) (estado-color e))
               (posn-x (estado-posicion e)) (posn-y (estado-posicion e))
               FONDO))

;; dibujar-figura : String String -> Image
;; Dado dos strings f y c, que se refieren a la figura a representar y al color de la misma,
;; devuelve una imagen de la figura f de color c.

(define (dibujar-figura f c)
  (cond [(string=? f "square") (square LADO-CUADRADO "solid" c)]
        [else (circle RADIO-CIRCULO "solid" c)]))

;; mouse : State Number Number MouseEvent -> State
;; Dados el estado del sistema, un evento de mouse y dos numeros x e y que representan donde
;; ocurrio dicho evento, devuelve el nuevo estado del sistema, de acuerdo a lo siguiente:
;; Si se hizo click izquierdo: la posicion del estado del sistema será (make-posn x y)
;; Cualquier otra: no se modifica el estado del sistema

(check-expect (mouse INICIAL 0 0 "button-down")
              (make-estado (estado-figura INICIAL) (make-posn 0 0) (estado-color INICIAL)))
(check-expect (mouse INICIAL 0 0 "move") INICIAL)

(define (mouse e x y evento)
  (cond [(mouse=? evento "button-down")
         (make-estado (estado-figura e) (make-posn x y) (estado-color e))]
        [else e]))

;; teclado : State KeyEvent -> State
;; Dados el estado del sistema y un evento de teclado, devuelve el nuevo estado del sistema,
;; de acuerdo a lo siguiente:
;; Si se presiona la tecla:
;; - "r" : el color del nuevo estado será rojo
;; - "b" : el color del nuevo estado será azul
;; - "g" : el color del nuevo estado será verde
;; - "s" : la figura del nuevo estado será un cuadrado
;; - "c" : la figura del nuevo estado será un circulo
;; Cualquier otra: no se modifica el estado del sistema

(check-expect (teclado INICIAL "r")
              (make-estado (estado-figura INICIAL) (estado-posicion INICIAL) "red"))
(check-expect (teclado INICIAL "b")
              (make-estado (estado-figura INICIAL) (estado-posicion INICIAL) "blue"))
(check-expect (teclado INICIAL "g")
              (make-estado (estado-figura INICIAL) (estado-posicion INICIAL) "green"))
(check-expect (teclado INICIAL "s")
              (make-estado "square" (estado-posicion INICIAL) (estado-color INICIAL)))
(check-expect (teclado INICIAL "c")
              (make-estado "circle" (estado-posicion INICIAL) (estado-color INICIAL)))
(check-expect (teclado INICIAL "j") INICIAL)

(define (teclado e k)
  (cond [(key=? k "r") (make-estado (estado-figura e) (estado-posicion e) "red")]
        [(key=? k "b") (make-estado (estado-figura e) (estado-posicion e) "blue")]
        [(key=? k "g") (make-estado (estado-figura e) (estado-posicion e) "green")]      
        [(key=? k "s") (make-estado "square" (estado-posicion e) (estado-color e))]
        [(key=? k "c") (make-estado "circle" (estado-posicion e) (estado-color e))]
        [else e]))

;; Expresion big-bang
(big-bang INICIAL
  [to-draw interpretar]
  [on-mouse mouse]
  [on-key teclado])