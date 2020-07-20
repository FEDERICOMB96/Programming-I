;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_15) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require lang/posn)
(require 2htdp/image)
(require 2htdp/universe)

;; -------------------------------------------------------------------------------------------------
;; Ejercicio 15

;; definiciones constantes, COMPLETE
(define ANCHO 500) ; ancho de la escena
(define ALTO 500)  ; alto de la escena
(define COLOR-FONDO "yellow")
(define FONDO (empty-scene ANCHO ALTO COLOR-FONDO))

(define COLOR-CIRCULO "black")
(define RADIO-CIRCULO 30)
(define CIRCULO (circle RADIO-CIRCULO "solid" COLOR-CIRCULO))

;; Estado inicial: lista vacia
(define INIT empty)

;; pantalla: Estado -> Image
;; Pantalla: dedicado a la cláusula to-draw de la función big-bang
;; Toma un estado y crea la imagen que corresponde agregando al fondo los círculos en las posiciones
;; que indica el estado.

(define (pantalla e)
  (cond [(empty? e) FONDO]
        [else (place-image CIRCULO
                           (posn-x (first e)) (posn-y (first e))
                           (pantalla (rest e)))]))

;; agregar-circulo : List(Posn) -> List(Posn)
;; Dado una lista de posn l, se agrega un elemento de tipo posn al final de l.

(define (agregar-circulo l)
  (reverse (cons (make-posn (+ RADIO-CIRCULO (random (- ANCHO (* 2 RADIO-CIRCULO))))
                            (+ RADIO-CIRCULO (random (- ALTO (* 2 RADIO-CIRCULO)))))
                 (reverse l))))

;; eliminar-ultimo : List(X) -> List(X)
;; Dado una lista de elementos cualesquiera, elimina el ultimo elemento de la misma.

(check-expect (eliminar-ultimo empty) empty)
(check-expect (eliminar-ultimo (list 1 2 3)) (list 1 2))

(define (eliminar-ultimo l)
  (cond [(empty? l) empty]
        [else (reverse (rest (reverse l)))]))

;; key-handler: Estado Key -> Estado
;; key-handler es el handler dedicado a la cláusula on-key de la función big-bang
;; - Si se presiona la tecla "b" deberán borrarse todas los círculos y volver a estado inicial.
;; - Si se presiona la tecla "u" deberá borrarse la posición del último círculo agregado.
;; - Si se presiona la barra espaciadora se agregan un circulo en una posicion aleatoria.

(check-expect (key-handler (list (make-posn 50 50) (make-posn 100 100)) "b") INIT)
(check-expect (key-handler (list (make-posn 50 50) (make-posn 100 100)) "u")
              (list (make-posn 50 50)))
(check-expect (key-handler (list (make-posn 50 50)) "j") (list (make-posn 50 50)))  

(define (key-handler e k)
  (cond [(key=? k " ") (agregar-circulo e)]
        [(key=? k "u") (eliminar-ultimo e)]
        [(key=? k "b") INIT]
        [else e]))

;; Expresión big-bang para el programa interactivo.
(big-bang INIT
  [to-draw  pantalla]
  [on-key   key-handler])