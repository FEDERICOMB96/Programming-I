;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 2

; dimensiones de la escena:
(define ALTO 300)
(define ANCHO 300)

; estado inicial:
(define RADIO-INICIAL 100)

; escena vacia:
(define FONDO (empty-scene ANCHO ALTO))

; color del circulo
(define COLOR-CIRCULO "blue")

; circulo: String -> Image
; dado un string que indica el color del circulo, devuelve una imagen de dicha
; figura del color especificado

(check-expect (circulo 50) (circle 50 "solid" COLOR-CIRCULO))

(define (circulo estado)
  (circle estado "solid" COLOR-CIRCULO))

; interpretar : Number -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar estado)
  (place-image (circulo estado) (/ ANCHO 2) (/ ALTO 2) FONDO))

; decrementar : Number -> Number
; Devuelve el predecesor de un número positivo.
; Si el número es 0, devuelve 100.

(check-expect (decrementar 0) RADIO-INICIAL)
(check-expect (decrementar 50) 49)

(define (decrementar n)
  (if (= n 0) RADIO-INICIAL (- n 1)))

; incrementar : Number -> Number
; suma uno a su argumento

(check-expect (incrementar 50) 51)

(define (incrementar n) (+ n 1))

; Preguntas:
; 1."¿Qué pasa si hago que decrementar reste uno siempre, incluso si su
;   argumento es 0?"

; 2."¿Qué pasa si hago que incrementar sume uno solo si el circulo cabe
;   completamente en la pantalla?"

; 3."¿Qué pasa si hago que el RADIO-INICIAL sea un numero complejo?"

; Respuestas:
; 1. Ocurre un error ya que el radio del circulo tiene que ser un numero no negativo.

; 2. Utilizando la siguiente funcion se puede lograr el comportamiento esperado:

#|
     (define (incrementar n)
       (if (< n (/ ANCHO 2))
           (+ n 1)
           0))
|#  

;    De esta manera el se puede observar correctamente el circulo en todo momento.   

; 3. Ocurre un error a la prmera vez que se llama a la funcion interpretar.

; expresión big-bang

(big-bang RADIO-INICIAL
  [to-draw interpretar]
  [on-tick decrementar])