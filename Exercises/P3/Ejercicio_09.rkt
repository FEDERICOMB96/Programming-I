;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
 (require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 9

; El estado del sistema es un string de longitud 2, de manera que el primer
; caracter indica la figura a representar ("t" si es triangulo o
; "c" si es circulo) y el segundo caracter indica el color de dicha figura
; ("a" azul o "v" verde)

; dimensiones de la escena:
(define ANCHO 500)
(define ALTO 500)
(define FONDO (empty-scene ANCHO ALTO))

; estado inicial:
(define ESTADO-INICIAL "tv")

; colores posibles:
(define AZUL "blue")
(define VERDE "green")

; segundos entre ticks:
(define SEGUNDOS-ENTRE-TICKS 1)

; radio del circulo:
(define RADIO-CIRCULO 25)

; circulo:
(define (circulo color)
  (circle RADIO-CIRCULO "solid" color))

; triangulo:
(define LADO-TRIANGULO 50)

(define (triangulo color)
  (triangle LADO-TRIANGULO "solid" color))

(define (primer-caracter s)
  (substring s 0 1))

(define (segundo-caracter s)
  (substring s 1 2))

; interpretar:
(define (interpretar estado)
  (place-image (if (string=? (primer-caracter estado) "t")
                   (triangulo (definir-color (segundo-caracter estado)))
                   (circulo (definir-color (segundo-caracter estado))))
               (random (+ ANCHO 1)) (random (+ ALTO 1))
               FONDO))

; definir-color: String -> String

(define (definir-color char)
  (if (string=? char "a")
      AZUL
      VERDE))

; cambiar-color: Estado -> Estado
(define (cambiar-color estado)
  (cond [(string=? (segundo-caracter estado) "a")
         (string-append (primer-caracter estado) "v")]
        [(string=? (segundo-caracter estado) "v")
         (string-append (primer-caracter estado) "a")]
        [else estado]))

; manejar-teclado: Estado KeyEvent -> String
(check-expect (manejar-teclado ESTADO-INICIAL "t") "tv")
(check-expect (manejar-teclado ESTADO-INICIAL "c") "cv")
(check-expect (manejar-teclado ESTADO-INICIAL "k") ESTADO-INICIAL)

(define (manejar-teclado estado k)
  (cond [(key=? k "t") (string-append "t" (segundo-caracter estado))]
        [(key=? k "c") (string-append "c" (segundo-caracter estado))]
        [else estado]))

; expresión big-bang:

(big-bang ESTADO-INICIAL
  [to-draw interpretar]
  [on-tick cambiar-color SEGUNDOS-ENTRE-TICKS]
  [on-key manejar-teclado]
  )

