;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; ##############################################################################
; Ejercicio 6
; El estado del sistema es un string, que representa el texto del editor

; dimensiones de la escena:
(define ANCHO 800)
(define ALTO 60)

; estado inicial:
(define ESTADO-INICIAL "")

; fondo:
(define FONDO (empty-scene 800 60))

; interpretar : String -> Image
; transforma el estado del sistema en una imagen a mostrar a través
; de la cláusula to-draw

(define (interpretar estado)
  (place-image/align (text estado 20 "indigo")
                     0 0 "left" "top"
                     FONDO))

; comportamiento: Estado String -> String
; Dado una tecla, se agrega o se borra una letra a la cadena que ya habíamos
; escrito hasta ahora

(check-expect (comportamiento "racke" "t") "racket")
(check-expect (comportamiento "C#" "\b") "C")
(check-expect (comportamiento "" "\b") "")

(define (comportamiento estado k)
  (cond [(key=? k "\b") (borrar-letra estado k)]
        [else (agregar-letra estado k)]))

; agregar-letra: Estado String -> String
; Dado una tecla, concatena esta a la cadena que ya habíamos escrito hasta ahora

(check-expect (agregar-letra "h" "i") "hi")

(define (agregar-letra s k)
  (string-append s k))

; borrar-letra: Estado String -> String
; Dado una tecla, se retira de la cadena que ya habíamos escrito hasta ahora
; el ultimo caracter agregado

(check-expect (borrar-letra "C" "\b") "")
(check-expect (borrar-letra "" "\b") "")

(define (borrar-letra s k)
  (if (> (string-length s) 0)
      (substring s 0 (- (string-length s) 1))
      s))

; expresión big-bang

(big-bang ESTADO-INICIAL
  [to-draw interpretar]
  [on-key comportamiento])