;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; -------------------------------------
;; Ejercicio 8

(define-struct Texto [s color tam])
;; Texto es (String, Color, Number)
;; Intepretación: El primer elemento es la cadena a mostarse, mientras que el segundo y
;; el tercero determinan el color y tamaño de la fuente en píxeles respectivamente.

(define ANCHO 800)                      ; Ancho de la escena
(define ALTO 100)                       ; Alto de la escena
(define FONDO (empty-scene ANCHO ALTO)) ; Escena Vacia

(define DELTA 20) ; Incremento o decremento de la fuente
(define ESTADO-INICIAL (make-Texto "" "black" 20)) ; Estado inicial

;; -------------------------------------
;; interpretar : Estado -> Image
;; transforma el estado del sistema en una imagen a mostrar a través
;; de la cláusula to-draw.

(define (interpretar t)
  (place-image/align (text (Texto-s t) (Texto-tam t) (Texto-color t))
                     0 0 "left" "top"
                     FONDO))

;; -------------------------------------
;; comportamiento: Estado String -> Estado
;; Dado una tecla:
;; - Al presionar las teclas "f1", "f2", ..., "f6": se modifica el color
;; - Al presionar la tecla "up": se incrementa la fuente
;; - Al presionar la tecla "down": se decrementa la fuente
;; - Al presionar la tecla "backspace": se borra el ultimo caracter
;; - Al presionar cualquier otra tecla: se agrega al final de la cadena

(check-expect (comportamiento ESTADO-INICIAL "f1")
              (make-Texto (Texto-s ESTADO-INICIAL)
                          "black"
                          (Texto-tam ESTADO-INICIAL)))

(check-expect (comportamiento ESTADO-INICIAL "up")
              (make-Texto (Texto-s ESTADO-INICIAL)
                          (Texto-color ESTADO-INICIAL)
                          (+ (Texto-tam ESTADO-INICIAL) DELTA)))

(check-expect (comportamiento ESTADO-INICIAL "down")
              (make-Texto (Texto-s ESTADO-INICIAL)
                          (Texto-color ESTADO-INICIAL)
                          (max DELTA (- (Texto-tam ESTADO-INICIAL) DELTA))))

(check-expect (comportamiento ESTADO-INICIAL "\b")
              (make-Texto (Texto-s ESTADO-INICIAL)
                          (Texto-color ESTADO-INICIAL)
                          (Texto-tam ESTADO-INICIAL)))

(check-expect (comportamiento ESTADO-INICIAL "k")
              (make-Texto (string-append (Texto-s ESTADO-INICIAL) "k")
                          (Texto-color ESTADO-INICIAL)
                          (Texto-tam ESTADO-INICIAL)))

(define (comportamiento t k)
  (cond [(key=? k "f1") (make-Texto (Texto-s t) "black" (Texto-tam t))]  ; Si se presiona "f1"
        [(key=? k "f2") (make-Texto (Texto-s t) "blue" (Texto-tam t))]   ; Si se presiona "f2"
        [(key=? k "f3") (make-Texto (Texto-s t) "green" (Texto-tam t))]  ; Si se presiona "f3"
        [(key=? k "f4") (make-Texto (Texto-s t) "red" (Texto-tam t))]    ; Si se presiona "f4"
        [(key=? k "f5") (make-Texto (Texto-s t) "cyan" (Texto-tam t))]   ; Si se presiona "f5"
        [(key=? k "f6") (make-Texto (Texto-s t) "yellow" (Texto-tam t))] ; Si se presiona "f6"
        ; Si se presiona la tecla "up", la fuente se incrementa:
        [(key=? k "up") (make-Texto (Texto-s t) (Texto-color t) (+ (Texto-tam t) DELTA))]
        ; Si se presiona la tecla "down", la fuente se achica (si es posible):
        [(key=? k "down") (make-Texto (Texto-s t)
                                      (Texto-color t)
                                      (max DELTA (- (Texto-tam t) DELTA)))]
        ; Si se presiona la tecla "backspace", se elimina el ultimo caracter:
        [(key=? k "\b") (make-Texto (borrar-letra  (Texto-s t))
                                    (Texto-color t)
                                    (Texto-tam t))]
        ; Si se presiona cualquier otra tecla:
        [else (make-Texto (agregar-letra (Texto-s t) k) (Texto-color t) (Texto-tam t))]))

;; -------------------------------------
;; agregar-letra: String String -> String
;; Dado una tecla, concatena esta a la cadena que ya habíamos escrito hasta ahora.

(check-expect (agregar-letra "h" "i") "hi")

(define (agregar-letra s k)
  (string-append s k))

;; -------------------------------------
;; borrar-letra: String -> String
;; Dado una tecla, se retira de la cadena que ya habíamos escrito hasta ahora
;; el ultimo caracter agregado.

(check-expect (borrar-letra "C") "")
(check-expect (borrar-letra "") "")

(define (borrar-letra s)
  (if (> (string-length s) 0)
      (substring s 0 (- (string-length s) 1))
      s))

;; -------------------------------------
;; Expresión big-bang:

(big-bang ESTADO-INICIAL
  [to-draw interpretar]
  [on-key comportamiento]
  )