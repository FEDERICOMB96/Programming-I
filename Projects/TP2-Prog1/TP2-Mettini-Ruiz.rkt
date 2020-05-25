;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TP2-Mettini-Ruiz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 2: Programas Interactivos

Integrantes:
- Mettini, Fabrizio comisión 3.
- Ruiz, Fabricio comisión 4.
|#

; El estado del sistema es un número entero en el rango [MINIMO, MAXIMO]
; (en el caso que nos compete MINIMO = 0 y MAXIMO = 100).

; Dimensiones de la escena: 
(define ANCHO 800)
(define ALTO 300)

; Color de fondo de la escena:
(define COLOR-FONDO "white")

; Escena:
(define ESCENA (empty-scene ANCHO ALTO COLOR-FONDO))

; Rango de los números aleatorios:
(define MINIMO 0)
(define MAXIMO 100)

; Estado inicial: 
(define ESTADO-INICIAL (round (/ (- MAXIMO MINIMO) 2)))

; Tamaño de la fuente:
(define TAMAÑO-FUENTE 50)

; Color si el número es par: 
(define COLOR-PAR "blue")

; Color si el número es impar: 
(define COLOR-IMPAR "red")

; Segundos entre los ticks:
(define SEGUNDOS-ENTRE-TICKS 2)

; Margen:
(define MARGEN
  (/ (image-width (text (number->string MAXIMO) TAMAÑO-FUENTE "black")) 2))

; Ancho efectivo:
(define ANCHO-EFECTIVO (- ANCHO (* 2 MARGEN)))

; --------------------------------------
; estado->imagen: Estado -> Image
; transforma el estado del sistema en una imagen con el tamaño y
; color correspondiente.

(define (estado->imagen estado)
  (text (number->string estado)
        TAMAÑO-FUENTE
        (if (even? estado) COLOR-PAR COLOR-IMPAR)))

; --------------------------------------
; posicion: Number -> Number
; Devuelve la posición horizontal del número a representar sin considerar
; los márgenes de la escena.

(check-expect (posicion MINIMO) 0)
(check-expect (posicion (round (/ MAXIMO 2))) (/ ANCHO-EFECTIVO 2))
(check-expect (posicion MAXIMO) ANCHO-EFECTIVO)

(define (posicion numero)
  (* (/ ANCHO-EFECTIVO (- MAXIMO MINIMO)) numero))

; --------------------------------------
; interpretar: Estado -> Image
; transforma el estado del sistema en una imagen, centrada verticalmente
; y de posicion horizontal tal que todo número se encuentra a la derecha
; de sus predecesores y a la izquierda de sus sucesores.

(define (interpretar estado)
  (place-image (estado->imagen estado)
               (+ MARGEN (posicion estado)) (/ ALTO 2)
               ESCENA))

; --------------------------------------
; generar: Estado -> Estado
; Devuelve un número aleatorio (entero) en el rango [MINIMO, MAXIMO].

(check-within (generar ESTADO-INICIAL) MINIMO MAXIMO)

(define (generar estado)
  (+ MINIMO (random (+ MAXIMO 1))))

; --------------------------------------
; manejador-teclado: Estado KeyEvent -> Estado
; Devuelve el nuevo estado del sistema de acuerdo al siguiente requerimiento:
; - Si se presiona la flecha izquierda: se reemplaza el estado actual
; por otro número elegido aleatoriamente entre MINIMO y MAXIMO/2 - 1.
; - Si se presiona la flecha derecha: se reemplaza el estado actual
; por otro número elegido aleatoriamente entre MAXIMO/2 + 1 y MAXIMO
; - Si se presiona cualquier otra tecla, el estado no se reemplaza.

(check-within (manejador-teclado ESTADO-INICIAL "left") MINIMO (/ MAXIMO 2))
(check-within (manejador-teclado ESTADO-INICIAL "right") (+ (/ MAXIMO 2) 1) MAXIMO)
(check-expect (manejador-teclado ESTADO-INICIAL " ") ESTADO-INICIAL)

(define (manejador-teclado estado k)
  (cond [(key=? k "left") (+ MINIMO (random (/ MAXIMO 2)))]
        [(key=? k "right") (+ (/ MAXIMO 2) 1 (random (/ MAXIMO 2)))]
        [else estado]))

; --------------------------------------
; multiplo-de-8?: Estado -> Boolean
; Devuelve si el estado dado es múltiplo de 8 o no.
; Si lo es el programa finalizará, caso contrario continúa ejecutandose
; la expresión big-bang.

(check-expect (multiplo-de-8? 17) #f)
(check-expect (multiplo-de-8? 64) #t)

(define (multiplo-de-8? estado)
  (zero? (modulo estado 8)))

; --------------------------------------
; Expresión big-bang:

(big-bang ESTADO-INICIAL
  [to-draw interpretar]
  [on-tick generar SEGUNDOS-ENTRE-TICKS]
  [on-key manejador-teclado]
  [stop-when multiplo-de-8? interpretar]
  )
