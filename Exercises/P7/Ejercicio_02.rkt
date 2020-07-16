;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicio_02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 2

;; Estas primera parte nos define una función que genera números aleatorios
;; en un intervalo real determinado.
;; Recordemos que la función random nos devuelve un natural, pero para
;; nuestro problema necesitamos números aleatorios fraccionarios.

(define RAND-MAX 4294967087) ; número máximo para valores aleatorios

; aleatorio : Number Number -> Number
; Dados dos números a y b, devuelve un número aleatorio en el intervalo [a,b].

(define (aleatorio a b) (+ a (* (- b a) (/  (+ 1 (random RAND-MAX)) (* 1.0 RAND-MAX)))))

;; fin de la parte para generar números aleatorios. No es necesario entender cómo funciona,
;; sólo basta con entender el propósito de aleatorio.

;; Definimos algunas constantes:
(define RADIO 3.0) ; radio del círculo
(define CENTRO (make-posn -1 -0.5)) ; coordenadas del centro del círculo

(define MAX 300000) ; cantidad de puntos a generar para nuestra estimación

; generar-puntos : Natural -> List(posn)
; Dado un natural n, devuelve una lista con n puntos aleatorios,
; con ambas componentes, dentro del cuadrado.

(define (generar-puntos n)
  (cond [(zero? n) empty]
        [else (cons (make-posn (aleatorio (posn-x CENTRO) (+ (posn-x CENTRO) RADIO))
                               (aleatorio (posn-y CENTRO) (+ (posn-y CENTRO) RADIO)))
                    (generar-puntos (- n 1)))]))

; distancia : posn posn -> Number
; Dados dos puntos, devuelve su distancia.

(check-expect (distancia (make-posn 15 0) (make-posn 3 0)) 12)
(check-expect (distancia (make-posn 2 1) (make-posn 6 4)) 5)
(check-expect (distancia (make-posn 0 16) (make-posn 0 0)) 16)

(define (distancia p q)
  (sqrt (+ (sqr (abs (- (posn-x p) (posn-x q))))
           (sqr (abs (- (posn-y p) (posn-y q)))))))

; adentro? : posn -> Boolean
; Dada una posición, determina si cae en el area sombreada.

(check-expect (adentro? (make-posn 0 0)) #t)
(check-expect (adentro? (make-posn 1 1)) #t)
(check-expect (adentro? (make-posn -0.5 2)) #f)

(define (adentro? p)
  (and (<= (distancia p CENTRO) RADIO)
       (and (>= (posn-x p) 0)
            (>= (posn-y p) 0))))

; generamos una lista con muchos puntos aleatorios
(define LISTA (generar-puntos MAX))

; nos quedamos con aquellos que están dentro del círculo 
(define ADENTRO (filter adentro? LISTA))

; aproximamos el área sombreada a partir de la proporción de puntos que
; caen dentro de la misma:
(define AREA (* 9.0 (/ (length ADENTRO) (length LISTA)))) ; 9.0 = Area Cuadrado

(string-append "El area sombreada es: " (number->string (exact->inexact AREA)))
