;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TP3-Mettini-Ruiz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 3: Estructuras

Integrantes:
- Mettini, Fabrizio, comisión 3.
- Ruiz, Fabricio, comisión 4.
|#

(define MENSAJE-ERROR-TIPO "Tipo de dato inválido")
(define MENSAJE-ERROR-NO-MAYOR-A-CERO "El entero n no es mayor a 0")

;; -------------------------------------
;; Ejercicio 1 

(define-struct Circunferencia [centro radio])
;; Circunferencia es (posn, Number)
;; Intepretación: El primer elemento es el centro de la circunferencia
;; (un par ordenado) mientras que el segundo es el radio de la misma
;; (en píxeles).

;; Ejemplos:
(define C1 (make-Circunferencia (make-posn 10 50) 10))
(define C2 (make-Circunferencia (make-posn 50 50) 30))
(define C3 (make-Circunferencia (make-posn 50 50) 20))

;; -------------------------------------
;; Ejercicio 2

;; distancia: posn posn -> Number
;; Dadas las coordenadas de los centros de dos circunferencias,
;; devuelve la distancia entre los mismos.

(check-expect (distancia (make-posn 3 5) (make-posn 4 5)) 1)

(define (distancia p q)
  (sqrt (+ (sqr (- (posn-x p) (posn-x q)))
           (sqr (- (posn-y p) (posn-y q))))))

;; tangentes-exteriores?: Circunferencia Circunferencia -> Bool/String
;; Dadas dos circunferencias, devuelve si son tangentes exteriores.
;; En caso que no se reciban datos de tipo Circunferencia, se informa el error.

(check-expect (tangentes-exteriores? C1 C2) #t)
(check-expect (tangentes-exteriores? C1 C3) #f)
(check-expect (tangentes-exteriores? C1 13) MENSAJE-ERROR-TIPO)

(define (tangentes-exteriores? c1 c2)
  (if (and (Circunferencia? c1 ) (Circunferencia? c2))
      (= (distancia (Circunferencia-centro c1) (Circunferencia-centro c2))
         (+ (Circunferencia-radio c1) (Circunferencia-radio c2)))
      MENSAJE-ERROR-TIPO))

;; -------------------------------------
;; Ejercicio 3

;; crear-tangente-exterior: Circunferencia Number -> Circunferencia/String
;; Dada una circunferencia C y un entero n mayor a 0, devuelva otra
;; circunferencia C’ que verifique las siguientes condiciones:
;; - C y C’ son tangentes exteriores
;; - El radio de C’ es n veces el radio de C
;; - Las coordenadas y de los centros de C y C’ son iguales
;; - La coordenada x del centro de C’ es mayor a la coordenada x del centro de C
;; En caso que no se reciban una Circunferencia y un entero mayor a 0,
;; se informa el error.

(check-expect (crear-tangente-exterior C1 3) C2)
(check-expect (crear-tangente-exterior C1 "") MENSAJE-ERROR-TIPO)
(check-expect (crear-tangente-exterior "C" 3) MENSAJE-ERROR-TIPO)
(check-expect (crear-tangente-exterior C1 -2) MENSAJE-ERROR-NO-MAYOR-A-CERO)

(define (crear-tangente-exterior C n)
  (cond [(not (Circunferencia? C)) MENSAJE-ERROR-TIPO]
        [(not (integer? n)) MENSAJE-ERROR-TIPO]
        [(<= n 0) MENSAJE-ERROR-NO-MAYOR-A-CERO]
        [else (crear-tangente-exterior-aux C n)]))

;; crear-tangente-exterior-aux: Circunferencia Number -> Circunferencia
;; Dada una circunferencia C y un entero n mayor a 0, devuelva otra
;; circunferencia C’ que verifique las siguientes condiciones:
;; - C y C’ son tangentes exteriores
;; - El radio de C’ es n veces el radio de C
;; - Las coordenadas y de los centros de C y C’ son iguales
;; - La coordenada x del centro de C’ es mayor a la coordenada x del centro de C

(check-expect (crear-tangente-exterior-aux C1 3) C2)

(define (crear-tangente-exterior-aux C n)
  (make-Circunferencia (make-posn (+ (posn-x (Circunferencia-centro C))
                                     (* (+ n 1) (Circunferencia-radio C)))
                                  (posn-y (Circunferencia-centro C)))
                       (* n (Circunferencia-radio C))))