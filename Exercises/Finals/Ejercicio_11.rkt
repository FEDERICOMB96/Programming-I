;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 11

(define-struct pers (peso altura))
;; pers es (make-struct Number Number)
;; Interpretacion: el primer campo representa el peso de una persona (en kg) y el segundo campo
;; representa su altura (en metros).

;; Ejemplos:
(define LUZ (make-pers 44 1.55))
(define SOFIA (make-pers 50 1.56))
(define JAVIER (make-pers 90 1.80))
(define MIGUEL (make-pers 110 1.85))

;; imc : pers -> Number
;; Dada una persona, devuelve su imc.

(check-within (imc LUZ) 18.31 0.01)
(check-within (imc SOFIA) 20.54 0.01)
(check-within (imc JAVIER) 27.77 0.01)
(check-within (imc MIGUEL) 32.14 0.01)

(define (imc p)
  (/ (pers-peso p) (sqr (pers-altura p))))

;; prom-imc : List(pers) -> Number
;; Dada una lista l de estructuras pers devuelva el promedio del IMC de todas las personas de la
;; lista l.

(check-expect (prom-imc empty) 0)
(check-within (prom-imc (list LUZ SOFIA)) 19.43 0.01)

(define (prom-imc l)
  (cond [(empty? l) 0]
        [else (/ (foldr + 0 (map imc l)) (length l))]))

(define IMC-BAJO 18.5)
(define IMC-SOBREPESO 25)
(define IMC-OBESIDAD 30)

;; clasificar : pers -> String
;; Dada una persona, devuelve su clasificacion segun su imc.

(check-expect (clasificar LUZ) "Bajo Peso")
(check-expect (clasificar SOFIA) "Peso Normal")
(check-expect (clasificar JAVIER) "Sobrepeso")
(check-expect (clasificar MIGUEL) "Obesidad")

(define (clasificar p)
  (cond [(< (imc p) IMC-BAJO) "Bajo Peso"]
        [(< (imc p) IMC-SOBREPESO) "Peso Normal"]
        [(< (imc p) IMC-OBESIDAD) "Sobrepeso"]
        [else "Obesidad"]))

;; clasif : List(pers) -> List(String)
;; Dada una lista l de estructuras pers devuelva una lista con la clasificación de las personas
;; de la lista l.

(check-expect (clasif empty) empty)
(check-expect (clasif (list LUZ SOFIA))
              (list "Bajo Peso" "Peso Normal"))

(define (clasif l)
 (map clasificar l))

;; poblaciónSaludable? : List(pers) -> Boolean
;; Dada una lista l de estructuras pers determine si al menos el 80% de las personas de la lista l
;; tienen peso normal.

(check-expect (poblaciónSaludable? (list SOFIA)) #t)
(check-expect (poblaciónSaludable? (list LUZ SOFIA)) #f)

(define (poblaciónSaludable? l)
  (>= (/ (length (filter (lambda (s) (string=? s "Peso Normal")) (clasif l))) (length l)) 0.8))