;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 12

(define-struct Persona [nombre peso peso-unidad estatura estatura-unidad])
;; Persona es (String, Number, String, Number, String)
;; interpretación: representamos una persona mediante su nombre,
;; peso y estatura, agregando campos para la unidad de medida de estos
;; dos últimos valores.

;; Representamos estaturas y pesos mediante números
;; Representamos nombres mediante strings
;; Representamos unidades de estatura mediante strings ("m" "cm")
;; Representamos unidades de peso mediante strings ("kg" "g")

;; Ejemplos de personas
(define SILVIA (make-Persona "Silvia Perez" 68 "kg" 1.64 "m"))
(define FER (make-Persona "Fernando Aguirre" 72 "kg" 179 "cm"))
(define NICO (make-Persona "Nicolás Zanarini" 24000 "g" 1.23 "m"))
(define ANA (make-Persona "Ana Gomez" 65000 "g" 170 "cm"))

(define MENSAJE-ERROR-TIPO "Tipo de dato inválido") ; Si la función IMC no recibe una
                                                    ; entrada de tipo Persona

;; -------------------------------------
;; imc : persona -> Number/String
;; Dada una persona, devuelve su índice de masa corporal,
;; para cualquier otro objeto, devuelve "Tipo de dato inválido".

(check-within (imc SILVIA) 25.28 0.1)
(check-expect (imc "NICO") MENSAJE-ERROR-TIPO)

(define (imc p)
  (if (Persona? p)
      (imc-aux p)
      MENSAJE-ERROR-TIPO))

;; -------------------------------------
;; imc : persona -> Number/String
;; Dada una persona, devuelve su índice de masa corporal en kg/m^2.

(check-within (imc ANA) 22.49 0.1)
(check-within (imc FER) 22.47 0.1)

(define (imc-aux p)
  (/ (toKg (Persona-peso p) (Persona-peso-unidad p))
     (sqr (toM (Persona-estatura p) (Persona-estatura-unidad p)))))

;; -------------------------------------
;; toKg: Number String -> Number
;; Dado un peso, evalúa si está en kilogramos.
;; Si no lo está, lo convierte de gramos a kilogramos.

(check-expect (toKg 64 "kg") 64)
(check-expect (toKg 82000 "g") 82)

(define (toKg p u)
  (if (string=? u "kg") p (* p 0.001)))

;; -------------------------------------
;; toM: Number String -> Number
;; Dado un estatura, evalúa si está en metros.
;; Si no lo está, lo convierte de centímetros a metros.

(check-expect (toM 1.83 "m") 1.83)
(check-expect (toM 190 "cm") 1.90)

(define (toM m u)
  (if (string=? u "m") m (* m 0.01)))