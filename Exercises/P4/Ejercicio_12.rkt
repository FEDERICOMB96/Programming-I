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
(define SILVIA (make-persona "Silvia Perez" 68 "kg" 1.64 "m"))
(define FER (make-persona "Fernando Aguirre" 72 "kg" 179 "cm"))
(define NICO (make-persona "Nicolás Zanarini" 24000 "g" 1.23 "m"))
(define ANA (make-persona "Ana Gomez" 65000 "g" 170 "cm"))

(define MENSAJE-ERROR-TIPO "Tipo de dato inválido") ; Si la función IMC no recibe una
                                                    ; entrada de tipo Persona

;; -------------------------------------
;; imc : persona -> Number/String
;; Dada una persona, devuelve su índice de masa corporal,
;; para cualquier otro objeto, devuelve "Tipo de dato inválido".

(define (imc p)
  (if (Persona? p)
      (/ (* (Persona-peso p) (if (string=? (Persona-peso-unidad p) "g") 0.001 1))
         (sqr (* (Persona-estatura p) (if (string=? (Persona-peso-unidad p) "c") 0.01 1))))
      MENSAJE-ERROR-TIPO))