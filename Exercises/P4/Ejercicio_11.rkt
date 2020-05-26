;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 11

(define-struct Casa [propietario direccion superficie zona])
;; Casa es (String, String, Number, String)
;; Intepretación: El primer elemento es el nombre del propietario, el segundo es la
;; dirección de la casa y el tercero es la superficie de la misma (en metros cuadrados)
;; y el cuarto la zona (A, B, C o D).

(define METRO-CUADRADO-ZONA-A 20000) ; Valor del metro cuadrado en zona A
(define METRO-CUADRADO-ZONA-B 15000) ; Valor del metro cuadrado en zona B
(define METRO-CUADRADO-ZONA-C 10000) ; Valor del metro cuadrado en zona C
(define METRO-CUADRADO-ZONA-D 5000)  ; Valor del metro cuadrado en zona D

(define COSTO-SELLADO-MINIMO 0.03) ; Porcentaje mínimo del sellado (un valor entre 0 y 1)
(define COSTO-SELLADO-MAXIMO 0.05) ; Porcentaje máximo del sellado (un valor entre 0 y 1)

(define VALOR-PROPIEDAD-SELLADO-MINIMO 1000000) ; Valor máximo hasta el cual el sellado
                                                ; es el mínimo

(define MENSAJE-ERROR-TIPO "Tipo de dato incorrecto") ; Si la función venta no recibe una
                                                      ; entrada de tipo Casa

; Si la función venta recibe como entrada un dato de tipo Casa con una zona
; distinta a A, B, C o D:
(define MENSAJE-ERROR-ZONA                            
  (string-append "No se puede calcular el precio de venta por no disponer de los valores "
                 "del metro cuadrado para la zona solicitada")) 

;; -------------------------------------
;; venta: Casa -> String
;; Devuelve un mensaje sobre los datos de la venta de dicha propiedad.

(check-expect (venta (make-posn 3 2)) MENSAJE-ERROR-TIPO)
(check-expect (venta (make-Casa "Pablo Escobar" "La Catedral" 250 "E")) MENSAJE-ERROR-ZONA)
(check-expect (venta (make-Casa "José Romero" "Rueda 3456" 120 "C"))
              (string-append "El señor José Romero recibirá 1140000 pesos por la venta de "
                             "su propiedad ubicada en la calle Rueda 3456."))

(define (venta casa)
  (cond [(not (Casa? casa)) MENSAJE-ERROR-TIPO]
        [(not (zona-valida? (Casa-zona casa))) MENSAJE-ERROR-ZONA]
        [else (string-append "El señor " (Casa-propietario casa) " recibirá "
                             (number->string (monto-propietario casa))
                             " pesos por la venta de su propiedad ubicada en la calle "
                             (Casa-direccion casa) ".")]))

;; -------------------------------------
;; zona-valida?: String -> Bool
;; Dada una zona, devuelve si es una zona válida (A, B, C o D) o no.

(check-expect (zona-valida? "A") #t)
(check-expect (zona-valida? "B") #t)
(check-expect (zona-valida? "C") #t)
(check-expect (zona-valida? "D") #t)
(check-expect (zona-valida? "E") #f)

(define (zona-valida? zona)
  (or (string=? zona "A")
      (string=? zona "B")
      (string=? zona "C")
      (string=? zona "D")))

;; -------------------------------------
;; metro-cuadrado-zona: String -> Number
;; Devuelve el valor del metro cuadrado segun la zona (A, B, C o D).

(check-expect (metro-cuadrado-zona "A") METRO-CUADRADO-ZONA-A)
(check-expect (metro-cuadrado-zona "B") METRO-CUADRADO-ZONA-B)
(check-expect (metro-cuadrado-zona "C") METRO-CUADRADO-ZONA-C)
(check-expect (metro-cuadrado-zona "D") METRO-CUADRADO-ZONA-D)

(define (metro-cuadrado-zona zona)
  (cond [(string=? zona "A") METRO-CUADRADO-ZONA-A]
        [(string=? zona "B") METRO-CUADRADO-ZONA-B]
        [(string=? zona "C") METRO-CUADRADO-ZONA-C]
        [else METRO-CUADRADO-ZONA-D]))

;; -------------------------------------
;; monto-propietario: Casa -> Number
;; Devuelve el monto que recibe el propietario por la venta, luego de pagar el sellado.

(check-expect (monto-propietario (make-Casa "Stanley Marsh" "Calle Falsa 123" 120 "C"))
              (* 120 METRO-CUADRADO-ZONA-C (- 1 COSTO-SELLADO-MAXIMO)))

(check-expect (monto-propietario (make-Casa "Eric Cartman" "Calle Falsa 456" 80 "C"))
              (* 80 METRO-CUADRADO-ZONA-C (- 1 COSTO-SELLADO-MINIMO)))

(define (monto-propietario casa)
  (* (Casa-superficie casa) (metro-cuadrado-zona (Casa-zona casa))
     (if (< (* (Casa-superficie casa) (metro-cuadrado-zona (Casa-zona casa)))
            VALOR-PROPIEDAD-SELLADO-MINIMO)
         (- 1 COSTO-SELLADO-MINIMO)
         (- 1 COSTO-SELLADO-MAXIMO))))
