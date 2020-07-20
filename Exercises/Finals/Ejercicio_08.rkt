;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 8

(define-struct trabajador (apellido estado-civil cantidad-hijos sueldo-bruto))
;; trabajador es (make-struct String String Number Number)
;; Interpretacion: el primer campo es el apellido del trabajador, el segundo es el estado civil
;; ("casado", "soltero", etc), el tercero es la cantidad de hijos que tiene y el ultimo es su
;; sueldo bruto.

;; Ejemplos
(define TRABAJADOR1 (make-trabajador "Morales" "soltero" 1 14000))  
(define TRABAJADOR2 (make-trabajador "Gómez" "casado" 1 17000))     
(define TRABAJADOR3 (make-trabajador "Fortuna" "soltero" 1 15500))  
(define TRABAJADOR4 (make-trabajador "Romano" "casado" 2 19500))    
(define TRABAJADOR5 (make-trabajador "Espósito" "casado" 4 290800)) 
(define TRABAJADOR6 (make-trabajador "Sandoval" "soltero" 1 18500)) 

;; Constantes
(define MSJ-ERROR-TIPO "Tipo de dato inválido")
(define MINIMO-NO-IMPONIBLE-SOLTERO 15000)
(define MINIMO-NO-IMPONIBLE-CASADO 18000)
(define INCREMENTO-POR-HIJO-MINIMO 1000)
(define PORCENTAJE-IMPUESTO 0.05)

;; impuesto : trabajador -> Number/String
;; Toma como entrada un valor de tipo trabajador y calcula el impuesto a pagar.
;; En caso que no reciba como entrada una estructura de tipo trabajador
;; se mostrará un mensaje de error.

(check-expect (impuesto "Morales") MSJ-ERROR-TIPO)
(check-expect (impuesto TRABAJADOR1) 0)
(check-expect (impuesto TRABAJADOR2) 0)
(check-expect (impuesto TRABAJADOR3) 0)
(check-expect (impuesto TRABAJADOR4) 0)
(check-expect (impuesto TRABAJADOR5) 0)
(check-expect (impuesto TRABAJADOR6) 925)

(define (impuesto t)
  (cond [(not (trabajador? t)) MSJ-ERROR-TIPO]
        [(>= (trabajador-cantidad-hijos t) 3) 0]
        [(and (string=? (trabajador-estado-civil t) "soltero")
              (<= (trabajador-sueldo-bruto t)
                  (+ MINIMO-NO-IMPONIBLE-SOLTERO
                     (* (trabajador-cantidad-hijos t) INCREMENTO-POR-HIJO-MINIMO)))) 0]
        [(and (string=? (trabajador-estado-civil t) "casado")
              (<= (trabajador-sueldo-bruto t)
                  (+ MINIMO-NO-IMPONIBLE-CASADO
                     (* (trabajador-cantidad-hijos t) INCREMENTO-POR-HIJO-MINIMO)))) 0]
        [else (* (trabajador-sueldo-bruto t) PORCENTAJE-IMPUESTO)]))