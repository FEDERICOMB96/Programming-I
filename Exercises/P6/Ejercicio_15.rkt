;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_15) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 15

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números natural

(define FONDO (empty-scene 200 200))

;; graficar-cuadrado : Natural Number -> Image
;; La función graficar-cuadrados toma un valor de lado lad y un ángulo ang
;; y a partir de ahí grafica un cuadrado de lado lad y lo rota en un ángulo ang.

(define (graficar-cuadrado l a)
  (rotate a (square l "outline" "blue")))

;; cuadrados : Natural Number -> Image
;; Toma un número natural m, un ángulo ang y devuelve una imagen cuadrada de
;; lado 200 con m cuadrados azules centrados.
;; Los cuadrados azules tendrán lados de tamaño: m^2, (m-1)^2, ... , 2^2,1 respectivamente.
;; El ángulo ang indica la rotación del cuadrado de mayor dimensión.
;; El ángulo que corresponde al cuadrado de lado (m-1)^2 debe ser de 20 grados mayor que
;; el que le corresponde al cuadrado de lado m^2, para cualquier valor de m mayor o igual a 1.

(define (cuadrados m ang)
  (cond [(zero? m) FONDO]
        [else (place-image (graficar-cuadrado (sqr m) ang)
                           100 100
                           (cuadrados (sub1 m) (+ ang 20)))]))