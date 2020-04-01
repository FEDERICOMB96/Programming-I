;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicios_1-9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 1.
; Funcion que toma como argumento una imagen, y la clasifica en "Flaca" o "Gorda".
(define (flaca-gorda image)
  (if (>= (image-height image) (image-width image))
      "Flaca"
      "Gorda"))

; 2.
; Funcion que toma como argumento una imagen, y la clasifica en "Flaca", "Gorda" o "Cuadrada".
(define (flaca-gorda-cuadrada image)
  (if (> (image-height image) (image-width image))
      "Flaca"
      (if (< (image-height image) (image-width image))
          "Gorda"
          "Cuadrada")))

; 3.
; Funcion que dado tres numeros que representan la amplitud de los tres angulos interiores de un trianguno,
; devuelve un string que nos indica si es equilatero, isosceles o escaleno.
(define (clasifica-triangulo a1 a2 a3)
  (if (= a1 a2 a3)
      "Equilatero"
      (if (or (= a1 a2) (= a2 a3) (= a1 a3))
          "Isosceles"
          "Escaleno")))

; 4.
; Funcion que dado tres numeros verifica si representan la amplitud de tres angulos interiores de un triangulo,
; y en cuyo caso devuelve un string que nos indica si es equilatero, isosceles o escaleno, sino devuelve "Error".
(define (clasifica-triangulo-con-error a1 a2 a3)
  (if (= (+ a1 a2 a3) 180)
      (clasifica-triangulo a1 a2 a3)
      "Error"))

; 5.
; Funcion que dados dos valores c y l devuelve el monto a pagar si se compran c cuadernos y l lapices.

(define PC 60)
(define PL 8)

(define LIMITE-CUADERNOS 4)
(define DESCUENTO-CUADERNOS 0.1)

(define LIMITE-LAPICES 5)
(define DESCUENTO-LAPICES 0.15)

(define (monto-a-pagar c l)
  (+ (monto-cuadernos c) (monto-lapices l)))

(define (monto-cuadernos c)
  (* c (* PC (if (>= c LIMITE-CUADERNOS)
                 (- 1 DESCUENTO-CUADERNOS)
                 1))))

(define (monto-lapices l)
  (* l (* PL (if (>= l LIMITE-LAPICES)
                 (- 1 DESCUENTO-LAPICES)
                 1))))

; 6.
; Funcion que dados dos valores c y l devuelve el monto a pagar si se compran c cuadernos y l lapices.
(define LIMITE-TOTAL 10)
(define DESCUENTO-TOTAL 0.18)

(define (monto-a-pagar-con-descuento-total c l)
  (* (monto-a-pagar c l) (if (>= (+ c l) LIMITE-TOTAL)
                             (- 1 DESCUENTO-TOTAL)
                             1)))

; 7.
; Redefinicion de la funcion pitagorica? de la Practica 0 usando if.

(define (pitagorica? a b c)
  (if (and (> a b) (> a c))
      (= (sqr a) (+ (sqr b) (sqr c)))
      (if (and (> b a) (> b c))
          (= (sqr b) (+ (sqr a) (sqr c)))
          (= (sqr c) (+ (sqr a) (sqr b))))))
  
; 8.
; Modificacion del ejercicio anterior para que se genere un string en lugar de un booleano.
; En esta version, esperamos que (pitagorica? 3 4 5) evalue a "Los numeros 3, 4 y 5 forman una terna pitagorica."
  
(define (pitagorica?-version2 x y z)
  (if (pitagorica? x y z)
      (string-append "Los numeros " (number->string x) ", " (number->string y) " y "
                     (number->string z) " forman una terna pitagorica.")
      (string-append "Los numeros " (number->string x) ", " (number->string y) " y "
                     (number->string z) " no forman una terna pitagorica.")))

; 9.
; Funcion que reciba un numero natural n y devuelve n/2 si n es par, o 3*n+1 si n es impar.

(define (collatz n)
  (if (= (remainder n 2) 0)
      (/ n 2)
      (+ (* 3 n) 1)))