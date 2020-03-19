;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicios_1-9) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; 1.
; Funcion que toma como argumento una imagen, y la clasifica en "Flaca" o "Gorda".

(define
  (flaca-gorda image)
  (if (>= (image-height image) (image-width image)) "Flaca" "Gorda"))

; 2.
; Funcion que toma como argumento una imagen, y la clasifica en "Flaca", "Gorda" o "Cuadrada".

(define
  (flaca-gorda-cuadrada image)
  (if (> (image-height image) (image-width image))
      "Flaca"
      (if (< (image-height image) (image-width image)) "Gorda" "Cuadrada")))

; 3.
; Funcion que dado tres numeros que representan la amplitud de los tres angulos interiores de un trianguno,
; devuelve un string que nos indica si es equilatero, isosceles o escaleno.

(define
  (equilatero-isoceles-escaleno x y z)
  (if (= x y z)
      "Equilatero"
      (if (or (= x y) (= y z) (= x z)) "Isoceles" "Escaleno")))

; 4.
; Funcion que dado tres numeros verifica si representan la amplitud de tres angulos interiores de un triangulo,
; y en cuyo caso devuelve un string que nos indica si es equilatero, isosceles o escaleno, sino devuelve "Error".

(define
  (equilatero-isoceles-escaleno-version2 x y z)
  (if (= (+ x y z) 180)
      (if (= x y z)
          "Equilatero"
          (if (or (= x y) (= y z) (= x z)) "Isoceles" "Escaleno"))
      "Error"))

; 5.
; Funcion que dados dos valores c y l devuelve el monto a pagar si se compran c cuadernos y l lapices.

(define PC 60)
(define PL 8)

(define
  (monto-a-pagar c l)
  (+ (if (< c 4) (* c PC) (* c (* PC 0.9)))
     (if (< l 4) (* l PL) (* l (* PL 0.9)))))

; 6.
; Funcion que dados dos valores c y l devuelve el monto a pagar si se compran c cuadernos y l lapices.

(define
  (monto-a-pagar-version2 c l)
  (* (if (>= (+ c l) 10) 0.80 1)
     (+ (if (< c 4) (* c PC) (* c (* PC 0.9)))
        (if (< l 4) (* l PL) (* l (* PL 0.9))))))

; 7.
; Redefinicion de la funcion pitagorica? de la Practica 0 usando if.

(define (f x y z)
        (= (expt x 2) (+ (expt y 2) (expt z 2))))
  
(define
  (pitagorica? x y z)
  (if (or (f x y z) (f y x z) (f z x y)) #t #f))
  
; 8.
; Modificacion del ejercicio anterior para que se genere un string en lugar de un booleano.
; En esta version, esperamos que (pitagorica? 3 4 5) evalue a "Los numeros 3, 4 y 5 forman una terna pitagorica."
  
(define
  (pitagorica?-version2 x y z)
  (if (or (f x y z) (f y x z) (f z x y))
      (string-append "Los numeros " (number->string x) ", " (number->string y) " y "
                     (number->string z) " forman una terna pitagorica.")
      (string-append "Los numeros " (number->string x) ", " (number->string y) " y "
                     (number->string z) " no forman una terna pitagorica.")))

; 9.
; Funcion que reciba un numero natural n y devuelve n/2 si n es par, o 3*n+1 si n es impar.

(define
  (collatz n)
  (if (= (/ n 2) 0) (/ n 2) (+ (* 3 n) 1)))