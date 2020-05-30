;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_24) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 24

;; Una ListaTempF es:
;; - empty
;; - (cons Number ListaTempF)
;; Interpretación: una lista cuyos elementos son
;; temperaturas en grados Fahrenheit.

;; Ejemplos:
(define LISTA-F1 (list 32 140 212))
(define LISTA-F2 (list 500 -40))

;; Una ListaTempC es:
;; - empty
;; - (cons Number ListaTempC)
;; Interpretación: una lista cuyos elementos son
;; temperaturas en grados Celsius.

;; Ejemplos:
(define LISTA-C1 (list 0 60 100))
(define LISTA-C2 (list 260 -40))

;; Fahrenheit-a-Celsius : Number -> Number
;; Dada una temperatura en grados Fahrenheit, devuelve
;; su equivalente en grados Celsius.

(check-expect (Fahrenheit-a-Celsius 32) 0)
(check-expect (Fahrenheit-a-Celsius 212) 100)

(define (Fahrenheit-a-Celsius t)
  (* (- t 32) (/ 5 9)))

;; convertirFC : ListaTempF -> ListaTempC
;; Dada una lista de temperaturas en grados Fahrenheit,
;; devuelve esta lista de temperaturas convertidas en grados Celsius.

(check-expect (convertirFC empty) empty)
(check-expect (convertirFC LISTA-F1) LISTA-C1)
(check-expect (convertirFC LISTA-F2) LISTA-C2)

(define (convertirFC l)
  (cond [(empty? l) empty]
        [else (cons (Fahrenheit-a-Celsius (first l)) (convertirFC (rest l)))]))