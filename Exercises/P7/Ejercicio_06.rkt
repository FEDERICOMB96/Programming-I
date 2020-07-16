#lang racket 

;; -------------------------------------------------------------------------------------------------
;; Ejercicio 6

;; OBSERVACION: Se trabajo con racket en vez del lenguaje ASL para utilizar la funcion shuffle.

(define MAX 60000) ; Numero de Partidas a generar para nuestra estimacion
(define N 40)      ; Numero de Cartas

;; generar-partida : Natural -> List(posn)
;; Dado un natural n, devuelve una partida de n cartas.

(define (generar-partida n)
  (shuffle (range 1 (+ n 1) 1))) ; Genera una lista de n elementos (list 1 2 .. n) y luego la mezcla

;; generar-partidas : Natural -> List(posn)
;; Dado un natural n, devuelve n partidas de N cartas.

(define (generar-partidas n)
  (cond [(zero? n) empty]
        [else (cons (generar-partida N) (generar-partidas (- n 1)))]))

;; ganadora?-aux : List(Natural) Natural -> Boolean
;; Dada una partida p y un indice i, con 1 <= i <= (length p),
;; devuelve si es una partida ganadora.

;(check-expect (ganadora?-aux (list 2 1) 1) #true)
;(check-expect (ganadora?-aux (list 2 1 5 4 3) 1) #false)

(define (ganadora?-aux p i)
  (cond [(empty? p) #true]
        [else (if (= (first p) i)
                  #false
                  (ganadora?-aux (rest p) (add1 i)))]))

;; ganadora? : List(Natural) -> Boolean
;; Dada una partida p, devuelve si es una partida ganadora.

;(check-expect (ganadora? (list 2 1)) #true)
;(check-expect (ganadora? (list 2 1 5 4 3)) #false)

(define (ganadora? p)
  (ganadora?-aux p 1))

;; generamos una lista con muchas partidas
(define LISTA (generar-partidas MAX))

;; nos quedamos con aquellas que fueron ganadoras
(define GANADORAS (filter ganadora? LISTA))

;; aproximamos el porcentaje de ganadoras a partir de la proporción de partidas
;; que resultaron ganadoras con respecto al total de partidas simuladas:
(define GANAR (* 100 (/ (length GANADORAS) (length LISTA))))

(string-append "El porcentaje de ganar es: " (number->string (exact->inexact GANAR)))

; si la probabilidad de ganar es mayor a la de perder no es prudente aceptar la apuesta
(if (> GANAR 50) "No es prudente aceptar la apuesta" "Es prudente aceptar la apuesta")