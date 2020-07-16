;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicio_01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 1

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;; -----------------------------------------------
;; Practica 6 - Ejercicio 6

;; intervalo-aux : Natural -> List(Natural)
;; Dado un número natural n, devuelve la lista (list n (n-1) ... 1).
;; Para 0 devuelve '().

(check-expect (intervalo-aux 0) '())
(check-expect (intervalo-aux 3) (list 3 2 1))

(define (intervalo-aux n)
  (cond [(zero? n) '()]
        [else (cons n (intervalo-aux (sub1 n)))]))

;; intervalo : Natural -> List(Natural)
;; Dado un número natural n, devuelve la lista (list 1 2 ... n).
;; Para 0 devuelve empty.

(check-expect (intervalo 0) empty)
(check-expect (intervalo 3) (list 1 2 3))

(define (intervalo n)
  (reverse (intervalo-aux n)))

;; -----------------------------------------------
(define CARAS 6)

;; simular-dado : Natural -> List(Natural)
;; Dada un número natural n, devuelve una lista con n números aleatorios entre 1 y CARAS.

(check-expect (simular-dado 0) empty)

(define (simular-dado n)
  (cond [(zero? n) empty]
        [else (cons (+ 1 (random CARAS)) (simular-dado (sub1 n)))]))

(define MAX 60000)
(define EXPERIMENTO (simular-dado MAX))
(define VALORES (intervalo CARAS))

;; -----------------------------------------------
;; frecuencia : Natural List(Natural) -> Natural
;; Dado un número natural n y una lista de naturales,
;; devuelve la cantidad de veces que n aparece en la lista. Es decir, su frecuencia absoluta.

(check-expect (frecuencia 5 empty) 0)
(check-expect (frecuencia 1 (list 2 3)) 0)
(check-expect (frecuencia 1 (list 2 1)) 1)
(check-expect (frecuencia 3 (list 1 2 3 4 2 3)) 2)

(define (frecuencia n l)
  (cond [(empty? l) 0]
        [else (+ (if (= n (first l)) 1 0) (frecuencia n (rest l)))]))

;; -----------------------------------------------
;; frecuencia-relativa : Natural List(Natural) -> Number
;; Dado un número natural n y una lista de naturales,
;; devuelve la proporción de veces que aparece n en la lista.

(check-expect (frecuencia-relativa 1 (list 1 1)) 1)
(check-expect (frecuencia-relativa 3 (list 1 2 3 4 2 3)) (/ 1 3))
(check-expect (frecuencia-relativa 5 (list 1 2 3 4 6 7)) 0)

(define (frecuencia-relativa n l)
  (/ (frecuencia n l) (length l)))

;; -----------------------------------------------
;; frec-rel-exp : Natural -> Number
;; Devuelve la frecuencia relativa de un valor en nuestro EXPERIMENTO.

(define (frec-rel-exp n)
  (frecuencia-relativa n EXPERIMENTO))

;; -----------------------------------------------
;; FRECUENCIAS-RELATIVAS calcula las frecuencias relativas de cada valor en el experimento.
(define FRECUENCIAS-RELATIVAS (map frec-rel-exp VALORES))

;; -----------------------------------------------
;; Preguntas:
;; 1 - ¿Qué valor de frecuencia aproximado debería repetirse en la lista FRECUENCIAS-RELATIVAS?
;; 2 - Para estar más cerca del valor esperado, ¿MAX debe ser más grande o más chico?
;; 3 - a) ¿Qué pasa si usamos 2 como valor para CARAS?
;; 3 - b) ¿Qué frecuencias relativas deberíamos esperar?
;; 3 - c) ¿Qué experimento estaríamos simulando?

;; Respuestas:
;; 1 - El valor de frecuencia aproximado que debería repetirse en la lista es 1/6
;; aproximadamente 1.667
;; 2 - Para estar más cerca del valor esperado, MAX debe ser más grande.

;; Para CARAS = 2
;; 3 - a) Se modifica totalmente el experimento.
;; 3 - b) El valor de frecuencia aproximado que debería repetirse en la lista es 1/2 = 0.5
;; 3 - c) Estariamos simulando el lanzamiento de una moneda.

;; Para CARAS = 37
;; 3 - a) Se modifica totalmente el experimento.
;; 3 - b) El valor de frecuencia aproximado que debería repetirse en la lista es 1/37
;; aproximadamente 0.027027027
;; 3 - c) Estariamos simulando el repartimiento de una baraja de cartas.

;; -----------------------------------------------
;; Considere ahora que el experimento es tirar dos dados de seis caras.
;; En este caso, la frecuencia esperada para cada resultado posible no es la misma.
;; Diseñe un programa que calcule las frecuencias relativas de todos los posibles
;; valores del experimento, tomando como base el programa de más arriba.


