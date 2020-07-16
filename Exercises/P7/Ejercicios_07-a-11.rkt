;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicios_07-a-11) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicios 7 a 11

;; -----------------------------------------------
;; Ejercicio 7
;; implica : Boolean Boolean -> Boolean

(check-expect (implica #false #false) #true)
(check-expect (implica #false #true) #true)
(check-expect (implica #true #false) #false)
(check-expect (implica #true #true) #true)

(define (implica a b)
  (not (and a (not b))))
  
;; equivalente : Boolean Boolean -> Boolean

(check-expect (equivalente #false #false) #true)
(check-expect (equivalente #false #true) #false)
(check-expect (equivalente #true #false) #false)
(check-expect (equivalente #true #true) #true)

(define (equivalente a b)
  (if (boolean=? a b) #true #false))

;; -----------------------------------------------
;; Ejercicio 8

;; fila : Natural Natural -> List(Boolean)
;; Dados dos números naturales m y n, genera la valuacion de la fila m para
;; n variables proposicionales.

(check-expect (fila 0 2) (list #false #false))
(check-expect (fila 2 2) (list #true #false))

(define (fila m n)
  (cond [(zero? n) empty]
        [else (cons (>= (modulo m (expt 2 n)) (expt 2 (sub1 n))) (fila m (sub1 n)))]))

;; valuaciones-aux : Natural Natural -> List(List(Boolean))
;; Dado dos números naturales m y n, genera todas las m posibles valuaciones
;; que existen con n variables proposicionales.

(check-expect (valuaciones-aux 2 1) (list (list #true) (list #false)))
(check-expect (valuaciones-aux 4 2) (list (list #true #true)
                                          (list #true #false)
                                          (list #false #true)
                                          (list #false #false)))

(define (valuaciones-aux m n)
  (cond [(zero? m) empty]
        [else (cons (fila (sub1 m) n) (valuaciones-aux (sub1 m) n))]))

;; valuaciones : Natural -> List(List(Boolean))
;; Dado un número natural n, genera todas las posibles valuaciones
;; que existen con n variables proposicionales.

(check-expect (valuaciones 0) empty)
(check-expect (valuaciones 1) (list (list #false) (list #true)))
(check-expect (valuaciones 2) (list (list #false #false)
                                    (list #false #true)
                                    (list #true #false)
                                    (list #true #true)))

(define (valuaciones n)
  (cond [(zero? n) empty]
        [else (reverse (valuaciones-aux (expt 2 n) n))]))

;; -----------------------------------------------
;; Ejercicio 9

;; A : List(Boolean) -> Boolean
(check-expect (A (list #t #t #t)) #true)

(define (A l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
    (equivalente (and (implica p1 p3)
                      (implica p2 p3))
                 (implica (or p1 p2) p3))))

;; B : List(Boolean) -> Boolean
(check-expect (B (list #t #t #t)) #true)

(define (B l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
    (equivalente (implica (and p1 p2) p3)
                 (and (implica p1 p3)
                      (implica p2 p3)))))

;; C: List(Boolean) -> Boolean
(check-expect (C (list #t #t)) #false)

(define (C l)
  (let ([p1 (first l)]
        [p2 (second l)])
    (equivalente (or (not p1) (not p2))
                 (and p1 p2))))


;; -----------------------------------------------
;; Ejercicio 10

(check-expect (evaluar A 3) (list #t #t #t #t #t #t #t #t))
(check-expect (evaluar C 2) (list #f #f #f #f))

;; evaluar : (List(Boolean) -> Boolean) Natural -> List(Boolean)

(define (evaluar f n)
  (map f (valuaciones n)))

;; -----------------------------------------------
;; Ejercicio 11

;; true? : Boolean -> Boolean
;; Devuelve si un valor es true.

(check-expect (true? #true) #true)
(check-expect (true? #false) #false)

(define (true? b) b)

;; tautología? : (List(Boolean) -> Boolean) Natural -> Boolean
;; Dada una formula proposicional f y un numero natural n, que representa la cantidad de variables
;; devuelve si f es una tautologia.

(check-expect (tautología? A 3) #true)
(check-expect (tautología? B 3) #false)
(check-expect (tautología? C 2) #false)

(define (tautología? f n)
  (andmap true? (evaluar f n)))

#| Alternativamente, sin usar true?

(define (tautología? f n)
  (not (ormap not (evaluar f n))))

|#

;; contradicción? : (List(Boolean) -> Boolean) Natural -> Boolean
;; Dada una formula proposicional f y un numero natural n, que representa la cantidad de variables
;; devuelve si f es una contradiccion.

(check-expect (contradicción? A 3) #false)
(check-expect (contradicción? B 3) #false)
(check-expect (contradicción? C 2) #true)

(define (contradicción? f n)
  (andmap false? (evaluar f n)))

;; satisfactible? : (List(Boolean) -> Boolean) Natural -> Boolean
;; Dada una formula proposicional f y un numero natural n, que representa la cantidad de variables
;; devuelve si f es un satisfacible.

(check-expect (satisfacible? A 3) #true)
(check-expect (satisfacible? B 3) #true)
(check-expect (satisfacible? C 2) #false)

(define (satisfacible? f n)
  (ormap true? (evaluar f n)))

#| Alternativamente, sin usar true?

(define (satisfacible? f n)
  (not (andmap not (evaluar f n))))

|#