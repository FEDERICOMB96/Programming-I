;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TP6-Mettini-Ruiz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#| Trabajo Práctico 6

Integrantes:
- Mettini, Fabrizio, comisión 3.
- Ruiz, Fabricio, comisión 4.
|#

;;;;;;;; Ejercicio 1

;; implica : Boolean Boolean -> Boolean
;; Dados dos valores booleanos a y b, devuelve a -> b.

(check-expect (implica #false #false) #true)
(check-expect (implica #false #true) #true)
(check-expect (implica #true #false) #false)
(check-expect (implica #true #true) #true)

(define (implica a b)
  (or (not a) b))
  
;; equivalente : Boolean Boolean -> Boolean
;; Dados dos valores booleanos a y b, devuelve a <-> b.

(check-expect (equivalente #false #false) #true)
(check-expect (equivalente #false #true) #false)
(check-expect (equivalente #true #false) #false)
(check-expect (equivalente #true #true) #true)

(define (equivalente a b)
  (boolean=? a b))

;;;;;;;; Ejercicio 2

;; fila : Natural Natural -> List(Boolean)
;; Dados dos números naturales m y n, genera la valuación de la fila m para
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

;;;;;;;; Ejercicio 3

;; A : List(Boolean) -> Boolean
;; A representa la fórmula proposicional ((p -> r) /\ (q -> r)) <-> ((p \/ q) -> r)

(check-expect (A (list #t #f #f)) #true)
(check-expect (A (list #t #t #t)) #true)

(define (A l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
    (equivalente (and (implica p1 p3)
                      (implica p2 p3))
                 (implica (or p1 p2) p3))))

;; B : List(Boolean) -> Boolean
;; B representa la fórmula proposicional ((p /\ q) -> r) <-> ((p -> r) /\ (q -> r))

(check-expect (B (list #f #t #f)) #false)
(check-expect (B (list #t #t #t)) #true)

(define (B l)
  (let ([p1 (first l)]
        [p2 (second l)]
        [p3 (third l)])
    (equivalente (implica (and p1 p2) p3)
                 (and (implica p1 p3)
                      (implica p2 p3)))))

;; C: List(Boolean) -> Boolean
;; C representa la fórmula proposicional (~p \/ ~q) <-> (p /\ q)

(check-expect (C (list #f #t)) #false)
(check-expect (C (list #t #t)) #false)

(define (C l)
  (let ([p1 (first l)]
        [p2 (second l)])
    (equivalente (or (not p1) (not p2))
                 (and p1 p2))))

;;;;;;;; Ejercicio 4

;; evaluar : (List(Boolean) -> Boolean) Natural -> List(Boolean)
;; Dada una fórmula proposicional P y la cantidad de variables n que utiliza P,
;; devuelve una lista con booleanos, que son el resultado de evaluar P en cada una de las posibles
;; valuaciones.

(check-expect (evaluar A 3) (list #t #t #t #t #t #t #t #t))
(check-expect (evaluar C 2) (list #f #f #f #f))

(define (evaluar P n)
  (map P (valuaciones n)))

;;;;;;;; Ejercicio 5
;; f-and : Boolean Boolean -> Boolean
;; Dados dos valores booleanos a y b, devuelve a and b.

(check-expect (f-and #t #f) #f)
(check-expect (f-and #t #t) #t)

(define (f-and a b) (and a b))

;; f-or : Boolean Boolean -> Boolean
;; Dados dos valores booleanos a y b, devuelve a or b.

(check-expect (f-or #f #f) #f)
(check-expect (f-or #t #f) #t)

(define (f-or a b) (or a b))

;; tautología? : (List(Boolean) -> Boolean) Natural -> Boolean
;; Dada una fórmula proposicional P y un número natural n, que representa la cantidad de variables,
;; devuelve si P es una tautología.

(check-expect (tautología? A 3) #true)
(check-expect (tautología? B 3) #false)
(check-expect (tautología? C 2) #false)

(define (tautología? P n)
  (foldr f-and #t (evaluar P n)))

#| Otras posibles soluciones:

(define (tautología? P n)
  (andmap identity (evaluar P n)))

(define (tautología? P n)
  (not (ormap not (evaluar P n))))

|#

;; contradicción? : (List(Boolean) -> Boolean) Natural -> Boolean
;; Dada una fórmula proposicional P y un número natural n, que representa la cantidad de variables,
;; devuelve si P es una contradicción.

(check-expect (contradicción? A 3) #false)
(check-expect (contradicción? B 3) #false)
(check-expect (contradicción? C 2) #true)

(define (contradicción? P n)
  (not (satisfactible? P n)))

#| Otra posible solución:

(define (contradicción? P n)
  (andmap false? (evaluar P n)))

|#

;; satisfactible? : (List(Boolean) -> Boolean) Natural -> Boolean
;; Dada una fórmula proposicional P y un número natural n, que representa la cantidad de variables,
;; devuelve si P es un satisfactible.

(check-expect (satisfactible? A 3) #true)
(check-expect (satisfactible? B 3) #true)
(check-expect (satisfactible? C 2) #false)

(define (satisfactible? P n)
  (foldr f-or #f (evaluar P n)))

#| Otras posibles soluciones:

(define (satisfactible? P n)
  (ormap identity (evaluar P n)))

(define (satisfactible? P n)
  (not (andmap not (evaluar P n))))

|#