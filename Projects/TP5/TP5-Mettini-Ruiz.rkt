;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname TP5-Mettini-Ruiz) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 5: Naturales

Integrantes:
- Mettini, Fabrizio, comisión 3.
- Ruiz, Fabricio, comisión 4.
|#

;; Un Natural es: 
;; – 0
;; – (add1 Natural)
;; interpretación: Natural representa los números naturales

;;;;;;;; Ejercicio 1

;; subir : Natural -> Natural
;; Dado un número natural n , determine de cuántas formas distintas se puede subir
;; una escalera de n escalones si se suben los escalones, únicamente, de a uno,
;; de a tres o de a cinco.

(check-expect (subir 0) 1) ; Caso Especial
(check-expect (subir 1) 1)
(check-expect (subir 2) 1)
(check-expect (subir 3) 2)
(check-expect (subir 4) 3)
(check-expect (subir 5) 5)
(check-expect (subir 6) 8)
(check-expect (subir 7) 12)

(define (subir n)
  (cond [(zero? n) 1]
        [(zero? (sub1 n)) 1]
        [(positive? n) (+ (subir (- n 1)) (subir (- n 3)) (subir (- n 5)))]
        [else 0]))

;;;;;;;; Ejercicio 2

;;;; Ejercicio 2-1

; Costo de los recorridos válidos:
; d : movimiento hacia la derecha (hacia la celda inmediata derecha)
; a : movimiento hacia abajo (hacia la celda inmediata inferior)

; * Costo = 20 (recorrido d-d-d-a-a)
; * Costo = 13 (recorrido d-d-a-d-a)
; * Costo = 12 (recorrido d-d-a-a-d)
; * Costo = 21 (recorrido d-a-d-d-a)
; * Costo = 20 (recorrido d-a-d-a-d)
; * Costo = 21 (recorrido d-a-a-d-d)
; * Costo = 18 (recorrido a-d-d-d-a)
; * Costo = 17 (recorrido a-d-d-a-d)
; * Costo = 18 (recorrido a-d-a-d-d)
; * Costo = -2 (recorrido a-a-d-d-d)

;;;; Ejercicio 2-2
#|

Version 1:
                    / tab[0][0]                                                 si i = 0, j = 0
                    | tab[0][j] + maximoCosto(0, j-1)                           si i = 0, j != 0
maximoCosto(i, j) = <
                    | tab[i][0] + maximoCosto(i-1, 0)                           si i != 0, j = 0
                    \ tab[i][j] + max(maximoCosto(i-1, j), maximoCosto(i, j-1)) si i != 0, j != 0

Version 2:
                    / tab[0][0]                                                 si i = 0, j = 0
                    | \sum_{k=0}^{j} tab[0][k]                                  si i = 0, j != 0
maximoCosto(i, j) = <
                    | \sum_{k=0}^{i} tab[k][0]                                  si i != 0, j = 0
                    \ tab[i][j] + max(maximoCosto(i-1, j), maximoCosto(i, j-1)) si i != 0, j != 0

|#

;;;; Ejercicio 2-3

(define TABLERO1 (list (list -5 10) (list 0 -2) (list 9 3)))
(define TABLERO2 (list (list 5 4 -10 15) (list 1 -2 8 2) (list -22 9 1 4)))
(define TABLERO3 (list (list 1 2) (list 3 -4)))

;; -------------------------------------
;; suma-lista : List(Number) Natural -> Number
;; Dada una lista de numeros l y un numero natural n, devuelve la suma de los primeros
;; n+1 numeros de l.

(check-expect (suma-lista (list 1 3 2 4 5) 0) 1)
(check-expect (suma-lista (list 1 3 2 4 5) 4) 15)

(define (suma-lista l n)
  (cond [(zero? n) (first l)]
        [else (+ (first l) (suma-lista (rest l) (sub1 n)))]))

;; -------------------------------------
;; primero-cada-sublista : List(List(Any)) -> List(Any)
;; Dada una lista de listas de elementos de cualquier tipo, devuelve una lista con el
;; primer elemento de cada sublista.

(check-expect (primero-cada-sublista (list (list 1 2) (list 3 5) (list 4 1))) (list 1 3 4))
(check-expect (primero-cada-sublista (list (list "h" "b") (list "i" "z"))) (list "h" "i"))

(define (primero-cada-sublista l)
  (cond [(empty? l) empty]
        [else (cons (first (first l)) (primero-cada-sublista (rest l)))]))

;; -------------------------------------
;; indice : List(Any) Natural -> Any
;; Dada una lista con elementos de cualquier tipo l y un numero natural n,
;; devuelve el elemento de indice n de la lista l (siendo 0 <= n < (length l)).

(define A (list (list 1 2 3) (list 2 3 4) (list 3 4 5)))

(check-expect (indice (list 0 1 2 3 4 5 6) 0) 0)
(check-expect (indice (list 0 1 2 3 4 5 6) 3) 3)
(check-expect (indice A 2) (list 3 4 5))
(check-expect (indice (indice A 2) 2) 5)

(define (indice l n)
  (cond [(zero? n) (first l)]
        [else (indice (rest l) (sub1 n))]))

;; -------------------------------------
;; Version 1:
;; maximo-costo : List(List(Integer)) Natural Natural -> Integer
;; Dados una lista de listas de enteros tab, que representa un tablero de m filas y n columnas,
;; y dos naturales i y j devuelve el máximo costo de un recorrido válido de (0, 0) a (i, j)
;; en el tablero tab. 0 ≤ i < n, 0 ≤ j < m.

(check-expect (maximo-costo TABLERO1 1 2) 7)
(check-expect (maximo-costo TABLERO2 3 2) 21)
(check-expect (maximo-costo TABLERO3 0 0) 1)

(define (maximo-costo tab i j)
  (cond [(and (zero? i) (zero? j)) (first (first tab))]
        [(zero? i) (+ (first (indice tab j)) (maximo-costo tab 0 (sub1 j)))]
        [(zero? j) (+ (indice (first tab) i) (maximo-costo tab (sub1 i) 0))]           
        [else (+ (indice (indice tab j) i)
                 (max (maximo-costo tab (sub1 i) j) (maximo-costo tab i (sub1 j))))]))

#|
;; Version 2:
;; maximo-costo : List(List(Integer)) Natural Natural -> Integer
;; Dados una lista de listas de enteros tab, que representa un tablero de m filas y n columnas,
;; y dos naturales i y j devuelve el máximo costo de un recorrido válido de (0, 0) a (i, j)
;; en el tablero tab. 0 ≤ i < n, 0 ≤ j < m.

(check-expect (maximo-costo TABLERO1 1 2) 7)
(check-expect (maximo-costo TABLERO2 3 2) 21)

(define (maximo-costo tab i j)
  (cond [(and (zero? i) (zero? j)) (first (first tab))]
        [(zero? i) (suma-lista (primero-cada-sublista tab) j)]
        [(zero? j) (suma-lista (first tab) i)]                 
        [else (+ (indice (indice tab j) i)
                 (max (maximo-costo tab (sub1 i) j) (maximo-costo tab i (sub1 j))))]))
|#

;;;; Ejercicio 2-4
;; -------------------------------------
;; maximo-lista : List(Number) -> Number
;; Dada una lista de numeros, devuelve el mayor elemento de la misma.
;; La lista no puede ser vacia.

(check-expect (maximo-lista (list 3)) 3)
(check-expect (maximo-lista (list -2 -1 -3)) -1)
(check-expect (maximo-lista (list 3 1 5 7 2)) 7)

(define (maximo-lista l)
  (cond [(empty? (rest l)) (first l)]                
        [else (max (first l) (maximo-lista (rest l)))]))

#|
Nota: Se evito definir maximo-lista de la siguiente manera:

(define (maximo-lista l)
  (foldr max 0 l))

Ya que fallaria en algunos casos:
Ejemplo 1: (maximo-lista empty) == 0         No tiene sentido calcular el maximo de una lista vacia.
Ejemplo 2: (maximo-lista (list -1 -2)) == 0  El resultado deberia ser -1 y no 0.
|#

#|
;; -------------------------------------
;; No fue necesaria esta funcion
;; maximo-lista-de-listas : List(List(Number)) Natural -> Number
;; Dada una lista de listas de enteros y un natural m, devuelve el maximo
;; elemento de la lista.

(define TAB1 (list (list 3 2 4 4) (list 5 2 3 76) (list 7 1 2 4)))
(define TAB2 (list (list 69 420 00 6) (list 7 33 13 12)))

(check-expect (maximo-lista-de-listas TAB1) 76)
(check-expect (maximo-lista-de-listas TAB2) 420)

(define (maximo-lista-de-listas tab)
  (cond [(empty? (rest tab)) (maximo-lista (first tab))]
        [else (max (maximo-lista (first tab))
                   (maximo-lista-de-listas (rest tab)))]))
|#

;; -------------------------------------
;; maximo-costo-lista : List(List(Integer)) Natural Natural -> List(Integer)
;; Dado una lista de listas de enteros tab, que representa un tablero de m filas y n columnas,
;; y dos naturales i y j, devuelve una lista con los maximos costos de un recorrido válido
;; de (0, 0) a (i, j) en el tablero tab. 0 ≤ i < n, 0 <= j < m, j fijo.

(check-expect (maximo-costo-lista TABLERO1 1 0) (list 5 -5))
(check-expect (maximo-costo-lista TABLERO2 3 0) (list 14 -1 9 5))
(check-expect (maximo-costo-lista TABLERO3 1 0) (list 3 1))

(define (maximo-costo-lista tab i j)
  (cond [(zero? i) (cons (maximo-costo tab 0 j) '())]
        [else (cons (maximo-costo tab i j)
                    (maximo-costo-lista tab (sub1 i) j))]))

;; -------------------------------------
;; maximo-costo-cada-casilla : List(List(Integer)) Natural Natural -> List(Integer)
;; Dado una lista de listas de enteros tab, que representa un tablero de m filas y n columnas,
;; y dos naturales i y j, devuelve una lista con los maximos costos de un recorrido válido
;; de (0, 0) a (i, j) en el tablero tab. 0 ≤ i < n, 0 <= j < m.

(check-expect (maximo-costo-cada-casilla TABLERO1 1 2) (list 7 4 3 -5 5 -5))
(check-expect (maximo-costo-cada-casilla TABLERO2 3 2) (list 21 17 16 -16 17 15 7 6 14 -1 9 5))
(check-expect (maximo-costo-cada-casilla TABLERO3 1 1) (list 0 4 3 1))

(define (maximo-costo-cada-casilla tab i j)
  (cond [(zero? j) (maximo-costo-lista tab i 0)]
        [else (append (maximo-costo-lista tab i j)
                      (maximo-costo-cada-casilla tab i (sub1 j)))]))

;; -------------------------------------
;; maximo-costo-tablero : List(List(Integer)) -> Integer
;; Dada una lista de listas de enteros tab, que representa un tablero de m filas y n columnas,
;; devuelve el máximo costo que puede tener un recorrido válido en tab.

(check-expect (maximo-costo-tablero TABLERO1) 7)
(check-expect (maximo-costo-tablero TABLERO2) 21)
(check-expect (maximo-costo-tablero TABLERO3) 4)

(define (maximo-costo-tablero tab)
  (maximo-lista
   (maximo-costo-cada-casilla tab (sub1 (length (first tab))) (sub1 (length tab)))))