;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 3

;; fold : (X X -> X) X List(X) -> X
;; Dadas una funcion f, un elemento c en X y una lista de objetos en X,
;; devuelve el resultado de aplicar f entre todos los elementos de la lista,
;; siendo c el valor devuelto para la lista vacia.

(define (fold f c l)
  (cond [(empty? l) c]
        [else (f (first l) (fold f c (rest l)))]))

;; -----------------
;; Ejercicio 9

;; aux1 : Bool Bool -> Bool
;; Dados dos valores booleanos, devuelve si ambos son #true.

(check-expect (aux1 #t #t) #t)
(check-expect (aux1 #f #t) #f)

(define (aux1 e1 e2)
  (and e1 e2))

;; todos-verdaderos : List(Bool) -> Bool
;; Dada una lista de valores booleanos, devuelve #true unicamente si todos los
;; elementos de la lista son #true.

(check-expect (todos-verdaderos empty) #t)
(check-expect (todos-verdaderos (list #t #f #f #t #t)) #f)
(check-expect (todos-verdaderos (list #t #t #t #t #t)) #t)

(define (todos-verdaderos l)
  (fold aux1 #t l))

;; -----------------
;; aux2 : Bool Bool -> Bool
;; Dados dos valores booleanos, devuelve si alguno de los dos es #true.

(check-expect (aux2 #t #f) #t)
(check-expect (aux2 #f #f) #f)

(define (aux2 e1 e2)
  (or e1 e2))

;; uno-verdadero : List(Bool) -> Bool
;; Dada una lista de valores booleanos, devuelve #true unicamente si al menos
;; uno de los elementos de la lista es #true.

(check-expect (uno-verdadero empty) #f)
(check-expect (uno-verdadero (list #f #f #f #f #f)) #f)
(check-expect (uno-verdadero (list #t #f #t #f #f)) #t)

(define (uno-verdadero l)
  (fold aux2 #f l))

;; -----------------
;; Ejercicio 10

;; aux3 : Any -> 1
;; Dado un elemento de cualquier tipo, devuelve 1.

(check-expect (aux3 420) 1)
(check-expect (aux3 "Hello World") 1)

(define (aux3 x) 1)

;; cant-elementos : List(Any) -> Number
;; Dada una lista, devuelve la cantidad de elementos que contiene.

(check-expect (cant-elementos empty) 0)
(check-expect (cant-elementos (list "Hello" 42 #t)) 3)

(define (cant-elementos l)
  (fold + 0 (map aux3 l)))

;; -----------------
;; Ejercicio 25

;; prod : List(Number) -> Number
;; Dada una lista de números, multiplica los elementos de la lista entre sí.
;; Para la lista vacía, devuelve 1.

(check-expect (prod empty) 1)
(check-expect (prod (list 1 2 3 4 5)) 120)

(define (prod l)
  (fold * 1 l))

;; -----------------
;; Ejercicio 26

;; pegar : List(String) -> String
;; dada una lista de strings, devuelve el string que se obtiene de concatenar
;; todos los elementos de la lista.

(check-expect (pegar empty) "")
(check-expect (pegar (list "Con " "15 " "pesos " "me " "hago " "alto " "guiso"))
               "Con 15 pesos me hago alto guiso")

(define (pegar l)
  (fold string-append "" l))

;; -----------------
;; Ejercicio 27

;; maximo : List(Number) -> Number
;; Dada una lista de números naturales, devuelve el máximo.
;; Para la lista vacía, devuelve 0.

(check-expect (maximo empty) 0)
(check-expect (maximo (list 1 3 4 2 5)) 5)

(define (maximo l)
  (fold max 0 l))