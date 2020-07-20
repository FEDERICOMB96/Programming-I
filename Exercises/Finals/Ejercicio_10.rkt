;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 10

;; menor-longitud : String String -> String
;; Dados dos strings s1 y s2, devuelve el de menor longitud.
;; En caso de tener la misma longitud devuelve s2.

(define (menor-longitud s1 s2)
  (if (< (string-length s1) (string-length s2)) s1 s2))

;; mayor-longitud : String String -> String
;; Dados dos strings s1 y s2, devuelve el de mayor longitud.
;; En caso de tener la misma longitud devuelve s2.

(define (mayor-longitud s1 s2)
  (if (> (string-length s1) (string-length s2)) s1 s2))

;; shortest : List(String) -> String
;; Dado una lista NO VACIA de strings, devuelve el de menor longitud.

(define (shortest l)
  (cond [(empty? (rest l)) (first l)]
        [else (menor-longitud (first l) (shortest (rest l)))]))

;; longest : List(String) -> String
;; Dado una lista NO VACIA de strings, devuelve el de mayor longitud.

(define (longest l)
  (cond [(empty? (rest l)) (first l)]
        [else (mayor-longitud (first l) (longest (rest l)))]))

;; shortest-longest : List(String) -> Posn/String
;; Dada una lista de strings, devuelva una estructura posn que contenga el último string de menor
;; longitud y el último string de mayor longitud de la lista. En caso de que la lista recibida sea
;; vacía, deberá mostrar un mensaje indicándolo.

(define LISTA-VACIA "La lista recibida es vacia")

(check-expect (shortest-longest empty) LISTA-VACIA)
(check-expect (shortest-longest (list "a")) (make-posn "a" "a"))
(check-expect (shortest-longest (list "a" "acb" "b" "xf" "asdf")) (make-posn "b" "asdf"))

(define (shortest-longest l)
  (cond [(empty? l) LISTA-VACIA]
        [else (make-posn (shortest l) (longest l))]))

#| Otra solucion posible:

;; longitud : Procedure String String -> String
;; Dado un procedimiento p y dos strings s1 y s2, devuelve el string que surge de aplicar
;; p a s1 y s2.

(check-expect (longitud < "a" "ba") "a")
(check-expect (longitud < "ab" "ba") "ba")
(check-expect (longitud > "ab" "b") "ab")
(check-expect (longitud > "ab" "ba") "ba")

(define (longitud p s1 s2)
  (if (p (string-length s1) (string-length s2)) s1 s2))

;; mayor-o-menor : Procedure List(String) -> String
;; Dado un procedimiento p y una lista NO VACIA de strings l, devuelve el string que surge
;; de aplicar p a cada par de elementos de l.

(check-expect (mayor-o-menor < (list "a" "abc" "cd")) "a")
(check-expect (mayor-o-menor < (list "a" "abc" "b" "cd")) "b")
(check-expect (mayor-o-menor > (list "a" "ab" "b" "cd")) "cd")
(check-expect (mayor-o-menor > (list "a" "abc" "b" "cd")) "abc")

(define (mayor-o-menor p l)
  (cond [(empty? (rest l)) (first l)]
        [else (longitud p (first l) (mayor-o-menor p (rest l)))]))

;; shortest-longest : List(String) -> Posn/String
;; Dada una lista de strings, devuelva una estructura posn que contenga el último string de menor
;; longitud y el último string de mayor longitud de la lista. En caso de que la lista recibida sea
;; vacía, deberá mostrar un mensaje indicándolo.

(define LISTA-VACIA "La lista recibida es vacia")

(check-expect (shortest-longest empty) LISTA-VACIA)
(check-expect (shortest-longest (list "a")) (make-posn "a" "a"))
(check-expect (shortest-longest (list "a" "acb" "b" "xf" "asdf")) (make-posn "b" "asdf"))

(define (shortest-longest l)
  (cond [(empty? l) LISTA-VACIA]
        [else (make-posn (mayor-o-menor < l) (mayor-o-menor > l))]))

|#