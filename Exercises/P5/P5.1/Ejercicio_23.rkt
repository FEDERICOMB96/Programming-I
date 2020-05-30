;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_23) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 23

;; Una Lista-de-strings es:
;; - empty
;; - (cons String Lista-de-strings)
;; Interpretación: una lista cuyos elementos son strings.

;; Ejemplos:
(define LISTA1 (list "hola" "cómo" "estás?"))
(define LISTA2 (list "A" "Nisman" "lo" "mataron"))

;; Una Lista-de-longitudes es:
;; - empty
;; - (cons Number Lista-de-longitudes)
;; Interpretación: una lista cuyos elementos son las
;; longitudes de strings.

;; Ejemplos:
(define LISTA-A (list 4 4 6))
(define LISTA-B (list 1 7 2 7))

;; longitudes : Lista-de-strings -> Lista-de-longitudes
;; dada una lista de cadenas, devuelve la lista de sus longitudes
;; (es decir, la cantidad de caracteres que contienen).

(check-expect (longitudes LISTA1) LISTA-A)
(check-expect (longitudes LISTA2) LISTA-B)

(define (longitudes l)
  (cond [(empty? l) empty]
        [else (cons (string-length (first l)) (longitudes (rest l)))]))