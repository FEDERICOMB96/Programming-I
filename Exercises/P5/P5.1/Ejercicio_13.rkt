;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_13) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 13

;; Una Lista-de-strings es:
;; - empty
;; - (cons String Lista-de-strings)
;; Interpretación: una lista cuyos elementos son strings.

;; Ejemplos:
(define LISTA1 (list "Lista" "de" "palabras" "sin" "sentido"))
(define LISTA2 (list "Massachusetts" "Connecticut" "Colorado"))

;; cortas : Lista-de-strings -> Lista-de-strings
;; Recibe una lista de strings y devuelve una lista con aquellas
;; palabras de longitud menor a 5.

(check-expect (cortas empty) empty)
(check-expect (cortas LISTA1) (list "de" "sin"))
(check-expect (cortas LISTA2) empty)

(define (cortas l)
  (cond [(empty? l) empty]
        [else (if (< (string-length (first l)) 5)
                  (cons (first l) (cortas (rest l)))
                  (cortas (rest l)))]))