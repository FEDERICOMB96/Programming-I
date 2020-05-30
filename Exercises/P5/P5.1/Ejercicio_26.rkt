;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_26) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 26

;; Una Lista-de-strings es:
;; - empty
;; - (cons String Lista-de-strings)
;; Interpretación: una lista cuyos elementos son strings.

;; Ejemplos:
(define LISTA1 (list "Las " "lis" "tas " "son " "complicadas" "."))
(define LISTA2 (list "hello " "darkness " "my " "old " "friend."))

;; pegar : Lista-de-strings -> String
;; Dada una lista de strings, devuelve el string que se
;; obtiene de concatenar todos los elementos de la lista.

(check-expect (pegar empty) "")
(check-expect (pegar LISTA1) "Las listas son complicadas.")
(check-expect (pegar LISTA2) "hello darkness my old friend.")

(define (pegar l)
  (cond [(empty? l) ""]
        [else (string-append (first l) (pegar (rest l)))]))