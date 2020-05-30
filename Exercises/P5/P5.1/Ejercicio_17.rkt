;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 17

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA1 (list 1 2 3 2 7 6))
(define LISTA2 (list 0 3 0 3 4 5))

;; eliminar : Lista-de-numeros Number -> Lista-de-numeros
;; Dada una lista de números l y un número n,
;; devuelve la lista que resulta de eliminar en l todas las ocurrencias de n.

(check-expect (eliminar empty 0) empty)
(check-expect (eliminar LISTA1 2) (list 1 3 7 6))
(check-expect (eliminar LISTA1 0) LISTA1)
(check-expect (eliminar LISTA2 3) (list 0 0 4 5))
(check-expect (eliminar LISTA2 6) LISTA2)

(define (eliminar l n)
  (cond [(empty? l) empty]
        [else (if (not (= (first l) n))
                  (cons (first l) (eliminar (rest l) n))
                  (eliminar (rest l) n))]))
