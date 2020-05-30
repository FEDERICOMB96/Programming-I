;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_20) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 20

;; Una Lista-de-imagenes es:
;; - empty
;; - (cons Image Lista-de-imagenes)
;; Interpretación: una lista cuyos elementos son imagenes.

;; Ejemplos:
(define LISTA1 (list (circle 30 "solid" "red") (rectangle 10 30 "outline" "blue")))
(define LISTA2 (list (square 40 "solid" "slateblue") (ellipse 30 60 "solid" "pink")))

;; Una Lista-de-numeros es:
;; - empty
;; - (cons Number Lista-de-numeros)
;; Interpretación: una lista cuyos elementos son números.

;; Ejemplos:
(define LISTA-A (list 60 10))
(define LISTA-B (list 40 30))

;; anchos : Lista-de-imagenes -> Lista-de-numeros
;; Dada una lista de imágenes,
;; devuelve una lista con el ancho de cada una.

(check-expect (anchos LISTA1) LISTA-A)
(check-expect (anchos LISTA2) LISTA-B)

(define (anchos l)
  (cond [(empty? l) empty]
        [else (cons (image-width (first l)) (anchos (rest l)))]))