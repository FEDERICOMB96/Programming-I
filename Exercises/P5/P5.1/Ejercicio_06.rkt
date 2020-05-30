;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 6

;; Contactos es:
;; - una lista vacia '() o
;; - una expresión del tipo (cons un-nombre-persona Contactos)

;; contiene-Marcos? : Contactos -> Booleano
;; Dada una lista de Contactos, determina si "Marcos" es un elemento de la misma.

(define (contiene-Marcos? l)
  (cond [(empty? l) #false]
        [(cons? l) (if (string=? (first l) "Marcos")
                       #true
                       (contiene-Marcos? (rest l)))]))

;; Evaluar las siguientes expresiones:
(contiene-Marcos? (cons "Marcos" (cons "C" '())))
(contiene-Marcos? (cons "A" (cons "Marcos" (cons "C" '()))))

;; ¿Qué pasa si reemplazamos a "Marcos" con "B"?
(contiene-Marcos? (cons "B" (cons "C" '())))
(contiene-Marcos? (cons "A" (cons "B" (cons "C" '()))))