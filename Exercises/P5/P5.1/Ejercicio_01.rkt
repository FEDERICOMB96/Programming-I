;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 1

;; Contactos es:
;; - una lista vacia '() o
;; - una expresión del tipo (cons un-nombre-persona Contactos)

(cons "Dante"
      (cons "Eugenia"
            (cons "Laura"
                  (cons "Juan Manuel"
                        (cons "Fernando" '())))))