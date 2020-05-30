;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 2

;; Contactos es:
;; - una lista vacia '() o
;; - una expresión del tipo (cons un-nombre-persona Contactos)


;; Explicación de por que la siguiente lista:
;; (cons "1" (cons "2" '())) es un ejemplo de lista de Contactos:

;; La lista vacia '() es un lista de contactos.
;; Si agregamos el elemento "2" a la lista existente tambien tenemos una lista de contactos,
;; ya que (cons "2" '()) es una expresión válida.
;; Y si agregamos el elemento "1" a la lista existente tambien tenemos una lista de contactos,
;; ya que (cons "1" (cons "2" '())) es una expresión válida.
;; Por lo tanto, (cons "1" (cons "2" '())) es un ejemplo de lista de Contactos.


;; Explicación de por que la siguiente lista:
;; (cons 2 '()) no es un ejemplo de lista de Contactos:

;; La lista vacia '() es un lista de contactos.
;; Si agregamos el elemento 2 a la lista existente dejamos de tener una lista de contactos,
;; ya que (cons "2" '()) no es una expresión válida.
;; Por lo tanto, (cons 2 '()) no es un ejemplo de lista de Contactos.
