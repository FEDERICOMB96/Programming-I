;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ejercicio_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;a.
(string-append "Hola" "mundo")

;b.
(string-append "Pro" "gra" "ma.")

;c.
(number->string 1357)

;d.
;(string-append "La respuesta es " (+ 21 21)) ERROR

;e.
(string-append "La respuesta es " (number->string (+ 21 21)))

;f.
(* (string-length "Hola") (string-length "Chau"))

;string-ith function
(string-ith "hola" 3)

;substring function
(substring "Hola Mundo" 0 4)