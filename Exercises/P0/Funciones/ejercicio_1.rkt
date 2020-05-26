;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ejercicio_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (cuad-azul x) (square x "solid" "blue"))
(cuad-azul (doble 10))

(define (h x y) (< x (* 2 y)))
(and (h 2 3) (h 3 4))

(define (doble x) (* x 2))
(= (f 1) (doble 1))