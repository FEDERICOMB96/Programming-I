;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 9

;; subcadena? : List(String) String -> List(String)
;; Dada una lista de strings l y un string s devuelva otra lista que contenga únicamente los strings
;; de l que contienen como subcadena a s.

(check-expect (subcadena? (list "cadena" "Helena" "casa") "ena") (list "cadena" "Helena"))
(check-expect (subcadena? (list "Hola" "mundo") "eso") '())
(check-expect (subcadena? '() "altos") '())

(define (subcadena? l s)
  (cond [(empty? l) empty]
        [(string-contains? s (first l)) (cons (first l) (subcadena? (rest l) s))]
        [else (subcadena? (rest l) s)]))