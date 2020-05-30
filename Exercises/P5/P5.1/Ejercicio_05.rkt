;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 5

;; List-of-strings es:
;; - empty
;; - (cons string List-of-strings)
;; Interpretación: una lista cuyos elementos son strings.

;; contiene? : String List-of-strings -> Boolean
;; Dado un string y una lista de strings, devuelve si la lista contiene el string
;; que recibe como entrada.

(check-expect (contiene? "A" empty) #false)
(check-expect (contiene? "Stan" (list "Stan" "Kyle" "Eric" "Kenny")) #true)
(check-expect (contiene? "Tina" (list "Bob" "Linda" "Gene" "Louis")) #false)

(define (contiene? str l)
  (cond [(empty? l) #false]
        [else (if (string=? (first l) str)
                  #true
                  (contiene? str (rest l)))]))