;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicio_03) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 3

;; intervalo : N N -> List(N)
;; Dados dos números naturales, devuelve una lista con todos los números entre a y b.

(check-expect (intervalo 2 4) (list 2 3 4))
(check-expect (intervalo 4 4) (list 4))
(check-expect (intervalo 5 4) empty)

(define (intervalo n m)
  (cond [(> n m) empty]
        [else (cons n (intervalo (add1 n) m))]))

;; elimimar-multiplos: N List(N) -> List(N)
;; Dados un natural positivo n y una lista l, elimina todos los múltiplos de n en l.

(check-expect (eliminar-multiplos 2 (list 2 3 4 5 6)) (list 3 5))
(check-expect (eliminar-multiplos 3 (list 4 6 11 15)) (list 4 11))
(check-expect (eliminar-multiplos 1 (intervalo 1 1000)) empty)
(check-expect (eliminar-multiplos 4 empty) empty)

(define (eliminar-multiplos n l)
  (cond [(empty? l) empty]
        [(= n 1) empty]
        [else (if (integer? (/ (first l) n))                 
                  (eliminar-multiplos n (rest l))
                  (cons (first l) (eliminar-multiplos n (rest l))))]))

;; eratostenes : List(N) -> List(N)
;; Dada una lista que inicialmente es de la forma [2, .., n] para algún n,
;; realiza el procedimiento de eratóstenes.

(check-expect (eratostenes (intervalo 2 10)) (list 2 3 5 7))
(check-expect (eratostenes empty) empty)
(check-expect (eratostenes (intervalo 2 17)) (list 2 3 5 7 11 13 17))

(define (eratostenes l)
  (cond [(empty? l) empty]
        [else (cons (first l) (eratostenes (eliminar-multiplos (first l) (rest l))))]))

;; criba-eratostenes : N -> List(N)
;; Dado un natural n >= 2, devuelve la lista de todos los números primos hasta n.

(check-expect (criba-eratostenes 10) (list 2 3 5 7))
(check-expect (criba-eratostenes 2) (list 2))
(check-expect (criba-eratostenes 20) (list 2 3 5 7 11 13 17 19))

(define (criba-eratostenes n)
  (eratostenes (intervalo 2 n)))

;; agregar-\n : String -> String
;; Dado un string s, devuelve otro string que surge de la concatenacion de s y "\n".

(check-expect (agregar-\n "2") "2\n")
(check-expect (agregar-\n "") "\n")

(define (agregar-\n s)
  (string-append s "\n"))

(write-file "primos.txt"
            (foldr string-append "" (map agregar-\n (map number->string (criba-eratostenes 10000)))))