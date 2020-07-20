;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_16) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 16

(define-struct libro (isbn nombre autor puntaje))
;; libro es (make-struct String String String Number)
;; Interpretacion: el primer campo es el isbn (identificador), el segundo es el nombre,
;; el tercero el autor y el cuarto el puntaje (número natural entre 1 y 10).

(define USUARIO1 (list (make-libro "9781411469532" "Fahrenheit" "Ray Bradbury" 10)
                       (make-libro "9781443434973" "1984" "George Orwell" 10)))

(define USUARIO2 (list (make-libro "9780307387899" "The Road" "Cormac McCarthy" 8)
                       (make-libro "9781443434973" "1984" "George Orwell" 9)))

(define USUARIO3 (list (make-libro "9781411469532" "Fahrenheit" "Ray Bradbury" 8)
                       (make-libro "9780060850524" "Brave New World" "Aldous Huxley" 9)))

(define USUARIO4 (list (make-libro "9781411469532" "Fahrenheit" "Ray Bradbury" 9)
                       (make-libro "9781443434973" "1984" "George Orwell" 9)))

;; coincide? : libro libro -> Boolean
;; Dadas dos estructuras libro decida si las lecturas fueron coincidentes.
;; Considerar el caso donde los dos libros argumento son distintos.

(check-expect (coincide? (first USUARIO1) (first USUARIO3)) #f)
(check-expect (coincide? (first USUARIO1) (first USUARIO2)) #f)
(check-expect (coincide? (second USUARIO1) (second USUARIO2)) #t)
(check-expect (coincide? (second USUARIO2) (second USUARIO1)) #t) 

(define (coincide? l1 l2)
  (cond [(not (string=? (libro-isbn l1) (libro-isbn l2))) #f]
        [(< (abs (- (libro-puntaje l1) (libro-puntaje l2))) 2) #t]
        [else #f]))

;; similares?-aux1 : libro List(libro) -> Boolean
;; Dados un libro l (que corresponde a la lectura de un usuario a) y un usuario b, verifica si
;; la lectura del libro l realizada por el usuario a coincide con la lectura de l del usuario b.

(check-expect (similares?-aux1 (first USUARIO1) USUARIO2) #f)
(check-expect (similares?-aux1 (first USUARIO1) USUARIO3) #f)
(check-expect (similares?-aux1 (first USUARIO1) USUARIO4) #t)

(define (similares?-aux1 l u)
  (cond [(empty? u) #f]
        [else (or (coincide? l (first u)) (similares?-aux1 l (rest u)))]))

;; similares?-aux2 : List(libro) List(libro) -> Boolean
;; Dados dos usuarios a y b (listas de libros), verifica si cada lectura de a coincide con la
;; lectura de b.

(check-expect (similares?-aux2 USUARIO1 USUARIO2) (list #f #t))
(check-expect (similares?-aux2 USUARIO1 USUARIO3) (list #f #f))
(check-expect (similares?-aux2 USUARIO1 USUARIO4) (list #t #t))

(define (similares?-aux2 u1 u2)
  (cond [(empty? u1) empty]
        [else (cons (similares?-aux1 (first u1) u2) (similares?-aux2 (rest u1) u2))]))

;; similares? : List(libro) List(libro) -> Boolean
;; Dados dos usuarios a y b (listas de libros) decida si b es similar a a y por lo tanto puede
;; recomendarle lecturas. Un usuario a puede recomendar lecturas a otro usuario b si más del 10%
;; de las lecturas de b son coincidentes con las de a.

(check-expect (similares? USUARIO1 USUARIO3) #f)
(check-expect (similares? USUARIO1 USUARIO4) #t)

(define (similares? a b)
  (> (/ (length (filter (lambda (x) x) (similares?-aux2 a b))) (length a)) 0.1))