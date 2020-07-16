;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #t #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 5

;; Representaremos alfabetos como Strings.
;; Por ejemplo, si nuestros símbolos son las cinco primeras letras, los dígitos y el espacio,
;; lo representaremos como "ABCDE0123456789 "

;; Representaremos símbolos como strings de longitud 1. En el alfabeto anterior,
;; el símbolo E lo representamos con el string "E"

;; El código del césar lo representaremos mediante parejas de símbolos.
;; Por ejemplo, si queremos decir que el símbolo "A" se codifica con el
;; símbolo "C", tendremos (make-posn "A" "C") para representar esta situación.

;;;;;;;; Primero comenzamos definiendo algunas funciones
;; sobre strings y listas que nos son de utilidad.

;; partir : String -> List(String)
;; Dado un string, devuele una lista de strings con cada símbolo separado

(check-expect (partir "ABC") (list "A" "B" "C"))
(check-expect (partir "12345") (list "1" "2" "3" "4" "5"))

(define (partir s)
  (explode s))

#| Alternativamente, sin utilizar explode:

(define (partir s)
  (cond [(zero? (string-length s)) empty]
        [else (cons (string-ith s 0) (partir (substring s 1)))]))

|#

;; tomar : List N -> List
;; Dada una lista y un número natural n, devuelve una lista
;; con los primeros n elementos de l. Si l no tiene tantos elementos,
;; devuelve l.

(check-expect (tomar (list 1 2 3 4 5) 4) (list 1 2 3 4))
(check-expect (tomar (list 1 2 3 4 5) 10) (list 1 2 3 4 5))
(check-expect (tomar (list 1 2 3 4 5) 0) empty)

(define (tomar l n)
  (cond [(empty? l) l]
        [(zero? n) empty]
        [else (cons (first l) (tomar (rest l) (sub1 n)))]))

;; tirar : List N -> List
;; Dada una lista y un número natural n, devuelve una lista
;; sin los primeros n elementos de l. Si l no tiene tantos elementos,
;; devuelve empty.

;; OBSERVACION: para cualquier n <= length l, (append (tomar n l) (tirar n l)) = l

(check-expect (tirar (list 1 2 3 4 5) 2) (list 3 4 5))
(check-expect (tirar (list 1 2 3 4 5) 10) empty)
(check-expect (tirar (list 1 2 3 4 5) 0) (list 1 2 3 4 5))

(define (tirar l n)
  (cond [(empty? l) empty]
        [(zero? n) l]
        [else (tirar (rest l) (sub1 n))]))

;; emparejar : List List -> List
;; Dadas dos listas [a0,..., an] y [b0, ...., bn] de la misma longitud, devuelve una lista
;; de posn con parejas tomadas de ambas listas: [(make-posn a0 b0), ...., (make-posn an bn)]

(check-expect (emparejar (list 1 2) (list "a" "b")) (list (make-posn 1 "a") (make-posn 2 "b")))
(check-expect (emparejar (list #t #f) (list 1 0)) (list (make-posn #t 1) (make-posn #f 0)))

(define (emparejar a b)
  (cond [(zero? (length a)) empty] ; Esto se puede hacer porque a y b tienen la misma longitud
        [else (cons (make-posn (first a) (first b)) (emparejar (rest a) (rest b)))]))


;;;;;;;;;;;;; Ahora comienzan las funciones específicas para el método del César
;; cifrado : N String -> List(posn)
;; Dada una clave de desplazamiento y un alfabeto s, devuelve una lista
;; con parejas de strings, donde el primer elemento es el caracter a cifrar, y el segundo
;; su código del César de acuerdo a la clave. Se asume que 0 < n < (string-length s)

(check-expect (cifrado 2 "ABC") (list (make-posn "A" "C") (make-posn "B" "A") (make-posn "C" "B"))) 
(check-expect (cifrado 1 "ABC") (list (make-posn "A" "B") (make-posn "B" "C") (make-posn "C" "A")))

(define (cifrado n s)
  (emparejar (partir s)
             (append (tirar (partir s) n) (tomar (partir s) n))))

;; encriptar-simbolo : String List(posn) -> String
;; Dado un string s de longitud 1 que es un símbolo del
;; alfabeto y una lista de parejas que representa un código del césar,
;; devuelve el código que le corresponde a s.

(check-expect (encriptar-simbolo "A" (cifrado 2 "ABC")) "C")
(check-expect (encriptar-simbolo "B" (cifrado 1 "ABC")) "C")

(define (encriptar-simbolo s l)
  (cond [(string=? (posn-x (first l)) s) (posn-y (first l))]
        [else (encriptar-simbolo s (rest l))]))

;; encriptar-mensaje : String String Natural -> String
;; dado un string, un alfabeto y una clave, devuelve el string encriptado

(check-expect (encriptar-mensaje "ABC" "ABCDEF" 3) "DEF")
(check-expect (encriptar-mensaje "ABC" "ABCDEF" 4) "EFA")

(define (encriptar-mensaje s a n)
  (cond [(zero? (string-length s)) ""]
        [else (string-append (encriptar-simbolo (string-ith s 0) (cifrado n a))
                             (encriptar-mensaje (substring s 1) a n))]))

;; desencriptar-simbolo : String List(posn) -> String
;; Dado un string s de longitud 1 que es un símbolo del
;; alfabeto y una lista de parejas que representa un código del césar,
;; devuelve el caracter desencriptado que le corresponde a s

(check-expect (desencriptar-simbolo "A" (cifrado 2 "ABC")) "B")
(check-expect (desencriptar-simbolo "A" (cifrado 1 "ABC")) "C")

(define (desencriptar-simbolo s l)
  (cond [(string=? (posn-y (first l)) s) (posn-x (first l))]
        [else (desencriptar-simbolo s (rest l))]))

;; desencriptar-mensaje : String String N -> String
;; dado un string, un alfabeto y una clave, devuelve el string desencriptado.

(check-expect (desencriptar-mensaje "DEF" "ABCDEF" 3) "ABC")
(check-expect (desencriptar-mensaje "EFA" "ABCDEF" 4) "ABC")

(define (desencriptar-mensaje s a n)
  (cond [(zero? (string-length s)) ""]
        [else (string-append (desencriptar-simbolo (string-ith s 0) (cifrado n a))
                             (desencriptar-mensaje (substring s 1) a n))]))

;; -----------------------------------------------
;; COMPROBACION: 
(define ALFABETO "ABCDEFGHIJKLMNÑOPQRSTUVWXYZ 0123456789")
(define CLAVE 3)
(define CODIGO-DEL-CESAR (cifrado CLAVE ALFABETO))

(check-expect (encriptar-mensaje "HOLA" ALFABETO CLAVE) "KRÑD")
(check-expect (encriptar-mensaje "ATACAR A LAS 18" ALFABETO CLAVE) "DWDFDU2D2ÑDV24B")
(check-expect (encriptar-mensaje "LA OPERACION ES REVERSIBLE" ALFABETO CLAVE)
              "ÑD2RSHUDFLRP2HV2UHYHUVLEÑH")
(check-expect
 (desencriptar-mensaje (encriptar-mensaje "LA OPERACION ES REVERSIBLE" ALFABETO CLAVE) ALFABETO CLAVE)
 "LA OPERACION ES REVERSIBLE")