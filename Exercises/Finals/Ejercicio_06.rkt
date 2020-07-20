;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 6

(define-struct pedido [nombre menu postre])
;; pedido es (make-pedido String Number Number)
;; Interpretación: El primer campo es un string que represente el nombre del cliente,
;; el segundo campo es un número de 1 a 3 que represente la opción de menú elegida,
;; y el tercer campo es un número entre 1 y 2 que represente la opción de postre elegida.

(define-struct pedido-final [cantmenus1 cantmenus2 cantmenus3 cantpostres1 cantpostres2])
;; pedido-final es (make-pedido-final Number Number Number Number Number)
;; Interpretación: Los primeros tres campos se refieren a la cantidad de pedidos de cada uno de los
;; tres posibles menus, mientras que los ultimos dos campos se refieren a la cantidad de pedidos
;; de los 2 posibles postres.

;; Ejemplos
(define PEDIDO1 (make-pedido "Bob" 1 2))
(define PEDIDO2 (make-pedido "Linda" 1 2))
(define PEDIDO3 (make-pedido "Tina" 2 1))
(define PEDIDO4 (make-pedido "Gene" 2 1))
(define PEDIDO5 (make-pedido "Louis" 3 1))

(define PEDIDO-FINAL (make-pedido-final 2 2 1 3 2))
(define LISTA-PEDIDOS (list PEDIDO1 PEDIDO2 PEDIDO3 PEDIDO4 PEDIDO5))

;; agregar-menu : Number pedido-final -> pedido-final
;; Dado un menu, lo agrega al pedido final.

(check-expect (agregar-menu 1 (make-pedido-final 1 2 1 3 1)) (make-pedido-final 2 2 1 3 1))
(check-expect (agregar-menu 2 (make-pedido-final 1 0 1 0 2)) (make-pedido-final 1 1 1 0 2))
(check-expect (agregar-menu 3 (make-pedido-final 1 2 3 2 3)) (make-pedido-final 1 2 4 2 3))

(define (agregar-menu n pf)
  (cond [(= n 1) (make-pedido-final (+ (pedido-final-cantmenus1 pf) 1)
                                    (pedido-final-cantmenus2 pf)
                                    (pedido-final-cantmenus3 pf)
                                    (pedido-final-cantpostres1 pf)
                                    (pedido-final-cantpostres2 pf))]
        [(= n 2) (make-pedido-final (pedido-final-cantmenus1 pf)                                    
                                    (+ (pedido-final-cantmenus2 pf) 1)
                                    (pedido-final-cantmenus3 pf)
                                    (pedido-final-cantpostres1 pf)
                                    (pedido-final-cantpostres2 pf))]
        [else (make-pedido-final (pedido-final-cantmenus1 pf)
                                 (pedido-final-cantmenus2 pf)
                                 (+ (pedido-final-cantmenus3 pf) 1)
                                 (pedido-final-cantpostres1 pf)
                                 (pedido-final-cantpostres2 pf))]))

;; agregar-postre : Number pedido-final -> pedido-final
;; Dado un postre, lo agrega al pedido final.
;; OBSERVACION: Se considero el caso que el cliente no pida postre.

(check-expect (agregar-postre 0 (make-pedido-final 1 2 1 2 1)) (make-pedido-final 1 2 1 2 1))
(check-expect (agregar-postre 1 (make-pedido-final 1 2 1 2 1)) (make-pedido-final 1 2 1 3 1))
(check-expect (agregar-postre 2 (make-pedido-final 1 2 1 2 1)) (make-pedido-final 1 2 1 2 2))

(define (agregar-postre n pf)
  (cond [(zero? n) pf]
        [(= n 1) (make-pedido-final (pedido-final-cantmenus1 pf)                                    
                                    (pedido-final-cantmenus2 pf)
                                    (pedido-final-cantmenus3 pf)
                                    (+ (pedido-final-cantpostres1 pf) 1)
                                    (pedido-final-cantpostres2 pf))]        
        [else (make-pedido-final (pedido-final-cantmenus1 pf)                                    
                                 (pedido-final-cantmenus2 pf)
                                 (pedido-final-cantmenus3 pf)
                                 (pedido-final-cantpostres1 pf)
                                 (+ (pedido-final-cantpostres2 pf) 1))]))

;; agregar-pedido : pedido -> pedido-final
;; Dado un pedido, lo agrega al pedido final.

(check-expect (agregar-pedido PEDIDO1 (make-pedido-final 0 0 0 0 0)) (make-pedido-final 1 0 0 0 1))
(check-expect (agregar-pedido PEDIDO3 (make-pedido-final 2 0 0 0 2)) (make-pedido-final 2 1 0 1 2))

(define (agregar-pedido p pf)
  (agregar-postre (pedido-postre p) (agregar-menu (pedido-menu p) pf)))

;; armar-pedido-final : List(pedido) pedido-final -> pedido-final
;; Dada una lista de pedidos individuales l y un pedido final pf, construya un pedido-final.

(check-expect (armar-pedido-final-aux empty (make-pedido-final 0 0 0 0 0))
              (make-pedido-final 0 0 0 0 0))

(check-expect (armar-pedido-final-aux LISTA-PEDIDOS (make-pedido-final 0 0 0 0 0))
              PEDIDO-FINAL)

(define (armar-pedido-final-aux l pf)
  (cond [(empty? l) pf]
        [else (agregar-pedido (first l) (armar-pedido-final-aux (rest l) pf))]))

;; armar-pedido-final : List(pedido) -> pedido-final
;; Dada una lista de pedidos individuales pedido construya un pedido-final.

(check-expect (armar-pedido-final empty) (make-pedido-final 0 0 0 0 0))
(check-expect (armar-pedido-final LISTA-PEDIDOS) PEDIDO-FINAL)

(define (armar-pedido-final l)
  (armar-pedido-final-aux l (make-pedido-final 0 0 0 0 0)))