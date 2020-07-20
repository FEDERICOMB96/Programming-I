;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 5

;; replicar : List(X) Natural -> List(X)
;; Toma una lista y un número entero n y replica cada elemento de la lista n veces.

(check-expect (replicar empty 3) empty)
(check-expect (replicar (list 4 6 3 9) 0) empty)
(check-expect (replicar (list 4 6 3 9) 3) (list 4 4 4 6 6 6 3 3 3 9 9 9))

(define (replicar l n)
  (cond [(empty? l) empty]
        [(zero? n) (replicar (rest l) n)]
        [else (append (make-list n (first l)) (replicar (rest l) n))]))

#| Evaluacion

(replicar (list 4 6 3 9) 3)
==
(append (make-list 3 4) (replicar (list 6 3 9) 3))
==
(append (list 4 4 4) (replicar (list 6 3 9) 3))


(replicar (list 6 3 9) 3)
==
(append (make-list 3 6) (replicar (list 3 9) 3))
==
(append (list 6 6 6) (replicar (list 3 9) 3))


(replicar (list 3 9) 3)
==
(append (make-list 3 3) (replicar (list 9) 3))
==
(append (list 3 3 3) (replicar (list 9) 3))


(replicar (list 9) 3)
==
(append (make-list 3 9) (replicar empty 3))
==
(append (list 9 9 9) (replicar empty 3))


Luego,
(append (list 4 4 4) (append (list 6 6 6) (append (list 3 3 3) (append (list 9 9 9) empty))))
(append (list 4 4 4) (append (list 6 6 6) (append (list 3 3 3) (list 9 9 9))))
(append (list 4 4 4) (append (list 6 6 6) (list 3 3 3 9 9 9)))
(append (list 4 4 4) (list 6 6 6 3 3 3 9 9 9))
(list 4 4 4 6 6 6 3 3 3 9 9 9)

|#