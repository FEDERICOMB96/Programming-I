;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------------------------------------
;; Ejercicio 7

;; sublistas-vacias? : List(List(X)) -> Bool
;; Dada una lista compuesta por listas devuelva #true si todas las sublistas están vacias
;; o #false en caso contrario.

(check-expect (sublistas-vacias? empty) #true)
(check-expect (sublistas-vacias? (list '() '() '())) #true)
(check-expect (sublistas-vacias? (list '() (list 5 7))) #false)

(define (sublistas-vacias? l)
  (cond [(empty? l) #true]
        [(empty? (first l)) (sublistas-vacias? (rest l))]
        [else #false]))