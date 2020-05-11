;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; --------------------------------------
; Ejercicio 3

; simétrico: (Number , Number) -> (Number , Number)
; Dada una posición, nos devuelva su simétrica respecto del origen

(check-expect (simétrico (make-posn 1 1)) (make-posn -1 -1))

(define (simétrico p)
  (make-posn (- (posn-x p)) (- (posn-y p))))