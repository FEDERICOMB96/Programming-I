;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ejercicio_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;a
(not #t)

;b.
(or #t #f)

;c.
(and #t #f)

;d.
(and #t (or #f (not #f)) (not #t))

;e.
(not (= 2 (* 1 3)))

;f.
(or (= 2 (* 1 3)) (< 4 (+ 3 2)))