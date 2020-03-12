;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ejercicio_1) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;a
(circle 10 "solid" "red")

;b
(rectangle 10 20 "solid" "blue")

;c
(rectangle 20 12 "outline" "magenta")

;d
(overlay (rectangle 20 20 "solid" "blue") (circle 7 "solid" "green"))

;e
(empty-scene 100 100)

;f
(place-image (circle 10 "solid" "blue") 40 80 (empty-scene 100 100))

;g
(+ (image-width (circle 10 "solid" "red")) (image-height (rectangle 10 20 "solid" "blue")))
