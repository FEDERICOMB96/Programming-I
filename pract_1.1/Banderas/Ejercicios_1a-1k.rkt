;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicios_1a-1k) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Tamaño de las banderas
(define ancho 180)
(define alto 120)

; Bandas y Marco
(define (banda-vertical color) (rectangle (/ ancho 3) alto "solid" color))
(define (banda-horizontal color) (rectangle ancho (/ alto 3) "solid" color))
(define marco (empty-scene ancho alto))

; a)
; Peru

(define peru (place-image (banda-vertical "red")
                          (/ ancho 6) (/ alto 2)
                          (place-image (banda-vertical "white")
                                       (/ ancho 2) (/ alto 2)
                                       (place-image (banda-vertical "red")
                                                    (* 5 (/ ancho 6)) (/ alto 2)
                                                    marco))))

; b)
; Italia

(define italia (place-image (banda-vertical "green")
                            (/ ancho 6) (/ alto 2)
                            (place-image (banda-vertical "white")
                                         (/ ancho 2) (/ alto 2)
                                         (place-image (banda-vertical "red")
                                                      (* 5 (/ ancho 6)) (/ alto 2)
                                                      marco))))

; c)
; Cualquier Bandera que esté formada por tres bandas verticales de igual ancho.

(define
  (bandera-3bandas-verticales color-banda1 color-banda2 color-banda3)
  (place-image (banda-vertical color-banda1)
               (/ ancho 6) (/ alto 2)
               (place-image (banda-vertical color-banda2)
                            (/ ancho 2) (/ alto 2)
                            (place-image (banda-vertical color-banda3)
                                         (* 5 (/ ancho 6)) (/ alto 2)
                                         marco))))

; d)
; Alemania

(define alemania (place-image (banda-horizontal "black")
                              (/ ancho 2) (/ alto 6)
                              (place-image (banda-horizontal "red")
                                           (/ ancho 2) (/ alto 2)
                                           (place-image (banda-horizontal "yellow")
                                                        (/ ancho 2) (* 5 (/ alto 6))
                                                        marco))))

; e)
; Holanda

(define holanda (place-image (banda-horizontal "red")
                              (/ ancho 2) (/ alto 6)
                              (place-image (banda-horizontal "white")
                                           (/ ancho 2) (/ alto 2)
                                           (place-image (banda-horizontal "blue")
                                                        (/ ancho 2) (* 5 (/ alto 6))
                                                        marco))))

; f)
; Cualquier Bandera que esté formada por tres bandas horizontales de igual alto.

(define
  (bandera-3bandas-horizontales color-banda1 color-banda2 color-banda3)
  (place-image (banda-horizontal color-banda1)
               (/ ancho 2) (/ alto 6)
               (place-image (banda-horizontal color-banda2)
                            (/ ancho 2) (/ alto 2)
                            (place-image (banda-horizontal color-banda3)
                                         (/ ancho 2) (* 5 (/ alto 6))
                                         marco))))

; g)
; Cualquier Bandera que esté formada por tres bandas del mismo alto/ancho.

(define
  (bandera-3bandas color-banda1 color-banda2 color-banda3 sentido)
  (if (string=? sentido "horizontal")
      (bandera-3bandas-horizontales color-banda1 color-banda2 color-banda3)
      (if (string=? sentido "vertical")
          (bandera-3bandas-verticales color-banda1 color-banda2 color-banda3)
          "Sentido no valido")))
; h)
; Francia
(define francia (bandera-3bandas "blue" "white" "red" "vertical"))

; i)
; Sudan
(define sudan (place-image (rotate -90 (triangle alto "solid" "green"))
                           (/ ancho 4) (/ alto 2)
                           (bandera-3bandas "red" "white" "black" "horizontal")))

; Argentina
(define argentina (place-image (circle (/ alto 8) "solid" "yellow")
                               (/ ancho 2) (/ alto 2)
                               (bandera-3bandas "blue" "white" "blue" "horizontal")))

; Camerun
(define camerun (place-image (star (/ alto 6) "solid" "yellow")
                             (/ ancho 2) (/ alto 2)
                             (bandera-3bandas "green" "red" "yellow" "vertical")))

; j)
; Brasil
(define brasil (place-image (circle (/ alto 5) "solid" "blue")
                            (/ ancho 2) (/ alto 2)
                            (place-image (rhombus (/ ancho 2) 120 "solid" "yellow")
                                         (/ ancho 2) (/ alto 2)
                                         (place-image (rectangle ancho alto "solid" "green")
                                                      (/ ancho 2) (/ alto 2)
                                                      marco))))