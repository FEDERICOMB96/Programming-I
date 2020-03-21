;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicios_1a-1k) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)

; Tamaño de las banderas
(define ANCHO 180)
(define ALTO 120)

; Bandas y Marco
(define (BANDA-VERTICAL color) (rectangle (/ ANCHO 3) ALTO "solid" color))
(define (BANDA-HORIZONTAL color) (rectangle ANCHO (/ ALTO 3) "solid" color))
(define MARCO (empty-scene ANCHO ALTO))

; a)
; Peru

(define peru (place-image (BANDA-VERTICAL "red")
                          (/ ANCHO 6) (/ ALTO 2)
                          (place-image (BANDA-VERTICAL "white")
                                       (/ ANCHO 2) (/ ALTO 2)
                                       (place-image (BANDA-VERTICAL "red")
                                                    (* 5 (/ ANCHO 6)) (/ ALTO 2)
                                                    MARCO))))

; b)
; Italia

(define italia (place-image (BANDA-VERTICAL "green")
                            (/ ANCHO 6) (/ ALTO 2)
                            (place-image (BANDA-VERTICAL "white")
                                         (/ ANCHO 2) (/ ALTO 2)
                                         (place-image (BANDA-VERTICAL "red")
                                                      (* 5 (/ ANCHO 6)) (/ ALTO 2)
                                                      MARCO))))

; c)
; Cualquier Bandera que esté formada por tres bandas verticales de igual ANCHO.

(define
  (bandera-3bandas-verticales color-banda1 color-banda2 color-banda3)
  (place-image (BANDA-VERTICAL color-banda1)
               (/ ANCHO 6) (/ ALTO 2)
               (place-image (BANDA-VERTICAL color-banda2)
                            (/ ANCHO 2) (/ ALTO 2)
                            (place-image (BANDA-VERTICAL color-banda3)
                                         (* 5 (/ ANCHO 6)) (/ ALTO 2)
                                         MARCO))))

; d)
; Alemania

(define alemania (place-image (BANDA-HORIZONTAL "black")
                              (/ ANCHO 2) (/ ALTO 6)
                              (place-image (BANDA-HORIZONTAL "red")
                                           (/ ANCHO 2) (/ ALTO 2)
                                           (place-image (BANDA-HORIZONTAL "yellow")
                                                        (/ ANCHO 2) (* 5 (/ ALTO 6))
                                                        MARCO))))

; e)
; Holanda

(define holanda (place-image (BANDA-HORIZONTAL "red")
                             (/ ANCHO 2) (/ ALTO 6)
                             (place-image (BANDA-HORIZONTAL "white")
                                          (/ ANCHO 2) (/ ALTO 2)
                                          (place-image (BANDA-HORIZONTAL "blue")
                                                       (/ ANCHO 2) (* 5 (/ ALTO 6))
                                                       MARCO))))

; f)
; Cualquier Bandera que esté formada por tres bandas horizontales de igual ALTO.

(define
  (bandera-3bandas-horizontales color-banda1 color-banda2 color-banda3)
  (place-image (BANDA-HORIZONTAL color-banda1)
               (/ ANCHO 2) (/ ALTO 6)
               (place-image (BANDA-HORIZONTAL color-banda2)
                            (/ ANCHO 2) (/ ALTO 2)
                            (place-image (BANDA-HORIZONTAL color-banda3)
                                         (/ ANCHO 2) (* 5 (/ ALTO 6))
                                         MARCO))))

; g)
; Cualquier Bandera que esté formada por tres bandas del mismo ALTO/ANCHO.

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
(define sudan (place-image (rotate -90 (triangle ALTO "solid" "green"))
                           (/ ANCHO 4) (/ ALTO 2)
                           (bandera-3bandas "red" "white" "black" "horizontal")))

; Argentina
(define argentina (place-image (circle (/ ALTO 8) "solid" "yellow")
                               (/ ANCHO 2) (/ ALTO 2)
                               (bandera-3bandas "blue" "white" "blue" "horizontal")))

; Camerun
(define camerun (place-image (star (/ ALTO 6) "solid" "yellow")
                             (/ ANCHO 2) (/ ALTO 2)
                             (bandera-3bandas "green" "red" "yellow" "vertical")))

; j)
; Brasil
(define brasil (place-image (circle (/ ALTO 5) "solid" "blue")
                            (/ ANCHO 2) (/ ALTO 2)
                            (place-image (rhombus (/ ANCHO 2) 120 "solid" "yellow")
                                         (/ ANCHO 2) (/ ALTO 2)
                                         (place-image (rectangle ANCHO ALTO "solid" "green")
                                                      (/ ANCHO 2) (/ ALTO 2)
                                                      MARCO))))