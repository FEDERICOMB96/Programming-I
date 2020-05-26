;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_09) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; --------------------------------------
;; Ejercicio 9

(define-struct Auto [modelo años tipo-combustible rendimiento-optimo])
;; Auto es (String, Number, String, Number)
;; Intepretación: El primer elemento es el modelo del auto, el segundo la cantidad de
;; años de antiguedad del mismo, el tercero es el tipo de combustible que acepta
;; (nafta o diesel) y el cuarto el rendimiento óptimo, expresado en km/l.

(define DISMINUCION-RENDIMIENTO-1-A-5 0.02)     ; Si el auto tiene entre 1 a 5 años
(define DISMINUCION-RENDIMIENTO-6-A-10 0.06)    ; Si el auto tiene entre 6 a 10 años
(define DISMINUCION-RENDIMIENTO-11-A-15 0.10)   ; Si el auto tiene entre 11 a 15 años
(define DISMINUCION-RENDIMIENTO-MAS-DE-15 0.15) ; Si el auto tiene más de 15 años

(define PRECIO-LITRO-NAFTA 19)   ; Precio del litro de nafta
(define PRECIO-LITRO-DIESEL 17)  ; Precio del litro de diesel
(define COSTO-PEAJE 50)          ; Por cada 100 kilómetros recorridos

;; --------------------------------------
;; costo-viaje: Auto Number -> Number
;; toma como entrada un valor de tipo Auto y un número de kilómetros a recorrer y
;; calcule el costo del viaje, teniendo en cuenta la cantidad y precio del combustible
;; y los peajes.

(define (costo-viaje auto m)
  (redondear-2-decimales
   (+ (* (/ m (rendimiento auto)) (precio-por-litro (Auto-tipo-combustible auto)))
      (gasto-peajes m))))

;; --------------------------------------
;; redondear-2-decimales: Number -> Number
;; dado un valor lo redondea a dos decimales.

(check-expect (redondear-2-decimales 10.653153) 10.65)

(define (redondear-2-decimales cantidad)
  (/ (floor (* cantidad 100)) 100))

;; --------------------------------------
;; rendimiento: Auto -> Number
;; dada la antiguedad del auto (en años), devuelve el rendimiento del mismo.

(check-expect (rendimiento (make-Auto "gol" 0 "nafta" 13))
              (* 13 1))

(check-expect (rendimiento (make-Auto "gol" 1 "nafta" 13))
              (* 13 (- 1 DISMINUCION-RENDIMIENTO-1-A-5)))

(check-expect (rendimiento (make-Auto "gol" 7 "nafta" 13))
              (* 13 (- 1 DISMINUCION-RENDIMIENTO-6-A-10)))

(check-expect (rendimiento (make-Auto "gol" 12 "nafta" 13))
              (* 13 (- 1 DISMINUCION-RENDIMIENTO-11-A-15)))

(check-expect (rendimiento (make-Auto "gol" 18 "nafta" 13))
              (* 13 (- 1 DISMINUCION-RENDIMIENTO-MAS-DE-15)))

(define (rendimiento auto)
  (* (Auto-rendimiento-optimo auto)
     (cond [(< (Auto-años auto) 1) 1]
           [(and (>= (Auto-años auto) 1) (<= (Auto-años auto) 5))
            (- 1 DISMINUCION-RENDIMIENTO-1-A-5)]
           [(and (>= (Auto-años auto) 6) (<= (Auto-años auto) 10))
            (- 1 DISMINUCION-RENDIMIENTO-6-A-10)]
           [(and (>= (Auto-años auto) 11) (<= (Auto-años auto) 15))
            (- 1 DISMINUCION-RENDIMIENTO-11-A-15)]
           [(> (Auto-años auto) 15) (- 1 DISMINUCION-RENDIMIENTO-MAS-DE-15)])))

;; --------------------------------------
;; gasto-peajes: Number -> Number
;; devuelve el gasto en peajes segun la cantidad de kilometros a recorrer
;; se debe pagar peaje cada 100km recorridos.

(check-expect (gasto-peajes 450) (* 4 COSTO-PEAJE))

(define (gasto-peajes m)
  (* (floor (/ m 100)) COSTO-PEAJE))

;; --------------------------------------
;; precio-por-litro: Number -> Number
;; devuelve el precio por litro segun el tipo de combustible (nafta o diesel).

(check-expect (precio-por-litro "nafta") PRECIO-LITRO-NAFTA)
(check-expect (precio-por-litro "diesel") PRECIO-LITRO-DIESEL)

(define (precio-por-litro tipo)
  (if (string=? tipo "nafta") PRECIO-LITRO-NAFTA PRECIO-LITRO-DIESEL))