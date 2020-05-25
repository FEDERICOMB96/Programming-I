;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ejercicio_2-12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; 2.
(define (cuadrado num) (* num num))

(define (dist_origen x y)
        (sqrt (+ (cuadrado x) (cuadrado y))))
        
; 3.
(define (dist_puntos x1 y1 x2 y2)
        (sqrt (+ (cuadrado (- x2 x1)) (cuadrado (- y2 y1)))))

; 4.
(define (cubo num) (expt num 3))

(define (vol-cubo arista)
        (cubo arista))

; 5.
(define (area-cuadrado arista) (cuadrado arista))

(define (area-cubo arista)
        (* (area-cuadrado arista) 6))

; 6.
(define pie 3.2808)

(define (metro-pie metros)
        (* metros pie))

; 7.
(define (cel-far t_en_celsius)
        (+ (* t_en_celsius (/ 9 5)) 32))

; 8.
(define (posible? a b c)
        (and (> (+ a b) c) (> (+ b c) a) (> (+ a c ) b)))

; 9.
(define (a x y z)
        (= (cuadrado x) (+ (cuadrado y) (cuadrado z))))
  
(define (pitagórica? x y z)
        (or (a x y z) (a y x z) (a z x y)))

; 10.
(define (suma-long a b)
        (+ (string-length a) (string-length b)))

; 11.
(define (comienzaA? a)
        (string=? "A" (substring a 0)))

; 12.
(define (inicio a i)
        (substring a 0 i))

(define (fin a i)
        (substring a i (string-length a)))

(define (poner- a i)
       (string-append (inicio a i) "-" (fin a i)))
