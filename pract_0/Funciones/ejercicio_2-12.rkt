;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ejercicio_2-12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;2
(define (dist_origen x y)
        (sqrt (+ (expt x 2) (expt y 2))))
        
;3
(define (dist_puntos x1 y1 x2 y2)
        (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))

;4
(define (vol-cubo arista)
        (expt arista 3))

;5
(define (area-cubo arista)
        (* (expt arista 2) 6))

;6
(define pie 3.2808)

(define (metro-pie metros)
        (* metros pie))

;7
(define (cel-far t_en_celsius)
        (+ (* t_en_celsius (/ 9 5)) 32))

;8
(define (posible? a b c)
        (and (and (> (+ a b) c)
                  (> (+ b c) a))
             (> (+ a c ) b)))

;9
(define (a x y z)
                (= (expt x 2) (+ (expt y 2) (expt z 2))))
  
(define (pitagórica? x y z)
        (or (or (a x y z) (a y x z)) (a z x y)))

;10
(define (suma-long a b)
        (+ (string-length a) (string-length b)))

;11
(define (comienzaA? a)
        (equal? (substring a 0 1) "A"))

;12
(define (inicio a i)
               (substring a 0 i))

(define (fin a i)
               (substring a (+ i 1) (string-length a)))

(define (poner- a i)
       (string-append (string-append (inicio a i) "-") (fin a i)))
