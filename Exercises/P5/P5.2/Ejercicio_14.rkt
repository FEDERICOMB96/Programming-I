;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname Ejercicio_14) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; -------------------------------------
;; Ejercicio 14

(define-struct alumno [nombre nota faltas])
;; alumno  (String, Number, Natural). Interpretación
;; - nombre representa el nombre del alumno.
;; - nota representa la calificación obtenida por el alumno (entre 0 y 10).
;; - faltas: número de clases a las el alumno no asistió.

;; -----------------
;; nota-mayor-o-igual-9? : Alumno -> Bool
;; Dado un alumno, devuelve si su nota es mayor o igual a 9.

(check-expect (nota-mayor-o-igual-9? (make-alumno "Bill Gates" 8.5 10)) #f)
(check-expect (nota-mayor-o-igual-9? (make-alumno "Alan Turing" 10 12)) #t)

(define (nota-mayor-o-igual-9? a)
  (>= (alumno-nota a) 9))

;; destacados : List(alumno) -> List(String)
;; Dada una lista de alumnos, devuelve una lista con el nombre de aquellos alumnos
;; que sacaron una nota mayor o igual a 9.

(check-expect (destacados (list (make-alumno "Ada Lovelace" 10 20)
                                (make-alumno "Carlos Software" 3.5 12)))
              (list "Ada Lovelace"))

(define (destacados l)
  (map alumno-nombre (filter nota-mayor-o-igual-9? l)))

;; -----------------
;; condicion : Alumno -> String

;; Dado un alumno, determina su condición de acuerdo a las siguientes reglas:
;; - Si la nota es mayor o igual a 8, su condición es "promovido".
;; - Si la nota es menor a 6, su condición es "libre".
;; En cualquier otro caso, la condición es "regular".

(check-expect (condicion (make-alumno "Richard Stallman" 10 5)) "promovido")
(check-expect (condicion (make-alumno "Pepe Developer" 4 15)) "libre")
(check-expect (condicion (make-alumno "Jim Halpert" 7.5 5)) "regular")

(define (condicion a)
  (cond [(>= (alumno-nota a) 8) "promovido"]
        [(< (alumno-nota a) 6) "libre"]
        [else "regular"]))

;; -----------------
;; libre? : String -> Bool
;; Dado un string, devuelve si es igual a "libre".

(define (libre? s)
  (string=? s "libre"))

;; exito : List(alumno) -> Bool
;; Dada una lista de alumnos, devuelve #true si ninguno está libre.
;; Caso contrario, devuelve #false.

(check-expect (exito empty) #t)
(check-expect (exito (list (make-alumno "Juan Computación" 5 13)
                           (make-alumno "Carlos Software" 3.5 12)
                           (make-alumno "Ada Lovelace" 10 20))) #f)

(define (exito l)
  (not (ormap libre? (map condicion l))))

;; -----------------
;; regulares : List(alumno) -> Bool
;; Dado una lista de alumnos, devuelve una lista formada solamente por aquellos alumnos
;; que son regulares.

(check-expect (regulares (list (make-alumno "Juan Computación" 7 2)
                               (make-alumno "Carlos Software" 3.5 4)
                               (make-alumno "Ada Lovelace" 10 1)))
              (list (make-alumno "Juan Computación" 7 2)))

(define (regulares l)
  (cond [(empty? l) empty]
        [else (if (string=? (condicion (first l)) "regular")
                  (cons (first l) (regulares (rest l)))
                  (regulares (rest l)))]))

;; faltas-regulares : List(alumno) -> Number
;; Dada una lista de alumnos, devuelve la suma de las ausencias de los alumnos regulares.

(check-expect (faltas-regulares (list (make-alumno "Juan Computación" 7 2)
                                      (make-alumno "Carlos Software" 3.5 4)
                                      (make-alumno "Ada Lovelace" 10 1))) 2)

(define (faltas-regulares l)
  (cond [(empty? l) 0]
        [else (foldr + 0 (map alumno-faltas (regulares l)))]))

#| Usando funciones lambda. Por qué? No hay porque -.-

(define (faltas-regulares l)
  (cond [(empty? l) 0]
        [else (foldr + 0 
                     (map alumno-faltas
                          (filter (lambda (alumno) (string=? (condicion alumno) "regular")) l)))]))
|#
;; -----------------
;; promovidos : List(alumno) -> Bool
;; Dado una lista de alumnos, devuelve una lista formada solamente por aquellos alumnos
;; que son promovidos.

(check-expect (promovidos (list (make-alumno "Juan Computación" 7 2)
                                (make-alumno "Carlos Software" 3.5 4)
                                (make-alumno "Ada Lovelace" 10 1)))
              (list (make-alumno "Ada Lovelace" 10 1)))

(define (promovidos l)
  (cond [(empty? l) empty]
        [else (if (string=? (condicion (first l)) "promovido")
                  (cons (first l) (promovidos (rest l)))
                  (promovidos (rest l)))]))

;; 3+faltas? : alumno -> Bool
;; Dado un alumno, devuelve si tiene 3 o mas faltas.

(check-expect (3+faltas? (make-alumno "Juan Computación" 7 2)) #f)
(check-expect (3+faltas? (make-alumno "Carlos Software" 3.5 4)) #t)

(define (3+faltas? a)
  (>= (alumno-faltas a) 3))

;; promovidos-ausentes : List(alumno) -> List(String)
;; Dada una lista de alumnos, devuelve una lista con el nombre de aquellos alumnos
;; promovidos que no asistieron a tres o más clases.

(check-expect (promovidos-ausentes (list (make-alumno "Juan Computación" 9 3)
                                         (make-alumno "Carlos Software" 3.5 2)
                                         (make-alumno "Ada Lovelace" 10 1)))
              (list "Juan Computación"))

#;(define (promovidos-ausentes l)
  (cond [(empty? l) empty]
        [else (map alumno-nombre (filter 3+faltas? (promovidos l)))]))

#| Otra Solucion Posible:

(define (promovidos-ausentes l)
  (cond [(empty? l) 0]
        [else (map alumno-nombre
                     (filter (lambda (alumno) (>= (alumno-faltas alumno) 3))
                          (filter (lambda (alumno) (string=? (condicion alumno) "promovido")) l)))]))
|#