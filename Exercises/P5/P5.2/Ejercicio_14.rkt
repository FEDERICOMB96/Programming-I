;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Ejercicio_14) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
;; regular? : String -> Bool
;; Dado un string, devuelve si es igual a "regular".

(define (regular? s)
  (string=? s "regular"))

;; faltas-regulares : List(alumno) -> Number
;; Dada una lista de alumnos, devuelve la suma de las ausencias de los alumnos regulares.

(check-expect (faltas-regulares (list (make-alumno "Juan Computación" 7 2)
                                      (make-cliente "Carlos Software" 3.5 4)
                                      (make-alumno "Ada Lovelace" 10 1))) 2)
;; -----------------
;; promovidos-ausentes