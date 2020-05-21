;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicio_10) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------
;; Ejercicio 10

(define-struct Alumno [nombre promedio asistencia])
;; Alumno es (String, Number, Number)
;; Intepretación: El primer elemento es el nombre del alumno, el segundo es el
;; promedio de sus calificaciones (un valor entre 0 y 10) y el tercero es el
;; porcentaje de asistencia a clases (un valor entre 0 y 100).

(define PORCENTAJE-ASISTENCIA-REGULAR 60) ; Porcentaje mínimo de asistencia para regularizar
(define PROMEDIO-REGULAR 6)               ; Promedio para regularizar
(define PROMEDIO-PROMOVIDO 8)             ; Promedio para promover

(define MENSAJE-ERROR "Tipo de dato inválido") ; Si la función condición no recibe una
                                               ; entrada de tipo Alumno

;; -------------------------------------
;; condicion: Alumno -> String
;; Dado un alumno, devuelve un string indicando su condición (Libre, Regular o Promovido).

(check-expect (condicion (make-Alumno "Pedro" 6 40)) "Libre")
(check-expect (condicion (make-Alumno "Lucas" 5 80)) "Libre")
(check-expect (condicion (make-Alumno "Mateo" 6 80)) "Regular")
(check-expect (condicion (make-Alumno "Marco" 8 80)) "Promovido")

(define (condicion alumno)
  (if (Alumno? alumno)
      (cond [(< (Alumno-asistencia alumno) PORCENTAJE-ASISTENCIA-REGULAR) "Libre"]
            [(< (Alumno-promedio alumno) PROMEDIO-REGULAR) "Libre"]
            [(< (Alumno-promedio alumno) PROMEDIO-PROMOVIDO) "Regular"]
            [else "Promovido"])
      MENSAJE-ERROR))
  