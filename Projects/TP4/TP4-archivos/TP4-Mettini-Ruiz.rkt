;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname TP4-Mettini-Ruiz) (read-case-sensitive #t) (teachpacks ((lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
#|
Trabajo Práctico 4: Listas

Integrantes:
- Mettini, Fabrizio, comisión 3.
- Ruiz, Fabricio, comisión 4.
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Datos ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;; Diseño de datos

; Representaremos fechas mediante strings, según el formato aaaa-mm-dd.
; Representaremos los nombres de las localidades y departamentos mediante strings.
; Representaremos la cantidad de casos (sospechosos, descartados y confirmados)
; mediante números.

(define-struct notificacion [fecha loc conf desc sosp notif])
; notificacion es (String, String, Number, Number, Number, Number)
; Interpretación: un elemento en notificacion representa el conjunto de notificaciones
; registradas en una localidad (loc) hasta un día (fecha), en donde:
; - hay conf casos confirmados de COVID-19
; - hay desc casos descartados de COVID-19
; - sosp casos estaban en estudio.
; El último elemento, notif, indica la cantidad total de notificaciones.

;;;;;;;;;;;; Preparación de los Datos

;;;;;; Datos sobre localidades santafesinas

; Datos de entrada sobre localidades
(define INPUT-LOC (read-csv-file "dataset/santa_fe_departamento_localidad.csv"))
(define DATOS-LOC (rest INPUT-LOC))

; tomar-dos : List(X) -> List(X)
; Dada una lista l de dos o más elementos, tomar-dos calcula
; la lista formada por los dos primeros elementos de l, en
; ese orden.

(check-expect (tomar-dos (list "a" "b")) (list "a" "b")) 
(check-expect (tomar-dos (list 0 1 2 3)) (list 0 1))

(define
  (tomar-dos l)
  (list (first l) (second l)))

; Lista de localidades santafecinas
(define LISTA-LOC (map second DATOS-LOC))
; Lista de localidades santafecinas, con su departamento
(define LISTA-DPTO-LOC (map tomar-dos DATOS-LOC))

;;;;;; Datos sobre notificaciones de COVID-19

; Datos de entrada sobre notificaciones
(define INPUT-NOTIF (read-csv-file "dataset/notificaciones_localidad.csv"))
(define DATOS-NOTIF (rest INPUT-NOTIF))

; Lista de notificaciones
;; Ejercicio 1
;; list->notificacion : List -> Notificacion
;; Dada una lista, devuelve un elemento de tipo notificacion.

(define LISTA-ESPERANZA-2020-06-02 (list "2020-06-02" "ESPERANZA" "1" "63" "0" "64"))
(define NOTIF-ESPERANZA-2020-06-02 (make-notificacion "2020-06-02" "ESPERANZA" 1 63 0 64))

(check-expect (list->notificacion LISTA-ESPERANZA-2020-06-02) NOTIF-ESPERANZA-2020-06-02)

(define (list->notificacion l)
  (make-notificacion (first l)
                     (second l)
                     (string->number (third l))
                     (string->number (fourth l))
                     (string->number (fifth l))
                     (string->number (sixth l))))

(define LISTA-NOTIF (map list->notificacion DATOS-NOTIF))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Consultas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ANTES "2020-04-02")
(define HOY "2020-06-02")
(define LIMITE-CASOS 25)

;;;;;;;;;;;; Consulta 1

;; Ejercicio 2-1
;; Algunos Ejemplos de Notificaciones
(define NOTIF-ROSARIO-2020-06-02 (make-notificacion "2020-06-02" "ROSARIO" 113 5053 72 5238))
(define NOTIF-TOSTADO-2020-06-02 (make-notificacion "2020-06-02" "TOSTADO" 0 6 0 6))
(define NOTIF-RAFAELA-2020-06-01 (make-notificacion "2020-06-01" "RAFAELA" 21 193 8 222))

;; notificacion-actual? : Notificacion -> Boolean
;; Dada una notificacion, devuelve si dicha notificacion es una notificacion del dia HOY.

(check-expect (notificacion-actual? NOTIF-ROSARIO-2020-06-02) #t)
(check-expect (notificacion-actual? NOTIF-RAFAELA-2020-06-01) #f)

(define (notificacion-actual? n)
  (string=? (notificacion-fecha n) HOY))

;; mayor-o-igual-LIMITE-CASOS? : Notificacion -> Boolean
;; Dada una notificacion, devuelve si dicha notificacion indica que la localidad
;; presenta una cantidad mayor o igual de casos confirmados que LIMITE-CASOS.

(check-expect (mayor-o-igual-LIMITE-CASOS? NOTIF-ROSARIO-2020-06-02) #t)
(check-expect (mayor-o-igual-LIMITE-CASOS? NOTIF-TOSTADO-2020-06-02) #f)

(define (mayor-o-igual-LIMITE-CASOS? n)
  (>= (notificacion-conf n) LIMITE-CASOS))

;; localidades-limite-casos : List(Notificacion) -> List(String)
;; Dada una lista de notificaciones, devuelve la lista de localidades que presentan
;; LIMITE-CASOS o más casos confirmados de COVID-19 hasta el día HOY.

(check-expect (localidades-limite-casos empty) empty)
(check-expect (localidades-limite-casos LISTA-NOTIF) (list "ROSARIO" "SANTA FE"))

(define (localidades-limite-casos l)
  (map notificacion-loc                           ; Obtencion de los nombres de la localidades
       (filter mayor-o-igual-LIMITE-CASOS?        ; Filtrar notificaciones por su cantidad de casos confirmados
               (filter notificacion-actual? l)))) ; Filtrar notificaciones por su fecha

;; Ejercicio 2-2
(define LOCALIDADES-LIMITE-CASOS (localidades-limite-casos LISTA-NOTIF))

;;;;;;;;;;;; Consulta 2

;; Ejercicio 3-1
;; obtener-primero-sublistas : List(List(X ..)) -> List(X)
;; Dada una lista de listas de elementos de cualquier tipo, devuelve una lista
;; con el primer elemento de cada sublista.

(check-expect (obtener-primero-sublistas empty) empty)
(check-expect (obtener-primero-sublistas (list (list "a" "b")
                                               (list "b" "c" "d")
                                               (list "c")))
              (list "a" "b" "c"))

(define (obtener-primero-sublistas l)
  (cond [(empty? l) empty]
        [else (cons (first (first l)) (obtener-primero-sublistas (rest l)))]))

;; esta? : String List(String) -> Boolean
;; Dado un String s y una lista de strings l, devuelve si s esta en l.

(check-expect (esta? "a" empty) #f)
(check-expect (esta? "a" (list "a" "b" "b" "a" "c")) #t)

(define (esta? s l)
  (cond [(empty? l) #f]
        [else (if (string=? s (first l))
                  #t
                  (esta? s (rest l)))]))

;; filtrar-strings-repetidas : List(String) -> List(String)
;; Dada una lista con elementos de tipo String, devuelve una lista
;; sin los elementos repetidos.

(check-expect (filtrar-strings-repetidas empty) empty)
(check-expect (filtrar-strings-repetidas (list "a" "b" "b" "a" "c")) (list "b" "a" "c"))

(define (filtrar-strings-repetidas l)
  (cond [(empty? l) empty]
        [else (if (esta? (first l) (rest l))
                  (filtrar-strings-repetidas (rest l))
                  (cons (first l) (filtrar-strings-repetidas (rest l))))]))

(define LISTA-DPTO (filtrar-strings-repetidas (obtener-primero-sublistas LISTA-DPTO-LOC)))

;; Ejercicio 3-2
;; Otros Ejemplos de Notificaciones
(define NOTIF-SUNCHALES-2020-05-08 (make-notificacion "2020-05-08" "SUNCHALES" 0 5 0 5))
(define NOTIF-SANTA-FE-2020-06-02 (make-notificacion "2020-06-02" "SANTA FE" 27 855 45 927))
(define NOTIF-SANTO-TOME-2020-06-02 (make-notificacion "2020-06-02" "SANTO TOME" 5 115 0 120))
(define NOTIF-LAGUNA-PAIVA-2020-06-02 (make-notificacion "2020-06-02" "LAGUNA PAIVA" 0 8 1 9))

;; Ejemplo de Lista de notificaciones
(define LISTA-NOTIF-EJEMPLO
  (list NOTIF-SUNCHALES-2020-05-08
        NOTIF-RAFAELA-2020-06-01
        NOTIF-ROSARIO-2020-06-02
        NOTIF-SANTA-FE-2020-06-02
        NOTIF-SANTO-TOME-2020-06-02
        NOTIF-LAGUNA-PAIVA-2020-06-02))

;; notificacion-del-dia-dado? : Notificacion String -> Boolean
;; Dada una notificacion y una fecha, devuelve si la notificacion es de la fecha dada.

(check-expect (notificacion-del-dia-dado? NOTIF-SUNCHALES-2020-05-08 HOY) #f)
(check-expect (notificacion-del-dia-dado? NOTIF-SANTO-TOME-2020-06-02 HOY) #t)

(define (notificacion-del-dia-dado? n f)
  (string=? (notificacion-fecha n) f))

;; filtrar-segun-fecha : List(Notificacion) String -> List(Notificacion)
;; Dada una lista de notificaciones y una fecha, devuelve una lista de notificaciones
;; de la fecha dada.

(check-expect (filtrar-segun-fecha empty HOY) empty)
(check-expect (filtrar-segun-fecha LISTA-NOTIF-EJEMPLO HOY)
              (list NOTIF-ROSARIO-2020-06-02
                    NOTIF-SANTA-FE-2020-06-02
                    NOTIF-SANTO-TOME-2020-06-02
                    NOTIF-LAGUNA-PAIVA-2020-06-02))

(define (filtrar-segun-fecha l f)
  (cond [(empty? l) empty]
        [else (if (notificacion-del-dia-dado? (first l) f)
                  (cons (first l) (filtrar-segun-fecha (rest l) f))
                  (filtrar-segun-fecha (rest l) f))]))

;; departamento : String List(List(String String)) -> String
;; Dada una localidad y una lista de listas con localidades y su departamento
;; correspondiente, devuelve el departamento al que pertenece.

(check-expect (departamento "CIUDAD INEXISTENTE" LISTA-DPTO-LOC) "")
(check-expect (departamento "SANTA FE" LISTA-DPTO-LOC) "La Capital")

(define (departamento loc l)
  (cond [(empty? l) ""]
        [else (if (string=? loc (second (first l)))
                  (first (first l))
                  (departamento loc (rest l)))]))

;; notificacion-del-dpto-dado? : Notificacion String -> Boolean
;; Dada una notificacion y un departamento, devuelve si la notificacion es de una
;; localidad perteneciente al departamento dado.

(check-expect (notificacion-del-dpto-dado? NOTIF-SANTA-FE-2020-06-02 "La Capital") #t)
(check-expect (notificacion-del-dpto-dado? NOTIF-ROSARIO-2020-06-02 "La Capital") #f)

(define (notificacion-del-dpto-dado? n d)
  (string=? (departamento (notificacion-loc n) LISTA-DPTO-LOC) d))

;; filtrar-segun-dpto : List(Notificacion) String -> List(Notificacion)
;; Dada una lista de notificaciones y un departamento, devuelve una lista de notificaciones
;; del departamento dado.

(check-expect (filtrar-segun-dpto empty "La Capital") empty)
(check-expect (filtrar-segun-dpto LISTA-NOTIF-EJEMPLO "La Capital")
              (list NOTIF-SANTA-FE-2020-06-02
                    NOTIF-SANTO-TOME-2020-06-02
                    NOTIF-LAGUNA-PAIVA-2020-06-02))

(define (filtrar-segun-dpto l d)
  (cond [(empty? l) empty]
        [else (if (notificacion-del-dpto-dado? (first l) d)
                  (cons (first l) (filtrar-segun-dpto (rest l) d))
                  (filtrar-segun-dpto (rest l) d))]))

;; sumar-casos-confirmados : List(Notificacion) -> Number
;; Dada una lista de notificaciones, devuelve la suma de todos los casos confirmados.

(check-expect (sumar-casos-confirmados empty) 0)
(check-expect (sumar-casos-confirmados LISTA-NOTIF-EJEMPLO) 166)

(define (sumar-casos-confirmados l)
  (cond [(empty? l) 0]
        [else (+ (notificacion-conf (first l))
                 (sumar-casos-confirmados (rest l)))]))

;; confirmados-dpto-fecha : List(Notificacion) String String -> Number
;; Dada una lista de notificaciones, un departamento y una fecha, calcula la cantidad de casos
;; confirmados registrados en dicho departamento hasta la fecha en cuestión.

(check-expect (confirmados-dpto-fecha empty "Castellanos" ANTES) 0)
(check-expect (confirmados-dpto-fecha LISTA-NOTIF "Garay" HOY) 3)

(define (confirmados-dpto-fecha l d f)
  (sumar-casos-confirmados (filtrar-segun-dpto (filtrar-segun-fecha l f) d)))

;; Ejercicio 3-3
;; confirmados-dpto-fecha-2 : List(Notificacion) String String -> List(String Number)
;; Dada una lista de notificaciones, un departamento y una fecha, devuelve una lista
;; donde el primer elemento es el nombre del departamento y el segundo el número de casos
;; confirmados que se han registrado allí hasta la fecha en cuestión.

(check-expect (confirmados-dpto-fecha-2 empty "Castellanos" ANTES) empty)
(check-expect (confirmados-dpto-fecha-2 LISTA-NOTIF "Garay" HOY) (list "Garay" 3))

(define (confirmados-dpto-fecha-2 l d f)
  (cond [(empty? l) empty]
        [else (list d (confirmados-dpto-fecha l d f))]))

;; confirmados-por-dpto-aux : List(Notificacion) List(String) String -> List(List(String Number))
;; Dada una lista de notificaciones, una lista de departamentos y una fecha, devuelve una lista
;; de listas de longitud dos: una por cada departamento santafesino, donde el primer elemento
;; sea el nombre del departamento y el segundo sea el número de casos confirmados que se
;; han registrado allí hasta la fecha en cuestión.

(check-expect (confirmados-por-dpto-aux LISTA-NOTIF empty HOY) empty)
(check-expect (confirmados-por-dpto-aux LISTA-NOTIF-EJEMPLO (list "La Capital" "Rosario") HOY)
              (list (list "La Capital" 32) (list "Rosario" 113)))

(define (confirmados-por-dpto-aux l1 l2 f)
  (cond [(empty? l2) empty]
        [else (cons (confirmados-dpto-fecha-2 l1 (first l2) f)
                    (confirmados-por-dpto-aux l1 (rest l2) f))]))

;; confirmados-por-dpto : List(Notificacion) String -> List(List(String Number))
;; Dada una lista de notificaciones y una fecha, devuelve una lista de listas de longitud dos:
;; una por cada departamento santafesino, donde el primer elemento sea el nombre
;; del departamento y el segundo es el número de casos confirmados que se
;; han registrado allí hasta la fecha en cuestión.

(define (confirmados-por-dpto l f)
  (confirmados-por-dpto-aux l LISTA-DPTO f))

;; Ejercicio 3-4
(define CONFIRMADOS-DPTO-ANTES (confirmados-por-dpto LISTA-NOTIF ANTES))
(define CONFIRMADOS-DPTO-HOY (confirmados-por-dpto LISTA-NOTIF HOY))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Salidas ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; separar-filas : String -> String
;; Dado un String s, devuelve otro String que surge de añadir "\n " al final de s.

(check-expect (separar-filas "") "\n ")
(check-expect (separar-filas "ROSARIO, 124") "ROSARIO, 124\n ")

(define (separar-filas s)
  (string-append s "\n "))

;; separar-columnas : List(String Number) -> String
;; Dada una lista donde el primer elemento es un String (en nuestro caso el nombre del departamento)
;; y el segundo un numero (la cantidad de casos confirmados en dicho departamento),
;; devuelve un String que surge de la concatenacion del primer elemento y el segundo
;; separados por ", ".

(check-expect (separar-columnas empty) "")
(check-expect (separar-columnas (list "La Capital" 47)) "La Capital, 47")

(define (separar-columnas l)
  (cond [(empty? l) ""]
        [else (string-append (first l) ", " (number->string (second l)))]))

;;;;;;;;;;;; Consulta 1

;; Ejercicio 4 - loc-lim-casos.csv
(write-file "loc-lim-casos.csv"
            (string-append "Localidades que presentan Limite de Casos\n "
                           (foldr string-append "" (map separar-filas LOCALIDADES-LIMITE-CASOS))))

;;;;;;; Consulta 2

;; Ejercicio 4 - casos-por-dpto-hoy.csv y casos-por-dpto-antes.csv
(write-file "casos-por-dpto-hoy.csv"
            (string-append "Departamento, Casos Confirmados\n "
                           (foldr string-append "" (map separar-filas
                                                        (map separar-columnas CONFIRMADOS-DPTO-HOY)))))

(write-file "casos-por-dpto-antes.csv"
            (string-append "Departamento, Casos Confirmados\n "
                           (foldr string-append "" (map separar-filas
                                                        (map separar-columnas CONFIRMADOS-DPTO-ANTES)))))