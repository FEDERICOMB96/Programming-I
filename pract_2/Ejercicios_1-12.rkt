;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname Ejercicios_1-12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require racket/string)

;###############################################################################
; Ejercicio 1:
; Representamos un punto mediante sus coordenadas cartesianas

; distancia-origen: Number Number -> Number
; recibe las coordenadas de un punto, devuelve la distancia al origen del mismo

(check-expect (distancia-origen 3 4) 5)

(define (distancia-origen x y)
  (sqrt (+ (sqr x) (sqr y))))

;###############################################################################
; Ejercicio 2:
; Representamos dos puntos mediante sus coordenadas cartesianas

; distancia-puntos: Number Number Number Number -> Number
; recibe las coordenadas de dos puntos, devuelve la distancia entre los mismos

(check-expect (distancia-puntos 3 4 0 0) 5)

(define (distancia-puntos x1 y1 x2 y2)
  (sqrt (+ (sqr (- x2 x1))(sqr (- y2 y1)))))

;###############################################################################
; Ejercicio 3:
; Representamos la longitud con números

; vol-cubo: Number -> Number
; recibe la longitud de la arista de un cubo, devuelve su volumen

(check-expect (vol-cubo 1) 1)

(define (vol-cubo arista)
  (expt arista 3))

;###############################################################################
; Ejercicio 4:
; Representamos la longitud con números

; area-cubo: Number -> Number
; recibe la longitud de la arista de un cubo y devuelve su área

(check-expect (area-cubo 2) 24)

(define (area-cubo arista)
  (* 6 (sqr arista)))

;###############################################################################
(define MSJ-ERROR-INDICE-NO-VALIDO "Error, indice no válido")
(define MSJ-ERROR-CADENA-VACIA "Error, la cadena ingresada es vacia")
(define MSJ-ERROR-NO-ES-STRING "Error, ingrese una cadena")

; Ejercicio 5:
; Diseño de datos

; string-insert: String Number -> String
; recibe un string y un número i e inserta "-" en la posición i-ésima del string

(check-expect (string-insert "Hello World" 3) "Hel-lo World")
(check-expect (string-insert "" 2 ) MSJ-ERROR-CADENA-VACIA)
(check-expect (string-insert "Argentina" -3) MSJ-ERROR-INDICE-NO-VALIDO)
(check-expect (string-insert "Chile" 13) MSJ-ERROR-INDICE-NO-VALIDO)
(check-expect (string-insert 42 3) MSJ-ERROR-NO-ES-STRING)

(define (string-insert str i)
  (if (string? str)
      (cond [(and (non-empty-string? str) (indice-válido str i))
             (string-append (inicioS str i) "-" (finS str i))]
            [(non-empty-string? str) MSJ-ERROR-INDICE-NO-VALIDO]
            [else MSJ-ERROR-CADENA-VACIA])
      MSJ-ERROR-NO-ES-STRING))

; Funciones auxiliares:

; indice-válido: String Number -> Boolean
; recibe un string y un número i y devuelve si existe la posición i-ésima del string
  
(check-expect (indice-válido "Gato" 3) #t)
(check-expect (indice-válido "Caballo" -1) #f)
(check-expect (indice-válido "Auto" 5) #f)
  
(define (indice-válido str i)
  (and (>= i 0) (<= i (string-length str))))

; inicioS: String Number -> String
; recibe un string y un número i y devuelve la subcadena formada por los
; caracteres desde la posición inicial hasta la posición i-ésima

(check-expect (inicioS "Hello" 2) "He")

(define (inicioS str i)
        (substring str 0 i))

; finS: String Number -> String
; recibe un string y un número i y devuelve la subcadena formada
; por los caracteres desde la posición i-ésima hasta la posición final

(check-expect (finS "Hello" 2) "llo")

(define (finS str i)
        (substring str i (string-length str)))

;###############################################################################
; Ejercicio 6:
; Diseño de datos

; string-last: String -> String
; recibe una cadena de caracteres, extrae el ultimo caracter de la misma

(check-expect (string-last "Perro") "Perr")
(check-expect (string-last "") MSJ-ERROR-CADENA-VACIA)
(check-expect (string-last 42) MSJ-ERROR-NO-ES-STRING)

(define (string-last str)
  (if (string? str)
      (if (non-empty-string? str)
          (substring str 0 (- (string-length str) 1))
           MSJ-ERROR-CADENA-VACIA)
      MSJ-ERROR-NO-ES-STRING))

;###############################################################################
; Ejercicio 7:
; Diseño de datos

; string-remove-last: String -> String
; recibe una cadena de caracteres, devuelve la misma cadena sin el ultimo
; caracter. En el caso de que la cadena sea vacia se informara de esto

(check-expect (string-remove-last "Elefante") "Elefant")
(check-expect (string-remove-last "") MSJ-ERROR-CADENA-VACIA)
(check-expect (string-remove-last 42) MSJ-ERROR-NO-ES-STRING)

(define (string-remove-last str)
  (if (string? str)
      (if (non-empty-string? str)
          (substring str 0 (- (string-length str) 1))
           MSJ-ERROR-CADENA-VACIA)
      MSJ-ERROR-NO-ES-STRING))

;###############################################################################
; Ejercicio 9:
; Representamos la cantidad de amigos y la cantidad de meses con números enteros,
; los montos con números positivos y los descuentos como valores entre 0 y 1
; (Ejemplo descuento del 25% -> 0.25)

(define DESCUENTO-DOS-AMIGOS 0.10)
(define DESCUENTO-TRES-O-MAS-AMIGOS 0.20)
(define DESCUENTO-DOS-MESES 0.15)
(define DESCUENTO-TRES-O-MAS-MESES 0.25)
(define DESCUENTO-MAXIMO 0.35)
(define PRECIO-ORIGINAL-CUOTA-MENSUAL 650)
(define MSJ-ERROR-NO-ES-NUMERO "Error, ingrese un número")
(define MSJ-ERROR-NO-ES-ENTERO "Error, ingrese un número entero")

; monto-persona: Number Number -> Number
; recibe la cantidad de personas que se están anotando y la cantidad de meses
; que abonan (para que se aplique la promoción deben pagar la misma cantidad
; de meses) y devuelve el monto que el Instituto debe cobrarle a cada uno.

(check-expect (monto-persona 2 2) 975)
(check-expect (monto-persona 3 3) 1267.5)
(check-expect (monto-persona 1 5) 2437.5)

(define (monto-persona cant-personas cant-meses)
  (* PRECIO-ORIGINAL-CUOTA-MENSUAL
     cant-meses
     (- 1 (min DESCUENTO-MAXIMO
               (+ (descuento-cant-personas cant-personas)
                  (descuento-cant-meses cant-meses))))))

; Funciones Auxiliares:

; descuento-cant-amigos: Number -> Number
; recibe la cantidad de personas que se están anotando
; y devuelve el descuento correspondiente

(check-expect (descuento-cant-personas 1) 0)
(check-expect (descuento-cant-personas 2) DESCUENTO-DOS-AMIGOS)
(check-expect (descuento-cant-personas 3) DESCUENTO-TRES-O-MAS-AMIGOS)
(check-expect (descuento-cant-personas -1) MSJ-ERROR-NO-ES-ENTERO)
(check-expect (descuento-cant-personas "") MSJ-ERROR-NO-ES-NUMERO)

(define (descuento-cant-personas cant-personas)
  (if (integer? cant-personas)
      (cond [(= cant-personas 1) 0]
            [(= cant-personas 2) DESCUENTO-DOS-AMIGOS]
            [(>= cant-personas 3) DESCUENTO-TRES-O-MAS-AMIGOS]
            [else MSJ-ERROR-NO-ES-ENTERO])
      MSJ-ERROR-NO-ES-NUMERO))

; descuento-cant-meses: Number -> Number
; recibe la cantidad de meses que se están abonando
; y devuelve el descuento correspondiente

(check-expect (descuento-cant-meses 1) 0)
(check-expect (descuento-cant-meses 2) DESCUENTO-DOS-MESES)
(check-expect (descuento-cant-meses 3) DESCUENTO-TRES-O-MAS-MESES)
(check-expect (descuento-cant-meses -1) MSJ-ERROR-NO-ES-ENTERO)
(check-expect (descuento-cant-meses "") MSJ-ERROR-NO-ES-NUMERO)

(define (descuento-cant-meses cant-meses)
  (if (integer? cant-meses)
      (cond [(= cant-meses 1) 0]
            [(= cant-meses 2) DESCUENTO-DOS-MESES]
            [(>= cant-meses 3) DESCUENTO-TRES-O-MAS-MESES]
            [else MSJ-ERROR-NO-ES-ENTERO])
      MSJ-ERROR-NO-ES-NUMERO))

;###############################################################################
; Ejercicio 10:
; Representamos las edades (en meses) y la hemoglobina en sangre (en g/dl)
; con números positivos

(define VALOR-MINIMO-HASTA-1-MES 13)
(define VALOR-MINIMO-ENTRE-1-Y-6-MESES 10)
(define VALOR-MINIMO-ENTRE-6-Y-12-MESES 11)
(define VALOR-MINIMO-ENTRE-1-Y-5-AÑOS 11.5)
(define VALOR-MINIMO-ENTRE-5-Y-10-AÑOS 12.6)
(define VALOR-MINIMO-MAS-DE-10-AÑOS 13)

; anemia: Number Number -> Boolean
; recibe la edad (en meses) y su nivel de hemoglobina en sangre
; y devuelve si la persona está anémica o no

(check-expect (anemia 1 12) #t)
(check-expect (anemia 5 12) #f)
(check-expect (anemia 12 10) #t)
(check-expect (anemia 13 11) #t)
(check-expect (anemia 64 13) #f)
(check-expect (anemia 150 12) #t)

(define (anemia edad nivel-de-hemoglobina)
  (cond [(<= edad 1)
         (< nivel-de-hemoglobina VALOR-MINIMO-HASTA-1-MES)]
        [(and (> edad 1) (<= edad 6))
         (< nivel-de-hemoglobina VALOR-MINIMO-ENTRE-1-Y-6-MESES)]
        [(and (> edad 6) (<= edad 12))
         (< nivel-de-hemoglobina VALOR-MINIMO-ENTRE-6-Y-12-MESES)]
        [(and (> edad (años->meses 1)) (<= edad (años->meses 5)))
         (< nivel-de-hemoglobina VALOR-MINIMO-ENTRE-1-Y-5-AÑOS)]
        [(and (> edad (años->meses 5)) (<= edad (años->meses 10)))
         (< nivel-de-hemoglobina VALOR-MINIMO-ENTRE-5-Y-10-AÑOS)]
        [(> edad (años->meses 10))
         (< nivel-de-hemoglobina VALOR-MINIMO-MAS-DE-10-AÑOS)]))

; Funciones Auxiliares:

; años->meses: Number -> Number
; recibe una cantidad de años y devuelve su equivalente en meses

(check-expect (años->meses 1) 12)

(define (años->meses años)
  (* años 12))

;###############################################################################
; Ejercicio 11:
; Diseño de datos

; autopromediable: Numbe Number Number -> Number
; recibe tres números y si son autopromediables devuelve el producto entre ellos
; sino devuelve la suma entre ellos

(check-expect (autopromediable 1 2 3) 6)
(check-expect (autopromediable 1 1 2) 4)

(define (autopromediable a b c)
  (if (autopromediable? a b c)
      (* a b c)
      (+ a b c)))

; Funciones Auxiliares:

; autopromediable?: Number Number Number -> Boolean
; recibe tres números y devuelve si son autopromediables o no

(check-expect (autopromediable? 1 2 3) #t)
(check-expect (autopromediable? 1 1 2) #f)

(define (autopromediable? a b c)
  (or (= a (promedio-2-números b c))
      (= b (promedio-2-números a c))
      (= c (promedio-2-números a b))))

; promedio-2-números: Number Number -> Number
; recibe dos números y devuelve el promedio entre ellos

(check-expect (promedio-2-números 2 4) 3)

(define (promedio-2-números a b)
  (/ (+ a b) 2))

;###############################################################################
; Ejercicio 12:
; Representamos el grado del combustible con números enteros (2 o 3),
; la cantidad de litros de combustible con números positivos y la mejora de
; rendimiento con valores entre 0 y 1 (Ejemplo: Mejora del 10% -> 0.10)

(define CONSUMO-PROMEDIO-CIUDAD 8)
(define CONSUMO-PROMEDIO-RUTA 11)
(define MEJORA-RENDIMIENTO 0.10)
(define MSJ-ERROR-GRADO-NO-VALIDO "Error, el grado de la nafta no es válido")
(define MSJ-ERROR-CANT-LITROS-RESTANTES "Error, cantidad de litros restantes no válida")

; autonomía: Number Number -> String
; recibe la cantidad de litros restantes en el tanque y el grado del combustible
; y devuelve la autonomía del vehículo tanto en ciudad como en ruta

(check-expect (autonomía 20 2) "Autonomía en ciudad: 160km. Autonomía en ruta: 220km.")
(check-expect (autonomía 20 3) "Autonomía en ciudad: 176km. Autonomía en ruta: 242km.")
(check-expect (autonomía 20 1) MSJ-ERROR-GRADO-NO-VALIDO)
(check-expect (autonomía -1 3) MSJ-ERROR-CANT-LITROS-RESTANTES)

(define (autonomía cant-litros-restantes grado-combustible)
  (cond [(not (grado-combustible-válido? grado-combustible))
         MSJ-ERROR-GRADO-NO-VALIDO]
        [(not (cant-litros-restantes-válida? cant-litros-restantes))
         MSJ-ERROR-CANT-LITROS-RESTANTES]
        [else
         (string-append "Autonomía en ciudad: "
                        (number->string (autonomía-ciudad cant-litros-restantes grado-combustible))
                        "km. "
                        "Autonomía en ruta: "
                        (number->string (autonomía-ruta cant-litros-restantes grado-combustible))
                        "km.")]))

; Funciones Auxiliares:

; cant-litros-restantes-válida?: Number -> Boolean
; recibe la cantidad de litros restantes en el tanque
; y devuelve si es una cantidad válida

(check-expect (cant-litros-restantes-válida? 1.5) #t)
(check-expect (cant-litros-restantes-válida? 0) #t)
(check-expect (cant-litros-restantes-válida? -1) #f)

(define (cant-litros-restantes-válida? cant-litros-restantes)
  (not (negative? cant-litros-restantes)))

; grado-combustible-válido?: Number -> Boolean
; recibe el grado del combustible y devuelve si es una cantidad válida

(check-expect (grado-combustible-válido? 2) #t)
(check-expect (grado-combustible-válido? 3) #t)
(check-expect (grado-combustible-válido? 1) #f)

(define (grado-combustible-válido? grado-combustible)
  (or (= grado-combustible 2) (= grado-combustible 3)))

; autonomía-ciudad: Number Number -> Number
; recibe la cantidad de litros restantes en el tanque y el grado del combustible
; y devuelve la autonomía del vehículo en ciudad

(check-expect (autonomía-ciudad 1 2) (* CONSUMO-PROMEDIO-CIUDAD 1 1))
(check-expect (autonomía-ciudad 1 3) (* CONSUMO-PROMEDIO-CIUDAD 1 (+ 1 MEJORA-RENDIMIENTO)))

(define (autonomía-ciudad cant-litros-restantes grado-combustible)
  (* CONSUMO-PROMEDIO-CIUDAD
     cant-litros-restantes
     (if (= grado-combustible 3) (+ 1 MEJORA-RENDIMIENTO) 1)))

; autonomía-ruta: Number Number -> Number
; recibe la cantidad de litros restantes en el tanque y el grado del combustible
; y devuelve la autonomía del vehículo en ruta

(check-expect (autonomía-ruta 1 2) (* CONSUMO-PROMEDIO-RUTA 1 1))
(check-expect (autonomía-ruta 1 3) (* CONSUMO-PROMEDIO-RUTA 1 (+ 1 MEJORA-RENDIMIENTO)))

(define (autonomía-ruta cant-litros-restantes grado-combustible)
  (* CONSUMO-PROMEDIO-RUTA
     cant-litros-restantes
     (if (= grado-combustible 3) (+ 1 MEJORA-RENDIMIENTO) 1)))
