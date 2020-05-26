;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname TP1-Mettini-Ruiz) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|
Trabajo Práctico 1: Receta para el diseño

Integrantes:
- Mettini, Fabrizio, comisión 3.
- Ruiz, Fabricio , comisión 4.
|#

;###############################################################################
; Ejercicio 1

#|
 Representamos las longitudes de la pileta mediante números,
 siendo 1 el equivalente a 1 metro.
 Las cantidades de pintura estan representadas con números,
 siendo 1 el equivalente a 1 litro.
|#

(define PINTURA-TIPO-1 1)
(define PINTURA-TIPO-2 2)
(define METROS-CUADRADOS-POR-LITRO-TIPO-1 16)
(define METROS-CUADRADOS-POR-LITRO-TIPO-2 12)
(define MSJ-ERROR-GENERAL "Ha ocurrido un error.")

#|
 pintura-necesaria: Number Number Number Number -> Number
 Dados 4 números que representan el ancho, largo, profundidad de la pileta
 (todas positivas) y el tipo de pintura,
 Calcula la superficie a pintar y devuelve la cantidad de litros necesaria
 segun el tipo de pintura.
|#

(check-expect (pintura-necesaria 2 2 2 1) 1.25)
(check-expect (pintura-necesaria 3 3 3 2) 3.75)
(check-expect (pintura-necesaria 1 1 1 3) MSJ-ERROR-GENERAL)
(check-expect (pintura-necesaria 1 0 1 2) MSJ-ERROR-GENERAL)
(check-expect (pintura-necesaria 1 1 1 "2") MSJ-ERROR-GENERAL)

(define (pintura-necesaria ancho largo profundidad tipo-pintura)
  (if (entradas-válidas-1? ancho largo profundidad tipo-pintura)
      (/ (superficie-pileta ancho largo profundidad)
         (metros-cuadrados-por-litro tipo-pintura))
      MSJ-ERROR-GENERAL))

; Funciones auxiliares.

#|
 entradas-válidas-1?: Number Number Number Number -> Boolean
 Recibe las dimensiones de la pileta y el tipo de pintura y devuelve si las
 entradas ingresadas son válidas.
|#

(check-expect (entradas-válidas-1? 1 1 1 -1) #f)
(check-expect (entradas-válidas-1? 1 1 0 1) #f)
(check-expect (entradas-válidas-1? 1 1 1 2) #t)
(check-expect (entradas-válidas-1? 1 1 1 3) #f)
  
(define (entradas-válidas-1? ancho largo profundidad tipo-pintura)
  (and (longitud-válida? ancho)
       (longitud-válida? largo)
       (longitud-válida? profundidad)
       (tipo-pintura-válido? tipo-pintura)))

#|
 longitud-válida?: Number -> Boolean
 Recibe una longitud y devuelve si es una entrada válida.
|#

(check-expect (longitud-válida? 2) #t)
(check-expect (longitud-válida? -2) #f)
(check-expect (longitud-válida? 2+i) #f)
  
(define (longitud-válida? longitud)
  (and (real? longitud)
       (positive? longitud)))

#|
 tipo-pintura-válido?: Number -> Boolean
 Recibe un tipo de pintura y devuelve si es una entrada válida.
|#

(check-expect (tipo-pintura-válido? 1) #t)
(check-expect (tipo-pintura-válido? 2) #t)
(check-expect (tipo-pintura-válido? 1.3) #f)

(define (tipo-pintura-válido? tipo-pintura)
  (and (integer? tipo-pintura)
       (or (= tipo-pintura PINTURA-TIPO-1)
           (= tipo-pintura PINTURA-TIPO-2))))

#|
 superficie-pileta: Number Number Number -> Number
 Recibe las dimensiones de la pileta y devuelve la superficie
 a pintar de la misma.
|#

(check-expect (superficie-pileta 2 2 2) 20)
(check-expect (superficie-pileta 2 3 4) 46)
  
(define (superficie-pileta ancho largo profundidad)
  (+ (área-rectángulo ancho largo)
     (* 2 (área-rectángulo largo profundidad))
     (* 2 (área-rectángulo ancho profundidad))))

#|
 área-rectángulo: Number Number -> Number
 Recibe la base y la altura de un rectángulo y devuelve su área.
|#
  
(check-expect (área-rectángulo 1 2) 2)
  
(define (área-rectángulo base altura)
  (* base altura))

#|
 metros-cuadrados-por-litro: Number -> Number
 Recibe el tipo de pintura y devuelve cuantos metros cuadrados se pueden pintar
 con un 1 litro de pintura de dicho tipo.
|#

(check-expect (metros-cuadrados-por-litro 1) METROS-CUADRADOS-POR-LITRO-TIPO-1)
(check-expect (metros-cuadrados-por-litro 2) METROS-CUADRADOS-POR-LITRO-TIPO-2)
  
(define (metros-cuadrados-por-litro tipo-pintura)
  (if (= tipo-pintura PINTURA-TIPO-1)
      METROS-CUADRADOS-POR-LITRO-TIPO-1
      METROS-CUADRADOS-POR-LITRO-TIPO-2))

;###############################################################################
; Ejercicio 2

#|
 Representamos las longitudes de la pileta mediante números,
 siendo 1 el equivalente a 1 metro.
 Las cantidades de pintura estan representadas con números,
 siendo 1 el equivalente a 1 litro.
|#

(define LITROS-POR-BALDE 2)
(define CANTIDAD-JUSTA "Cantidad justa")

#|
 chequeo-pintura: Number Number Number Number Number -> String
 Se ingresan 5 números positivos, que representan el ancho, largo,
 profundidad de la pileta, el tipo de pintura y la cantidad de baldes
 disponibles de pintura.
 Si las entradas son válidas devuelve un mensaje indicando si la cantidad de
 pintura disponible es la correcta o, en caso de no serlo,
 cuantos baldes sobran o faltan.
 En caso de que alguna entrada no sea válida, se informa que ha ocurrido un error.
|#

(check-expect (chequeo-pintura 1 1 1 1 1) CANTIDAD-JUSTA)
(check-expect (chequeo-pintura 1 1 1 1 3) "Sobran 2 baldes.")
(check-expect (chequeo-pintura 1 1 1 2 3) "Sobran 2 baldes.")
(check-expect (chequeo-pintura 4 3 2 1 1) "Faltan 1 baldes.") 
(check-expect (chequeo-pintura 4 4 4 2 1) "Faltan 3 baldes.") 
(check-expect (chequeo-pintura 4 -4 4 2 1) "Ha ocurrido un error.")

(define (chequeo-pintura ancho largo profundidad tipo-pintura cant-baldes)
  (if (entradas-válidas-2? ancho largo profundidad tipo-pintura cant-baldes)
      (mensaje-correspondiente ancho largo profundidad tipo-pintura cant-baldes)
      MSJ-ERROR-GENERAL))

; Funciones Auxiliares:

#|
 entradas-válidas-2?: Number Number Number Number Number -> Boolean
 Recibe las dimensiones de la pileta, el tipo de pintura y la cantidad de baldes
 y devuelve si las entradas ingresadas son válidas.
|#

(check-expect (entradas-válidas-2? 1 1 1 1 1) #t)
(check-expect (entradas-válidas-2? 1 1 1 0 1) #f)
(check-expect (entradas-válidas-2? 1 4 1 2 3) #t)
(check-expect (entradas-válidas-2? 1 0 1 2 3) #f)

(define (entradas-válidas-2? ancho largo profundidad tipo-pintura cant-baldes)
  (and (entradas-válidas-1? ancho largo profundidad tipo-pintura)
       (cant-baldes-válida? cant-baldes)))

#|
 cant-baldes-válida?: Number -> Boolean
 Dado 1 número positivo que indica la cantidad disponible de baldes
 de pintura, revisa que sea un número válido, y en caso de serlo devuelve #true.
|#

(check-expect (cant-baldes-válida? 2) #t)
(check-expect (cant-baldes-válida? 0) #f)
(check-expect (cant-baldes-válida? -1) #f)

(define (cant-baldes-válida? cant-baldes)
  (and (real? cant-baldes)
       (positive? cant-baldes)))

#|
 mensaje-correspondiente: Number Number Number Number Number -> String
 Dados 5 números positivos, que representan el ancho, largo,
 profundidad de la pileta, el tipo de pintura y la cantidad de baldes
 disponibles de pintura, devuelve un mensaje indicando si la cantidad de pintura
 es la correcta o, en caso de no serlo, cuantos baldes sobran o faltan.
|#

(check-expect (mensaje-correspondiente 1 1 1 1 1) CANTIDAD-JUSTA)
(check-expect (mensaje-correspondiente 1 1 1 1 3) "Sobran 2 baldes.")
(check-expect (mensaje-correspondiente 1 1 1 2 3) "Sobran 2 baldes.")
(check-expect (mensaje-correspondiente 4 3 2 1 1) "Faltan 1 baldes.") 
(check-expect (mensaje-correspondiente 4 4 4 2 1) "Faltan 3 baldes.") 

(define
  (mensaje-correspondiente ancho largo profundidad tipo-pintura cant-baldes)
  (cond [(= (pintura-necesaria ancho largo profundidad tipo-pintura)
            (* cant-baldes LITROS-POR-BALDE)) CANTIDAD-JUSTA]
        [(> (pintura-necesaria ancho largo profundidad tipo-pintura)
            (* cant-baldes LITROS-POR-BALDE))
         (faltan-baldes (- (pintura-necesaria ancho largo profundidad tipo-pintura)
                           (* cant-baldes LITROS-POR-BALDE)))]
        [else (sobran-baldes (- (* cant-baldes LITROS-POR-BALDE)
                                (pintura-necesaria ancho largo profundidad tipo-pintura)))]))
#|
 faltan-baldes: Number -> String
 (Si la cantidad de pintura necesaria es mayor a la que tenemos).
 Dado 1 número positivo que indica la cantidad de litros que nos faltan.
 Calcula cuantos baldes adicionales de pintura se necesitan.
|#

(check-expect (faltan-baldes 2) "Faltan 1 baldes.")
(check-expect (faltan-baldes 3) "Faltan 2 baldes.")
(check-expect (faltan-baldes 3.5) "Faltan 2 baldes.")              

(define (faltan-baldes litros-faltantes)
  (string-append "Faltan "
                 (number->string (ceiling (/ litros-faltantes
                                             LITROS-POR-BALDE)))
                 " baldes."))

#|
 sobran-baldes: Number -> String
 (Si la cantidad de pintura necesaria es menor a la que tenemos).
 Dado 1 número positivo que indica la cantidad de litros que nos sobran,
 calcula cuantos baldes de pintura se tienen de más.
|#

(check-expect (sobran-baldes 1) CANTIDAD-JUSTA)
(check-expect (sobran-baldes 3) "Sobran 1 baldes.")
(check-expect (sobran-baldes 4) "Sobran 2 baldes.")

(define (sobran-baldes litros-sobrantes)
  (if (< litros-sobrantes LITROS-POR-BALDE)
      CANTIDAD-JUSTA
      (string-append "Sobran "
                     (number->string (floor (/ litros-sobrantes
                                               LITROS-POR-BALDE)))
                     " baldes.")))

;###############################################################################
; Ejercicio 3

#|
  Si, resultó de utilidad la función del apartado 1 (junto con sus funciones
 auxiliares) para completar el apartado 2, ya que nos permitió reducir
 la complejidad del problema, transformando al mismo en subproblemas de
 solución más sencilla.
  Por lo tanto, si sólo se hubiese pedido diseñar la función del apartado 2,
 hubiese sido conveniente diseñar también la función del apartado 1.
|#
