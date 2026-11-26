#lang eopl
<<<<<<< Updated upstream
=======
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608


(require "sintaxis.rkt")
(provide (all-defined-out))

; procval: valor procedimiento (clausura)
; Propósito: guarda los parámetros, el cuerpo sin evaluar y el ambiente
; vigente al definir el procedimiento.
; El campo env se declara sin ambiente? para evitar el ciclo de módulos
; (ambiente.rkt requiere a valores.rkt y no al revés).
(define-datatype procval procval?
  (cerradura
   (ids (list-of symbol?))
   (body expresion?)
   (env (lambda (v) #t))))

; unitval: el valor unit
; Propósito: resultado de las expresiones que se evalúan por su efecto

(define-datatype unitval unitval?
  (el-unit))

; valor-unit: -> unitval
; Propósito: la única instancia de unit que usa el intérprete.
(define valor-unit (el-unit))

; regval: valor de registro (instancia creada con new)
; Propósito: guarda el nombre del tipo de registro y una lista de pares
; (campo . valor) con los valores ya evaluados de cada campo.
(define-datatype regval regval?
  (un-registro
   (nombre-tipo symbol?)
   (campos (list-of pair?))))

; extraer-campo-registro: regval x symbol -> any
; Propósito: busca el campo dado dentro de la instancia de registro y retorna
; su valor; reporta error con eopl:error si no es un registro o el campo no existe.
(define extraer-campo-registro
  (lambda (val campo)
    (if (regval? val)
        (cases regval val
          (un-registro (nombre-tipo campos)
            (let ((par (assq campo campos)))
              (if par
                  (cdr par)
                  (eopl:error 'field
                              "El campo ~s no existe en el registro ~s"
                              campo nombre-tipo)))))
        (eopl:error 'field
                    "Se esperaba un registro en field, se recibió: ~s"
                    val))))
>>>>>>> Stashed changes
