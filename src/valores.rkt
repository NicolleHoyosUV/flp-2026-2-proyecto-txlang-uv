#lang eopl
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