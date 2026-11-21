#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

(require "sintaxis.rkt"
         "ambiente.rkt"
         "almacen.rkt"
         "valores.rkt"
         "primitivas.rkt"
         "interprete.rkt")

(provide (all-defined-out))

(define valor->texto
  (lambda (v)
    (valor->texto-prim v)))

(define chequear-programa
  (lambda (pgm) pgm))

(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "El chequeador de tipos llega en la segunda entrega")))

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (inicializar-almacen!)
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (decls cuerpo)
            (value-of cuerpo (ambiente-vacio))))))))

(define correr
  (lambda (texto) (valor->texto (interpretar texto))))