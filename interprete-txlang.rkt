#lang eopl
; Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
; Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

#|
  Interpretador de TxLang-UV.
  Proyecto final del curso Fundamentos de Interpretación y Compilación de
  Lenguajes de Programación, Universidad del Valle.

  Este archivo funciona como la fachada principal (sección 4.1 del enunciado)
  reuniendo los módulos de la carpeta src/ y exportando la interfaz que
  requieren pruebas-txlang.rkt y ejemplos-anexo.rkt.
|#

;; Importamos los módulos construidos en la carpeta src/
(require "src/sintaxis.rkt"
         "src/unparse.rkt"
         "src/ambiente.rkt"
         "src/valores.rkt"
         "src/primitivas.rkt"
         "src/interprete.rkt")

;;; ==========================================================
;;; 6. Valores expresados (Formateador de salida)
;;; ==========================================================

(define valor->texto
  (lambda (v)
    (valor->texto-prim v)))

;;; ==========================================================
;;; 7. Chequeador de tipos (segunda entrega)
;;; ==========================================================

(define chequear-programa
  (lambda (pgm) pgm))

(define tipo-del-programa
  (lambda (texto)
    (eopl:error 'tipo-del-programa "El chequeador de tipos llega en la segunda entrega")))

;;; ==========================================================
;;; 9. Interfaz
;;; ==========================================================

(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (begin
        (chequear-programa pgm)
        (cases programa pgm
          (un-programa (decls cuerpo) 
            (value-of cuerpo (ambiente-vacio))))))))

;; Lo que usan las baterías: un texto de TxLang-UV y el valor como texto.
(define correr
  (lambda (texto) (valor->texto (interpretar texto))))

;; POR ESTO:
(provide (all-defined-out)
         (all-from-out "src/sintaxis.rkt")
         (all-from-out "src/unparse.rkt")
         (all-from-out "src/ambiente.rkt")
         (all-from-out "src/valores.rkt")
         (all-from-out "src/primitivas.rkt")
         (all-from-out "src/interprete.rkt"))