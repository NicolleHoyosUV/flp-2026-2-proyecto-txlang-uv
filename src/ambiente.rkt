#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

(require "sintaxis.rkt" "valores.rkt")
(provide (all-defined-out))

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids (list-of symbol?))
   (vals list?)
   (env ambiente?))
  (ambiente-recursivo
   (nombres (list-of symbol?))
   (idss (list-of (list-of symbol?)))
   (cuerpos (list-of expresion?))
   (env ambiente?)))

; extender-ambiente: (listof symbol) x (listof valor) x ambiente -> ambiente
(define extender-ambiente
  (lambda (ids vals env)
    (if (= (length ids) (length vals))
        (ambiente-extendido ids vals env)
        (eopl:error 'extender-ambiente "Hay ~s identificadores y ~s valores" (length ids) (length vals)))))

; extender-ambiente-recursivo: nombres x idss x cuerpos x ambiente -> ambiente
(define extender-ambiente-recursivo
  (lambda (nombres idss cuerpos env)
    (ambiente-recursivo nombres idss cuerpos env)))

; posicion: symbol x (listof symbol) -> entero | #f
(define posicion
  (lambda (id ids)
    (letrec ((buscar (lambda (l i)
                       (cond ((null? l) #f)
                             ((eqv? (car l) id) i)
                             (else (buscar (cdr l) (+ i 1)))))))
      (buscar ids 0))))

; apply-env: ambiente x symbol -> valor
(define apply-env
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env "Variable no ligada: ~s" id))
      (ambiente-extendido (ids vals ext-env)
        (let ((pos (posicion id ids)))
          (if pos (list-ref vals pos) (apply-env ext-env id))))
      (ambiente-recursivo (nombres idss cuerpos ext-env)
        (let ((pos (posicion id nombres)))
          (if pos
              (cerradura (list-ref idss pos) (list-ref cuerpos pos) env)
              (apply-env ext-env id)))))))