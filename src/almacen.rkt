#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

(provide (all-defined-out))

#|TAD almacén: liga referencias a valores. Es independiente del ambiente.

   Representación (privada): vector que crece al llenarse, más un
   contador de celdas usadas. Una referencia es un entero (índice).

   Interfaz:
   inicializar-almacen! : () -> void
     Deja el almacén vacío. Se llama al iniciar cada programa.

   nueva-celda : valor -> referencia
     Reserva una celda con el valor dado y devuelve su referencia.

   leer-celda : referencia -> valor
     Contenido de la celda. Error si la referencia no es válida.

   escribir-celda! : referencia x valor -> void
     Reemplaza el contenido de la celda (definitivo en la entrega 1).
     Error si la referencia no es válida.

   Nota: toda reserva pasa por nueva-celda y toda escritura por
   escribir-celda!, para poder agregar el registro de transacciones
   en la entrega 2 sin cambiar la interfaz.|#

(define el-almacen (make-vector 16 0))
(define celdas-usadas 0)

; inicializar-almacen!: -> void
(define inicializar-almacen!
  (lambda ()
    (set! el-almacen (make-vector 16 0))
    (set! celdas-usadas 0)))

; crecer-almacen!: -> void
; Propósito: duplica la capacidad del vector copiando las celdas.
(define crecer-almacen!
  (lambda ()
    (let* ((n (vector-length el-almacen))
           (nuevo (make-vector (* 2 n) 0)))
      (letrec ((copiar (lambda (i)
                         (if (< i n)
                             (begin (vector-set! nuevo i (vector-ref el-almacen i))
                                    (copiar (+ i 1)))
                             (set! el-almacen nuevo)))))
        (copiar 0)))))

; nueva-celda: valor -> referencia
(define nueva-celda
  (lambda (val)
    (if (= celdas-usadas (vector-length el-almacen))
        (crecer-almacen!)
        #t)
    (let ((ref celdas-usadas))
      (vector-set! el-almacen ref val)
      (set! celdas-usadas (+ celdas-usadas 1))
      ref)))

; verificar-referencia: symbol x any -> #t | error
(define verificar-referencia
  (lambda (quien ref)
    (if (and (integer? ref) (>= ref 0) (< ref celdas-usadas))
        #t
        (eopl:error quien "Referencia inválida: ~s" ref))))

; leer-celda: referencia -> valor
(define leer-celda
  (lambda (ref)
    (verificar-referencia 'leer-celda ref)
    (vector-ref el-almacen ref)))

; escribir-celda!: referencia x valor -> void
(define escribir-celda!
  (lambda (ref val)
    (verificar-referencia 'escribir-celda! ref)
    (vector-set! el-almacen ref val)))