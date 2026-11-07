#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

(require "sintaxis.rkt" 
         "valores.rkt" 
         "ambiente.rkt" 
         "primitivas.rkt")

(provide (all-defined-out))

;; ==========================================================
;; Evaluador Principal (value-of) - Punto 7
;; ==========================================================
;; value-of: expresion x ambiente -> valor
(define value-of
  (lambda (exp env)
    (cases expresion exp

      ;; 1. Literales
      (num-exp (n) n)
      (float-exp (n) n)
      (string-exp (s) (substring s 1 (- (string-length s) 1)))
      (char-exp (c) c)
      (true-exp () #t)
      (false-exp () #f)
      (unit-exp () valor-unit)

      ;; 2. Variables
      (var-exp (id)
        (apply-env env id))

      ;; 3. Ligaduras locales (let)
      (let-exp (ids exps body)
        (let ((vals (map (lambda (e) (value-of e env)) exps)))
          (value-of body (extender-ambiente ids vals env))))

      ;; 4. Secuencia (begin)
      (begin-exp (exp1 exps)
        (let loop ((actual exp1)
                   (resto exps))
          (let ((val (value-of actual env)))
            (if (null? resto)
                val
                (loop (car resto) (cdr resto))))))

      ;; 5. Condicional (if / elif / else) con verificación booleana estricta
      (if-exp (test-exp then-exp elif-tests elif-thens else-exp)
        (let ((val-test (value-of test-exp env)))
          (if (not (boolean? val-test))
              (eopl:error 'value-of "La condición del if debe evaluar a booleano, se obtuvo: ~s" val-test)
              (if val-test
                  (value-of then-exp env)
                  (eval-elifs elif-tests elif-thens else-exp env)))))

      ;; 6. Ciclo (while)
      (while-exp (cond-exp body-exp)
        (let loop ()
          (let ((val-cond (value-of cond-exp env)))
            (if (not (boolean? val-cond))
                (eopl:error 'value-of "La condición del while debe evaluar a booleano, se obtuvo: ~s" val-cond)
                (if val-cond
                    (begin
                      (value-of body-exp env)
                      (loop))
                    valor-unit)))))

      ;; 7. Primitivas
      (prim-exp (prim exps)
        (let ((args (map (lambda (e) (value-of e env)) exps)))
          (apply-primitiva prim args)))

      ;; PENDIENTE: Tu compañero integrará aquí ref, deref, set, proc, apply y letrec
      (else
       (eopl:error 'value-of "Construcción no implementada en la 1ra entrega: ~s" exp)))))

;; Función auxiliar para evaluar ramas elif en orden
(define eval-elifs
  (lambda (tests thens else-exp env)
    (if (null? tests)
        (value-of else-exp env)
        (let ((val-test (value-of (car tests) env)))
          (if (not (boolean? val-test))
              (eopl:error 'value-of "La condición del elif debe evaluar a booleano, se obtuvo: ~s" val-test)
              (if val-test
                  (value-of (car thens) env)
                  (eval-elifs (cdr tests) (cdr thens) else-exp env)))))))

;; Función de entrada para ejecutar la 1ra entrega
(define interpretar
  (lambda (texto)
    (let ((pgm (scan&parse texto)))
      (cases programa pgm
        (un-programa (decls cuerpo)
          (value-of cuerpo (ambiente-vacio)))))))