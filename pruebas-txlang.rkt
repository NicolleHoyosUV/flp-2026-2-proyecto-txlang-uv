#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

#|
  Batería de pruebas del interpretador de TxLang-UV.

  Cada prueba compara el texto que devuelve `correr` con el que se espera, o
  comprueba que un programa se rechace. La primera entrega deja aquí al
  menos 15 programas que cubran todo su alcance; la segunda, al menos 20 que
  cubran el proyecto completo y al menos 5 programas mal tipados con el
  error que el chequeador reporta en cada uno.

  Se ejecuta con:  racket pruebas-txlang.rkt
|#

(require (only-in racket/base with-handlers exn:fail? exn-message))
(require "interprete-txlang.rkt")

(define pasadas 0)
(define fallidas 0)

;; Corre el programa y, si aborta, devuelve el mensaje precedido de ERROR.
(define intentar
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
      (correr texto))))

(define empieza-con-error?
  (lambda (texto)
    (and (>= (string-length texto) 5) (string=? (substring texto 0 5) "ERROR"))))

(define verificar
  (lambda (etiqueta obtenido esperado)
    (if (string=? obtenido esperado)
        (begin (set! pasadas (+ pasadas 1))
               (eopl:printf "  ok    ~a~%" etiqueta))
        (begin (set! fallidas (+ fallidas 1))
               (eopl:printf "  FALLA ~a: se esperaba ~s y llegó ~s~%"
                            etiqueta esperado obtenido)))))

;; El programa devuelve el texto esperado.
(define prueba
  (lambda (etiqueta texto esperado)
    (verificar etiqueta (intentar texto) esperado)))

;; El chequeador asigna al programa el tipo esperado.
(define prueba-tipo
  (lambda (etiqueta texto esperado)
    (verificar etiqueta
               (with-handlers ((exn:fail? (lambda (e) (string-append "ERROR: " (exn-message e)))))
                 (tipo-del-programa texto))
               esperado)))

;; El programa se rechaza, por el chequeador o por el intérprete.
(define prueba-rechazo
  (lambda (etiqueta texto)
    (let ((r (intentar texto)))
      (if (empieza-con-error? r)
          (begin (set! pasadas (+ pasadas 1))
                 (eopl:printf "  ok    ~a  [~a]~%" etiqueta r))
          (begin (set! fallidas (+ fallidas 1))
                 (eopl:printf "  FALLA ~a: se esperaba un rechazo y llegó ~s~%" etiqueta r))))))

;; unparse del árbol de un texto vuelve a parsear al mismo árbol.
(define ida-y-vuelta
  (lambda (etiqueta texto)
    (let ((una (unparse (scan&parse texto))))
      (verificar etiqueta (unparse (scan&parse una)) una))))

;;; ==========================================================
;;; Primera entrega: léxico, gramática, unparse y núcleo
;;; ==========================================================

(eopl:printf "~%== Primera entrega ==~%")

(prueba "literal entero" "42" "42")
(ida-y-vuelta "unparse de un literal" "42")
(prueba "literal flotante" "3.14" "3.14")
(prueba "literal cadena" "\"hola\"" "hola")
(prueba "literal booleano" "true" "true")
(prueba "let con múltiples variables" "let a = 10, b = 20 in +(a, b) end" "30")
(prueba "secuencia begin" "begin 1; 2; 3 end" "3")
(prueba "if rama true" "if true then 42 else 0 end" "42")
(prueba "if con elif" "if false then 1 elif true then 2 else 3 end" "2")
(prueba "primitiva resta y multiplicación" "*( -(10, 4), 2)" "12")
(prueba "primitiva módulo" "%(10, 3)" "1")
(prueba "primitivas relacionales" ">(5, 3)" "true")
(prueba "primitivas booleanas" "and(true, not(false))" "true")
(prueba "concatenación de cadenas" "concat(\"hola \", \"mundo\")" "hola mundo")
(prueba "longitud de cadena" "length(\"racket\")" "6")
(prueba "conversión to-string" "to-string(100)" "100")
(prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")

;; --- Pruebas extra Entrega 1: cobertura completa ---

;; Literales restantes (char y unit) y comentarios
(prueba "literal caracter" "'a'" "'a'")
(prueba "literal unit" "unit" "unit")
(prueba "comentarios de linea y bloque"
  "// comentario de linea
   #| comentario de bloque |#
   +(10, 5)"
  "15")

;; Alcance de let (las ligaduras no se ven entre si)
(prueba "let alcance independiente"
  "let x = 10 in let x = 20, y = +(x, 5) in y end end"
  "15")

;; Primitivas faltantes (division entera, flotante, to-number, print) y errores
(prueba "division entera y flotante"
  "+(/(7, 2), to-number(\"2\"))"
  "5")
(prueba "primitiva print devuelve unit"
  "print(\"probando salida\")"
  "unit")
(prueba-rechazo "division por cero" "/(10, 0)")

;; Memoria mutable (ref, deref, set) y casos de error
(prueba "celdas ref, deref y set"
  "let c = ref(10) in begin set c := +(deref(c), 5); deref(c) end end"
  "15")
(prueba-rechazo "deref sobre valor no referencia" "deref(42)")
(prueba-rechazo "set sobre valor no referencia" "set 10 := 20")

;; Ciclo while
(prueba "ciclo while con contador"
  "let i = ref(0), suma = ref(0) in
     begin
       while <(deref(i), 5) do
         begin
           set suma := +(deref(suma), deref(i));
           set i := +(deref(i), 1)
         end
       end;
       deref(suma)
     end
   end"
  "10")

;; Procedimientos y clausuras (alcance lexico)
(prueba "procedimiento con alcance lexico"
  "let x = 10 in
     let f = proc (y: int) +(x, y) end in
       let x = 99 in apply f (5) end
     end
   end"
  "15")

;; Recursión simple y mutua (letrec)
(prueba "letrec recursion mutua par e impar"
  "letrec bool es_par (n: int) = if ==(n, 0) then true else apply es_impar (-(n, 1)) end
          bool es_impar (n: int) = if ==(n, 0) then false else apply es_par (-(n, 1)) end
   in apply es_par (6) end"
  "true")

;; Ida y vuelta de unparse sobre construcciones complejas
(ida-y-vuelta "unparse de proc y let"
  "let f = proc (x: int, y: bool) if y then x else 0 end end in apply f (10, true) end")

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")

;; Agregue aquí las pruebas de la primera entrega: literales de cada clase,
;; let, ref/deref/set, begin, if con elif, while, cada primitiva, proc y
;; apply, letrec con recursión mutua. Por ejemplo:
;;
;; (prueba "let y suma" "let a = 1, b = 2 in +(a, b) end" "3")
;; (prueba "una celda" "let c = ref(0) in begin set c := 5; deref(c) end end" "5")
;; (prueba-rechazo "if con prueba no booleana" "if 1 then 2 else 3 end")

;;; ==========================================================
;;; Segunda entrega: transacciones, registros y tipos
;;; ==========================================================

(eopl:printf "~%== Segunda entrega ==~%")

;; Agregue aquí las pruebas de la segunda entrega: bloques atomic que
;; confirman y que revierten, anidamiento, registros, y al menos cinco
;; programas mal tipados con prueba-rechazo o prueba-tipo. Por ejemplo:
;;
;; (prueba-tipo "tipo de un proc" "proc (x: int) x end" "(int -> int)")
;; (prueba-rechazo "suma de int y bool" "+(1, true)")

;;; ==========================================================
;;; Resumen
;;; ==========================================================

(eopl:printf "~%==========================================~%")
(eopl:printf "  pasadas ~s, fallidas ~s~%" pasadas fallidas)
(eopl:printf "==========================================~%")
