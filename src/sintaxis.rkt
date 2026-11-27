#lang eopl
;Autores: Adriana Milena Noscue Dagua 2477336, Sebastian Cucalon Astorquiza 2477344
;Santiago Torres Rojas 2380301, Nicolle Camila Hoyos Puin 2380608

(provide (all-defined-out))

;; 1. ESPECIFICACIÓN LÉXICA (Scanner)
(define especificacion-lexica
  '(
    ;; Espacios en blanco
    (whitespace (whitespace) skip)
    
    ;; Comentario de línea (desde // hasta fin de línea)
    (comment ("//" (arbno (not #\newline))) skip)
    ;; Comentario de bloque (#| ... |#) que no admite '|' adentro según la especificación
    (comment ("#|" (arbno (not #\|)) "|#") skip)
    
        ;; Identificadores y palabras reservadas
    (identificador (letter (arbno (or letter digit "_" "?"))) symbol)

    ;; Literales
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (flotante (digit (arbno digit) "." digit (arbno digit)) number)
    (flotante ("-" digit (arbno digit) "." digit (arbno digit)) number)
    (cadena ("\"" (arbno (not #\")) "\"") string)
    (caracter ("'" (not #\') "'") symbol)
   )
)

;; 2. GRAMÁTICA Y AST (Parser)
(define gramatica-txlang
  '(
    ;; Programa principal
    (programa ((arbno decl-registro) expresion) un-programa)

    ;; Declaración de registros
    (decl-registro ("record" identificador "{" (separated-list identificador ":" tipo ",") "}") una-decl-registro)

    ;; Tipos de datos
    (tipo ("int") int-type)
    (tipo ("float") float-type)
    (tipo ("bool") bool-type)
    (tipo ("string") string-type)
    (tipo ("char") char-type)
    (tipo ("unit") unit-type)
    (tipo ("ref" "(" tipo ")") ref-type)
    (tipo ("(" (separated-list tipo ",") "->" tipo ")") proc-type)
    (tipo (identificador) record-type)

    ;; Expresiones principales
    (expresion (numero) num-exp)
    (expresion (flotante) float-exp)
    (expresion (cadena) string-exp)
    (expresion (caracter) char-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion ("unit") unit-exp)
    (expresion (identificador) var-exp)

    ;; Ligaduras locales (let)
    (expresion ("let" (separated-list identificador "=" expresion ",") "in" expresion "end") let-exp)

    ;; Referencias y memoria mutable
    (expresion ("ref" "(" expresion ")") ref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("set" expresion ":=" expresion) set-exp)

    ;; Secuencia y control de flujo
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)
    (expresion ("if" expresion "then" expresion (arbno "elif" expresion "then" expresion) "else" expresion "end") if-exp)
    (expresion ("while" expresion "do" expresion "end") while-exp)

    ;; Primitivas
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)

    ;; Procedimientos y recursión
    (expresion ("proc" "(" (separated-list identificador ":" tipo ",") ")" expresion "end") proc-exp)
    (expresion ("apply" expresion "(" (separated-list expresion ",") ")") apply-exp)
    (expresion ("letrec" (arbno tipo identificador "(" (separated-list identificador ":" tipo ",") ")" "=" expresion) "in" expresion "end") letrec-exp)

    ;; Transacciones
    (expresion ("atomic" expresion "onfail" "(" identificador ")" expresion "end") atomic-exp)
    (expresion ("abort" "(" expresion ")") abort-exp)

    ;; Registros
    (expresion ("new" identificador "{" (separated-list identificador "=" expresion ",") "}") new-record-exp)
    (expresion ("field" "(" expresion "," identificador ")") field-exp)

    ;; Operadores primitivos
    (primitiva ("+") add-prim)
    (primitiva ("-") sub-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("%") mod-prim)
    (primitiva (">") gt-prim)
    (primitiva ("<") lt-prim)
    (primitiva (">=") geq-prim)
    (primitiva ("<=") leq-prim)
    (primitiva ("==") eq-prim)
    (primitiva ("!=") diff-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("not") not-prim)
    (primitiva ("concat") concat-prim)
    (primitiva ("length") length-prim)
    (primitiva ("to-string") to-string-prim)
    (primitiva ("to-number") to-number-prim)
    (primitiva ("print") print-prim)
   )
)

;; 3. CONSTRUCCIÓN DE SLLGEN (Datatypes y Parser)
(sllgen:make-define-datatypes especificacion-lexica gramatica-txlang)

(define show-datatypes
  (lambda () 
    (sllgen:list-define-datatypes especificacion-lexica gramatica-txlang)))

;; Construcción de la función scan&parse
(define scan&parse
  (sllgen:make-string-parser especificacion-lexica gramatica-txlang))