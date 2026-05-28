#lang eopl

;; Valeria Zamudio Arevalo (2415210)
;; Juan Esteban Arias Saldaña (2417915)
;; Link al repositorio: https://github.com/valeturra1/flp-taller3.git


;; Gramatica BNF:

;; Valores denotados: Texto + Número + Booleano + ProcVal
;; Valores expresado: Texto + Número + Booleano + ProcVal


;; <programa> :=  <expresion>
;;               un-programa (exp)


;; <expresion> := <numero>
;;               numero-lit (num)
;;             := "\""<texto> "\""
;;               texto-lit (txt)
;;             := <identificador>
;;               var-exp (id)
;;             := (<expresion> <primitiva-binaria> <expresion>)
;;               primapp-bin-exp (exp1 prim-binaria exp2)
;;             := <primitiva-unaria> (<expresion>)
;;               primapp-un-exp (prim-unaria exp)


;; <primitiva-binaria> :=  + (primitiva-suma)
;;                     :=  ~ (primitiva-resta) 
;;                     :=  / (primitiva-div)
;;                     :=  * (primitiva-multi)
;;                     :=  concat (primitiva-concat)
;;                     := > (primitiva-mayor)
;;                     := < (primitiva-menor)
;;                     := >= (primitiva-mayor-igual)
;;                     := <= (primitiva-menor-igual)
;;                     := != (primitiva-diferente)
;;                     := == (primitiva-comparador-igual)


;; <primitiva-unaria> :=  longitud (primitiva-longitud)
;;                    :=  add1 (primitiva-add1)
;;                    :=  sub1 (primitiva-sub1)
;;                    :=  neg (primitiva-negacion-booleana)



;; Especificación léxica
;; Define los componentes léxicos básicos (tokens) que el analizador reconocerá a partir del texto de entrada.

(define especificacion-lex
'((white-sp
   (whitespace) skip)
  (comentario
   ("%" (arbno (not #\newline))) skip)
  (identificador
   ("@" letter (arbno (or letter digit "?"))) symbol)
  (numero
   (digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit)) number)
  (numero
   (digit (arbno digit) "." digit (arbno digit)) number)
  (numero
   ("-" digit (arbno digit) "." digit (arbno digit)) number)
  (texto
   (letter (arbno (or letter digit "_"))) string)))


;; Especificación sintáctica (gramática)
;; Define las reglas de producción de la gramática y asigna las etiquetas de los datatypes que representarán el árbol de sintaxis abstracta (AST).

(define gramatica
  '((programa (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)
    (expresion ("Si" expresion "{" expresion "}" "sino" "{" expresion "}") condicional-exp)
    (expresion ("declarar" "(" (arbno identificador "=" expresion ";") ")" "{" expresion "}") variableLocal-exp)
    (expresion ("procedimiento" "("(separated-list identificador ",")")" "{" expresion "}") procedimiento-exp)
    (expresion ("declarar-recursivo" (arbno identificador "(" (separated-list identificador ",") ")" "=" expresion )  "en" expresion) 
                recursivo-exp)
    (expresion ("evaluar" expresion "(" (separated-list expresion ",") ")" "finEval") app-exp)

    
    (primitiva-binaria ("+") primitiva-suma)
    (primitiva-binaria ("~") primitiva-resta)
    (primitiva-binaria ("/") primitiva-div)
    (primitiva-binaria ("*") primitiva-multi)
    (primitiva-binaria ("concat") primitiva-concat)
    (primitiva-binaria (">") primitiva-mayor)
    (primitiva-binaria ("<") primitiva-menor)
    (primitiva-binaria (">=") primitiva-mayor-igual)
    (primitiva-binaria ("<=") primitiva-menor-igual)
    (primitiva-binaria ("!=") primitiva-diferente)
    (primitiva-binaria ("==") primitiva-comparador-igual)


    (primitiva-unaria ("longitud") primitiva-longitud)
    (primitiva-unaria ("add1") primitiva-add1)
    (primitiva-unaria ("sub1") primitiva-sub1)
    (primitiva-unaria ("neg") primitiva-negacion-booleana)))

;******************************************************************************************

;; Construidos automáticamente por SLLGeng para manejar las estructuras del AST.

(sllgen:make-define-datatypes especificacion-lex gramatica)

;******************************************************************************************

;; El FrontEnd (Análisis léxico (scanner) y sintáctico (parser) integrados)
;; Toma un string de entrada y genera el árbol de sintaxis abstracta correspondiente según las reglas.
(define scan&parse
  (sllgen:make-string-parser especificacion-lex gramatica))

;; El Interpretador (FrontEnd + Evaluación + señal para lectura )
;; Inicia el bucle REPL para leer expresiones del usuario, evaluarlas y mostrar los resultados.
(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser especificacion-lex gramatica)))

;******************************************************************************************

;El Interprete

;; eval-program: <programa> -> expresion-evaluada
;; Desempaqueta el programa principal y arranca la evaluación de la expresión raíz usando el ambiente inicial.

(define eval-program
  (lambda (pgm)
    (cases programa pgm
      (un-programa (body)
                 (evaluar-expresion body (init-env))))))

;; evaluar-expresion: <expresion> <ambiente> -> valor-expresado
;; Es el motor del intérprete. Recorre el árbol de sintaxis abstracta (AST) de forma recursiva 
;; y computa el resultado de la expresión según el tipo de nodo y el ambiente actual.
(define evaluar-expresion
  (lambda (exp amb)
    (cases expresion exp
      (numero-lit (num) num)
      (texto-lit (txt) txt)
      (var-exp (id) (buscar-variable amb id))
      (primapp-bin-exp (exp1 prim-binaria exp2)
                       (let ((arg1 (evaluar-expresion exp1 amb))
                             (arg2 (evaluar-expresion exp2 amb)))
                         (apply-primitive-bin prim-binaria arg1 arg2)))
      (primapp-un-exp (prim-unaria expr)
                   (let ((arg (evaluar-expresion expr amb)))
                     (apply-primitive-un prim-unaria arg)))

      (condicional-exp (test-exp true-exp false-exp)
                       (let(
                            (base (evaluar-expresion test-exp amb))
                            )
                         (if (valor-verdad? base) (evaluar-expresion true-exp amb) (evaluar-expresion false-exp amb)) 
                         
                       )
      )
                     
      (variableLocal-exp (ids exps cuerpo)
                         (let ((args (eval-exps exps amb)))
                            (evaluar-expresion cuerpo (extend-env ids args amb))))

      (procedimiento-exp (listIDs cuerpo)
                         (cerradura listIDs cuerpo amb))

      (recursivo-exp (nombresFunciones argumentos cuerposFunciones cuerpoRecursivo) 
                 (evaluar-expresion cuerpoRecursivo (extend-env-recursively nombresFunciones argumentos cuerposFunciones amb)))
      
      (app-exp (exp listExps)
               (let ((proc (evaluar-expresion exp amb))
                     (args (eval-exps listExps amb)))
                 (evaluar-proc proc args)))
                         
      )))
      

;; apply-primitive-bin: <primitiva-binaria> <valor> <valor> -> valor
;; Ejecuta las operaciones matemáticas, lógicas o de concatenación que requieren dos operandos.
(define apply-primitive-bin
  (lambda (prim arg1 arg2)
    (cases primitiva-binaria prim
      (primitiva-suma () (+ arg1 arg2))
      (primitiva-resta () (- arg1 arg2))
      (primitiva-div () (/ arg1 arg2))
      (primitiva-multi () (* arg1 arg2))
      (primitiva-concat () (string-append arg1 arg2))
      (primitiva-mayor () (if (> arg1 arg2)
                              1
                              0))
      (primitiva-menor () (if (< arg1 arg2)
                              1
                              0))
      (primitiva-mayor-igual () (if (>= arg1 arg2)
                              1
                              0))
      (primitiva-menor-igual () (if (<= arg1 arg2)
                              1
                              0))
      (primitiva-diferente () (if (not (equal? arg1 arg2))
                              1
                              0))
      (primitiva-comparador-igual () (if (equal? arg1 arg2)
                                     1
                                     0))
      )))

;; apply-primitive-un: <primitiva-unaria> <valor> -> valor
;; Resuelve operaciones unarias operando sobre un único argumento (ej. longitud de strings o incrementos numéricos).
(define apply-primitive-un
  (lambda (prim arg)
    (cases primitiva-unaria prim
      (primitiva-longitud () (string-length arg))
      (primitiva-add1 () (+ arg 1))
      (primitiva-sub1 () (- arg 1))
      (primitiva-negacion-booleana () (if (equal? arg 0)
                                          1
                                          0))
      )))



;; eval-exps: <list-of-expresion> <ambiente> -> list-of-valores
;; Transforma una lista de expresiones sin evaluar en sus valores correspondientes aplicando mapeo recursivo.
(define eval-exps
  (lambda (exps amb)
    (map (lambda (x) (evaluar-expresion x amb)) exps)))


;*******************************************************************************************
;Ambientes

;; Tipo de dato environment
;; Representa los registros de activación en memoria utilizando una estructura jerárquica para soportar alcance estático y recursión.
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record (syms (list-of symbol?))
                       (vals (list-of scheme-value?))
                       (env environment?))
  (recursively-extended-env-record (proc-names (list-of symbol?))
                                  (idss (list-of (list-of symbol?)))
                                  (bodies (list-of expresion?))
                                  (env environment?))
                                                                                                            
  )

;; scheme-value?: -> boolean
;; Predicado utilitario que permite aceptar cualquier tipo de dato nativo de Scheme/Racket dentro de los ambientes.
(define scheme-value? (lambda (v) #t))

;; empty-env: -> environment
;; Constructor que genera un ambiente completamente limpio y sin variables (caso base de la memoria).
(define empty-env  
  (lambda ()
    (empty-env-record)))       ;llamado al constructor de ambiente vacío 


;; extend-env: <list-of-symbols> <list-of-values> <environment> -> environment
;; Añade variables locales vinculando sus identificadores con sus valores en un nuevo marco de ambiente extendido.
(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

;; extend-env-recursively: <list-of-symbols> <list-of-lists-of-symbols> <list-of-expressions> <environment> -> environment
;; Crea un ambiente preparado para recursión asociando nombres de funciones y parámetros sin evaluar sus cuerpos prematuramente.
(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (recursively-extended-env-record proc-names idss bodies old-env)))


;****************************************************************************************
;Funciones Auxiliares

;; list-find-position: <symbol> <list-of-symbols> -> integer o #f
;; Determina el índice numérico de un identificador dentro de una lista de variables para facilitar su posterior extracción.
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;; list-index: <procedure> <list> -> integer o #f
;; Función iterativa que busca qué posición de la lista satisface el predicado enviado por parámetro.
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

;****************************************************************************************

;; Punto 2
;; buscar-variable: <environment> <symbol> -> valor
;; Explora de manera ascendente los ámbitos del ambiente buscando el símbolo solicitado. Si llega al ambiente vacío, arroja error.
(define buscar-variable
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'buscar-variable "Error, la variable no existe"))
      
      (extended-env-record (syms vals old-env)
                           (let ((pos (list-find-position sym syms)))
                             (if (number? pos)
                                 (list-ref vals pos)
                                 (buscar-variable old-env sym))))
      
      (recursively-extended-env-record (proc-names idss bodies old-env)
                                       (let ((pos (list-find-position sym proc-names)))
                                         (if (number? pos)
                                             (cerradura (list-ref idss pos)
                                                        (list-ref bodies pos)
                                                        env)
                                             (buscar-variable old-env sym)))))))

;; init-env: -> environment
;; Genera el ambiente global inicial cargando las constantes requeridas por el enunciado (@a, @b, @c, @d, @e).
(define init-env
  (lambda ()
    (extend-env
     '(@a @b @c @d @e)
     '(1 2 3 "hola" "FLP")
     (empty-env))))


;****************************************************************************************
;;Punto 3

;; valor-verdad?: <valor> -> boolean
;; Regla semántica que mapea el valor numérico 0 como un falso lógico (#f) y cualquier otra cantidad como verdadero (#t).
(define valor-verdad? (lambda (valor)
          (if (equal? valor 0) #f #t)))


;****************************************************************************************

;; Punto 6

;; Tipo de dato para las cerraduras (procVal)
;; Representa los procedimientos encapsulando sus argumentos, cuerpo lógico y el ambiente exacto de su declaración.
(define-datatype procVal procVal?
  (cerradura (lista-ID (list-of symbol?)) (exp expresion?) (amb environment?)))



;****************************************************************************************

;; Punto 7

;; evaluar-proc: <procVal> <list-of-values> -> valor
;; Aplica un procedimiento vinculando los argumentos reales con los formales y evaluando su cuerpo en el ambiente guardado.
(define evaluar-proc
  (lambda (proc args)
    (cases procVal proc
      (cerradura (listaID cuerpo amb)
               (evaluar-expresion cuerpo (extend-env listaID args amb))))))

;****************************************************************************************

;; Punto 9
;; Códigos de los programas escritos en el lenguaje diseñado.

;; a) Procedimiento recursivo para sumar los dígitos de un número entero positivo.

;; declarar-recursivo @div10(@x) = 
;;   Si (@x < 10)
;;    {0}
;;   sino
;;    {add1(evaluar @div10((@x ~ 10)) finEval)}
    
;; @sumarDigitos(@n) = 
;;   Si (@n == 0) 
;;    {0} 
;;   sino 
;;    {((@n ~ (10 * evaluar @div10(@n) finEval)) + evaluar @sumarDigitos(evaluar @div10(@n) finEval) finEval)}
;; en
;; evaluar @sumarDigitos(147) finEval


;; b) Procedimiento recursivo para calcular el factorial de un número n.

;;Prueba con 5

;;declarar-recursivo @factorial (@n) =
;;                   Si (@n==1) {1} sino {
;;                                       (@n * evaluar @factorial ((@n ~ 1)) finEval )}
;;en
;; evaluar @factorial (5) finEval


;;Prueba con 10

;;declarar-recursivo @factorial (@n) =
;;                   Si (@n==1) {1} sino {
;;                                       (@n * evaluar @factorial ((@n ~ 1)) finEval )}
;;en
;; evaluar @factorial (10) finEval      


;; c) Procedimiento recursivo para calcular la potencia de una base elevada a un exponente.

;; declarar-recursivo @potencia(@base, @exponente) =
;;   Si (@exponente == 1)
;;     {@base}
;;   sino
;;     {(@base * evaluar @potencia(@base, (@exponente ~ 1)) finEval)}
;; en
;; evaluar @potencia (4, 2) finEval


;; d) Procedimiento recursivo para sumar los números contenidos en un rango positivo cerrado [a, b].

;;declarar-recursivo @sumaRango (@a, @b) =
;; Si (@a == @b) {@a} sino {(@a + evaluar @sumaRango ((@a+1), @b) finEval)}
;;en
;;evaluar @sumaRango (2,5) finEval


;; e) Implementación de un Decorador simple de funciones en programación funcional.

;; declarar (
;; @integrantes =
;;     procedimiento() {"Valeria_y_Juan"};

;; @saludar =
;;     procedimiento(@funcion){
;;       procedimiento()
;;         {("Hola_" concat evaluar @funcion() finEval)}
;;       };
;; )
;;  {declarar (
;;    @decorate = evaluar @saludar(@integrantes) finEval;
;;  )
;;    {evaluar @decorate() finEval}
;;  }


;; f) Modificación del decorador funcional para aceptar un mensaje dinámico de sufijo al final del string.

;; declarar (
;; @integrantes =
;;     procedimiento() {"Valeria_y_Juan"};

;; @saludar =
;;     procedimiento(@funcion){
;;     procedimiento(@mensaje) {
;;         (("Hola_" concat evaluar @funcion() finEval) concat @mensaje)}
;;       };

;; )
;;  {declarar (
;;    @decorate = evaluar @saludar(@integrantes) finEval;
;;  )
;;    {evaluar @decorate("ProfesoresFLP") finEval}
;;  }



;****************************************************************************************

(interpretador)