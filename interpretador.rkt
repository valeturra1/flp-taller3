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
;;                    := neg (primitiva-negacion-booleana)



;; Especificación léxica

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


;; Gramática

(define gramatica
  '((program (expresion) un-programa)
    (expresion (numero) numero-lit)
    (expresion ("\"" texto "\"") texto-lit)
    (expresion (identificador) var-exp)
    (expresion ("(" expresion primitiva-binaria expresion ")") primapp-bin-exp)
    (expresion (primitiva-unaria "(" expresion ")") primapp-un-exp)

    
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