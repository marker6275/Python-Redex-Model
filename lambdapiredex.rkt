#lang racket
(require redex)

#;(define-language λπ
  (e ::=  x b (λ (x) e) (e e) (op e ...))
  (v ::= x b (λ (x) e))
  (x y z ::= variable-not-otherwise-mentioned)
  (E ::= hole (E e) (v E) (op v ... E e ...))
  (op ::= iszero? add1 sub1 mult expt)
  (b ::= integer)
  #:binding-forms
  (λ (x) e #:refers-to x))

(define-language λπ
  ;;;; Σ - refers to the store
  (Σ ::= ((ref v+undef) ...))
  
  ;;;; ref - refers to pointers
  (ref ::= natural)
  
  ;;;; v, val - refer to values
  (v val ::= (triple val mval (string : ref ...))
     (triple x mval (string : ref ...))
     ref
     (sym string))
  
  (v+undef ::= v skull)
  (e+undef ::= e skull)
  
  ;;;; t - refers to scope
  (t ::= global local)
  
  ;;;; mval - refers to types
  (mval ::= (no-meta) number string meta-none
        (list val ...) (tuple val ...) (set val ...)
        (meta-class x)
        (meta-code (x ...) x e)
        (λ (x ...) opt-var.e))
  (opt-var ::= (x) (no-var))
  
  ;;;; e - refers to expressions
  (e ::= v
     ref
     (fetch e)
     (set! e e)
     (alloc e)
     (list-ref e e) ;; e[e]
     (list-assign e e e) ;; e[e := e]
     (if e e e) (e e)
     (let x e+undef e)
     x (assign e e) (delete e)
     (e (e ...)) (e (e ...) e) (frame e) (return e)
     (while e e e) (loop e e) break continue
     (builtin-prim op (e ...))
     (fun (x ...) opt-var e)
     (obj-type e mval) (list e (e ...))
     (tuple e (e ...)) (set e (e ...))
     (tryexcept e x e e) (tryfinally e e)
     (raise e) (err val)
     (module e e) (construct-module e)
     (in-module e ϵ))

  ;;;; x, y, z - refer to arbitrary variables
  (x y z ::= variable-not-otherwise-mentioned)
  ;; "binding forms"?
  )

;; How much of these special form names (fetch, set!, allloc, if, let, frame,
;; return, while, loop, etc) do we need to define? I assume all of them, but I want to
;; double check :)

;; We need to define triple, list, tuple, and set.
(default-language λπ)

(define-metafunction λπ
  alloc : e+undef Σ -> Σ
  [(alloc e+undef ()) ((0 e+undef))]
  [(alloc e+undef_1 ((ref v+undef_2) ...)) ((,(length (term ((ref v+undef_2) ...))) e+undef_1) (ref v+undef_2) ...)])
                      
(define -->PythonRR
  (reduction-relation
   λπ
   [--> ((let x e+undef e) Σ)
        (e (alloc e+undef Σ))
        E-LetLocal]
   [--> (ref ((ref_2 v+undef_1) ... (ref v+undef_2) (ref_3 v+undef_3) ...))
        (v+undef_2 ((ref_2 v+undef_1) ... (ref v+undef_2) (ref_3 v+undef_3) ...))
        E-GetVar]))
;; List of things we need to define in the language or as a metafunction:
;; triple
;; sym
;; skull
;; global
;; local
;; no-meta
;; meta-none
;; list -- meta edition
;; tuple -- meta edition
;; set -- meta edition
;; meta-class
;; meta-code
;; no-var
;; fetch
;; set!
;; alloc
;; if
;; let
;; in
;; delete
;; *
;; frame
;; return
;; while
;; loop
;; break
;; continue
;; builtin-prim
;; op -- probably just copy from lecture?
;; fun
;; cons/pair
;; list
;; tuple
;; set
;; tryexcept
;; tryfinally
;; raise
;; err
;; module
;; construct-module
;; in-module

;; Reduction Rules