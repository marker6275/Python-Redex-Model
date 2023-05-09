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
     (sym string)
     ;; What is (sym string) meant to represent? An error perhaps?
     )
  
  (v+undef ::= v skull)
  ;; undef? What's that? Is this syntax of "v+undef" cool?
  (e+undef ::= e skull)
  
  ;;;; t - refers to scope
  (t ::= global local)
  
  ;;;; mval - refers to types
  (mval ::= (no-meta) number string meta-none
        (list val ...) (tuple val ...) (set val ...)
        (meta-class x)
        (meta-code (x ...) x e)
        (λ (x ...) opt-var.e))
  
  ;; What is this period doing in this mval definition?
  
  ;; General question: how much of this paper's syntax can we copy word for word,
  ;; especially when the group of us don't understand exactly what it's doing?
  ;; Also, what is opt-var?
  (opt-var ::= (x) (no-var))
  ;; Why does x have parentheses around it here?

  ;;;; e - refers to expressions
  (e ::= v
     ref
     (fetch e)
     (set! e e)
     (alloc e)
     e[e] e[e := e]
     (if e e e) (e e)
     (let x = e+undef in e)
     x (e := e) (delete e)
     (e (e ...)) (e (e ...)*e) (frame e) (return e)
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
;; We're getting an error that reads as follows:
;; lambdapiredex.rkt:36:3: define-language: the non-terminal e is defined in terms of itself
;; at: e
;; in: (define-language λπ (Σ ::= ((ref v+undef) ...)) (ref ::= natural) (v val ::= (triple val mval (string : ref ...)) (triple x mval (string : ref ...)) ref) (v+undef ::= v skull) (e+undef ::= e skull) (t ::= global local) (mval ::= (no-meta) number s...
;; #(1195 1)
;; ^^ How should we handle this? The ISWIM syntax self-references all the time.

(default-language λπ)

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
;; they don't actually talk much about reduction rules for the basic function calls, etc.
;; so how should we approach translating it?