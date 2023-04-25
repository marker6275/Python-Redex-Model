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
  (Σ ::= ((ref v+undef) ...))
  (ref ::= natural)
  (v val ::= (triple val mval (string : ref ...)))
  (e ::= v ref (fetch e) (set! e e) (alloc e)
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