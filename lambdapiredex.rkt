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
  (v val ::= (triple val mval (dict (string ref) ...))
     (triple x mval (dict (string ref) ...))
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
     #; (alloc e)
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

  ;;;; nv - refers to a new variable reference (ref) and an updated store
  (nv ::= (ref Σ))
  (vs ::= (v+undef Σ))
  )

(define-extended-language RS-λπ
  λπ
  (e ::= .... error)
  (v ::= b (λ (x) e))
  (c ::= (e ρ))
  (ρ ::= ((x c) ...))
  (vc ::= (v ρ))
  (κ ::= mt (fn vc κ) (arg c κ) (prim (vc ... op) (c ...) κ) (handler b ((λ (x) e) ρ) κ))
  (κκ ::= (fn vc κ) (arg c κ) (prim (vc ... op) (c ...) κ))
  (s ::= (c κ)))

;; We need to define triple, list, tuple, and set.
(default-language λπ)

;; alloc!: takes a value and a store and returns a reference to that value in the store
;; alongside the new store.
(define-metafunction λπ
  alloc! : v+undef Σ -> nv
  [(alloc! v+undef ()) (0 ((0 v+undef)))]
  [(alloc! v+undef_1 ((ref v+undef_2) ...))
   ((store-length ((ref v+undef_2) ...))
    (((store-length((ref v+undef_2) ...)) v+undef_1) (ref v+undef_2) ...))]
  )

;; get: takes a ref and a store and returns the value referenced by ref in the store.
(define-metafunction λπ
  get : ref Σ -> v+undef
  [(get ref ()) ((raise (triple "Uninitialized Global" str (dict))))]
  [(get ref ((ref v+undef_1) (ref_2 v+undef_2) ...)) v+undef_1]
  [(get ref ((ref_1 v+undef_1) (ref_2 v+undef_2) ...)) (get ref ((ref_2 v+undef_2) ...))])

;; update: takes a ref, a val, and a store and returns the store with the provided ref
;; updated to val.
(define-metafunction λπ
  update : ref val Σ -> Σ
  [(update ref val ()) ((raise (triple "Uninitialized Global" str (dict))))]
  [(update ref val ((ref val_1) (ref_1 val_2) ...)) ((ref val) (ref_1 val_2) ...)]
  [(update ref val ((ref_1 val_1) (ref_2 val_2) ...)) ((ref_1 val_1) (ref_3 val_3) ...)
                                                      (where ((ref_3 val_3) ...) (update ref val ((ref_2 val_2) ...)))])
(define-metafunction λπ
  let-helper : e (ref Σ) -> (e Σ)
  [(let-helper e (ref Σ))
   (e Σ)])

(define-metafunction λπ
  get-store : nv -> Σ
  [(get-store (ref Σ)) Σ])

(define-metafunction λπ
  get-ref : nv -> ref
  [(get-ref (ref Σ)) ref])

(define-metafunction λπ
  store-length : Σ -> ref
  [(store-length Σ)
   ,(length (term Σ))])

(define-metafunction λπ
  add-field : nv string ref -> Σ
  [(add-field (ref_1
               ((ref_3 v+undef_1) ...
                (ref_2 (triple x mval (dict (string_1 ref_4) ...)))
                (ref_5 v+undef_2) ...
                )) string_2 ref_2)
   ((ref_3 v+undef_1) ...
    (ref_2 (triple x mval (dict (string_2 ref_1) (string_1 ref_4) ...)))
    (ref_5 v+undef_2) ...)])

(define-metafunction λπ
  get-pair : ref Σ -> vs
  [(get-pair ref Σ) ((get ref Σ) Σ)])

(define-metafunction λπ
  class-lookup : r1 ref string Σ -> vs
  [(class-lookup r1 ref string Σ) (class-lookup-mro (get-mval (get ref Σ)) string Σ)])

(define-metafunction λπ
  get-mval : v+undef -> mval
  [(get-mval (triple x mval (dict (string ref) ...))) mval])

(define-metafunction λπ
  class-lookup-mro : mval string Σ -> v+undef
  [(class-lookup-mro (list val_1 val_2 ...) string Σ) 

(define -->PythonRR
  (reduction-relation
   λπ
   ;; Figure 2
   [--> ((let x v+undef e) Σ)
        (e (get-ref (alloc! v+undef Σ)))
        E-LetLocal]
   [--> (ref ((ref_2 v+undef_1) ... (ref val) (ref_3 v+undef_3) ...))
        (val ((ref_2 v+undef_1) ... (ref val) (ref_3 v+undef_3) ...))
        E-GetVar]
   [--> (ref ((ref_2 v+undef_1) ... (ref skull) (ref_3 v+undef_3) ...))
        ((raise (triple "Uninitialized Local" str (dict)) ((ref_2 v+undef_1) ... (ref skull) (ref_3 v+undef_3) ...)))
        E-GetVarUndef]
   
   ;; Figure 3
   [--> ((obj-type val mval) Σ)
        (alloc! (triple val mval (dict)) Σ)
        E-Object]
   [--> ((tuple e_1 (list e_2 ...)) Σ)
        (alloc! (triple e_1 (tuple e_2 ...) (dict)) Σ)
        E-Tuple]
   [--> ((set e_1 (list e_2 ...)) Σ)
        (alloc! (triple e_1 (set e_2 ...) (dict)) Σ)
        E-Set]
   
   ;; Figure 5
   [--> ((fetch ref) Σ)
        ((get ref Σ) Σ)
        E-Fetch]
   [--> ((set! ref val) Σ)
        (val (update ref val Σ))
        E-Set!]
   [--> ((alloc val) Σ)
        (alloc! val Σ)
        E-Alloc]
   
   ;; Figure 6
   [--> ((list-assign ref_4 ref_5 val)
         ((ref_6 v+undef_1) ...
          (ref_5 (triple y string_1 (dict (string ref) ...)))
          (ref_7 v+undef_2) ...
          (ref_4 (triple x mval (dict (string_2 ref_2) ... (string_1 ref_1) (string_3 ref_3) ...)))
          (ref_8 v+undef_3) ...))
        (val (update ref_1 val
                     ((ref_6 v+undef_1) ...
                      (ref_4 (triple x mval (dict (string_2 ref_2) ... (string_1 ref_1) (string_3 ref_3) ...)))
                      (ref_7 v+undef_2) ...
                      (ref_5 (triple y string_1 (dict (string ref) ...)))
                      (ref_8 v+undef_3) ... )))
        E-SetFieldUpdate]
   [--> ((list-assign ref_4 ref_5 val)
         ((ref_6 v+undef_1) ...
          (ref_5 (triple y string_1 (dict (string ref) ...)))
          (ref_7 v+undef_2) ...
          (ref_4 (triple x mval (dict (string_2 ref_2) ...)))
          (ref_8 v+undef_3) ...))
        ;; Where condition that (string_2 ref_2) doesn't have string_1 as a member?
        (val (add-field
              (alloc! val
                      ((ref_6 v+undef_1) ...
                       (ref_5 (triple y string_1 (dict (string ref) ...)))
                       (ref_7 v+undef_2) ...
                       (ref_4 (triple x mval (dict (string_2 ref_2) ...)))
                       (ref_8 v+undef_3) ...))
              string_1
              ref_4))
        (side-condition (not (member (term string_1) (term (string_2 ...)))))
        E-SetFieldAdd]
   [--> ((list-ref ref_1 ref_2)
         ((ref_3 v+undef_3) ...
          (ref_2 (triple x string_1 (dict (string ref) ...)))
          (ref_6 v+undef_4) ...
          (ref_1 (triple y mval (dict (string_2 ref_7) ... (string_1 ref_4) (string_3 ref_5) ...)))
          (ref_8 v+undef_8) ...))
        (get-pair ref_4
                  ((ref_3 v+undef_3) ...
                   (ref_2 (triple x string_1 (dict (string ref) ...)))
                   (ref_6 v+undef_4) ...
                   (ref_1 (triple y mval (dict (string_2 ref_7) ... (string_1 ref_4) (string_3 ref_5) ...)))
                   (ref_8 v+undef_8) ...))
        E-GetField]
   ;; Figure 7
   [--> ((list-ref r1 r2)
         ((ref_1 v+undef_1) ...
          (r2 (triple x string (dict (string_10 ref_10) ...)))
          (ref_20 v+undef) ...
          (r1 (triple ref mval (dict (string_1 ref_2) ...)))
          (ref_30 v+undef) ...))
        (class-lookup r1
                      (get ref ((ref_1 v+undef_1) ...
                                (r2 (triple x string (dict (string_10 ref_10) ...)))
                                (ref_20 v+undef) ...
                                (r1 (triple ref mval (dict (string_1 ref_2) ...)))
                                (ref_30 v+undef) ...))
                      string
                      ((ref_1 v+undef_1) ...
                       (r2 (triple x string (dict (string_10 ref_10) ...)))
                       (ref_20 v+undef) ...
                       (r1 (triple ref mval (dict (string_1 ref_2) ...)))
                       (ref_30 v+undef) ...))
        (side-condition (not (member (term string) (term (string_1 ...)))))]))
        

#;(traces -->PythonRR (term ((list-assign 0 1 3)
                          ((1 (triple x "str" (dict)))
                           (0 (triple x "num" (dict)))))))
#;(traces -->PythonRR (term ((list-assign 1 2 3)
                          ((2 (triple x "str" (dict)))
                           (1 (triple x "num" (dict ("str" 8))))
                           (8 0)))))
#;(traces -->PythonRR (term ((list-ref 2 3)
                           ((1 12)
                            (3 (triple x "str" (dict)))
                            (2 (triple x "num" (dict ("str" 8))))
                            (8 0)))))
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