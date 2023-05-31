#lang racket
(require redex)

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

  ;;;; nv - refers to a new variable reference (ref) and an updated store
  (nv ::= (ref Σ))
  (vs ::= (v+undef Σ))
  (es ::= (e Σ)))

;; We need to define triple, list, tuple, and set.
(default-language λπ)

;; alloc!: takes a value and a store and returns a reference to that value in the store
;; alongside the new store.
(define-metafunction λπ
  alloc! : v+undef Σ -> nv
  [(alloc! v+undef ()) (0 ((0 v+undef)))]
  [(alloc! v+undef_1 ((ref v+undef_2) ...))
   ((store-length ((ref v+undef_2) ...))
    (((store-length ((ref v+undef_2) ...)) v+undef_1) (ref v+undef_2) ...))]
  )

(define-metafunction λπ
  local-substitute : e x nv -> es
  [(local-substitute e x nv)
   ((substitute e (x (get-ref nv))) (get-store nv))])

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
  class-lookup : ref v+undef string Σ -> vs
  [(class-lookup ref
                 (triple val mval (dict (string_1 ref_1) ...
                                      ("__mro__" ref_2)
                                      (string_2 ref_3) ...))
                 string
                 Σ)
   ((class-lookup-mro (get-mval (get (get ref_2 Σ) Σ)) string Σ) Σ)])

(define-metafunction λπ
  get-mval : v+undef -> mval
  [(get-mval (triple val mval (dict (string ref) ...))) mval])

(define-metafunction λπ
  class-lookup-mro : mval string Σ -> v+undef
  [(class-lookup-mro (list ref val_1 ...)
                     string
                     ((ref_2 v+undef_1) ...
                      (ref (triple val_2 mval_1 (dict (string_1 ref_3) ...
                                                      (string ref_1)
                                                      (string_2 ref_4) ...)))
                      (ref_5 v+undef_2) ...))
   ref_1]
  [(class-lookup-mro (list ref val_1 ...)
                     string
                     ((ref_2 v+undef_1) ...
                      (ref (triple val_2 mval_1 (dict (string_1 ref_3) ...)))
                      (ref_4 v+undef_2) ....))
   (class-lookup-mro (list val_1 ...)
                     string
                     ((ref_2 v+undef_1) ...
                      (ref (triple val_2 mval_1 (dict (string_1 ref_3) ...)))
                      (ref_4 v+undef_2) ...))
   (side-condition (not (member (term string) (term (string_1 ...)))))])
                                   

(define -->PythonRR
  (reduction-relation
   λπ
   ;; Figure 2
   [--> ((let x v+undef e) Σ)
        (local-substitute e x (alloc! v+undef Σ))
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
   [--> ((list-ref ref_4 ref_2)
         ((ref_1 v+undef_1) ...
          (ref_2 (triple val string (dict (string_1 ref_6) ...)))
          (ref_3 v+undef_2) ...
          (ref_4 (triple ref mval (dict (string_2 ref_7) ...)))
          (ref_5 v+undef_3) ...))
        (class-lookup ref_4
                      (get ref ((ref_1 v+undef_1) ...
                                (ref_2 (triple val string (dict (string_1 ref_6) ...)))
                                (ref_3 v+undef_2) ...
                                (ref_4 (triple ref mval (dict (string_2 ref_7) ...)))
                                (ref_5 v+undef_3) ...))
                      string
                      ((ref_1 v+undef_1) ...
                       (ref_2 (triple val string (dict (string_1 ref_6) ...)))
                       (ref_3 v+undef_2) ...
                       (ref_4 (triple ref mval (dict (string_2 ref_7) ...)))
                       (ref_5 v+undef_3) ...))
        (side-condition (not (member (term string) (term (string_2 ...)))))
        E-GetFieldClass]))

(begin
  (traces -->PythonRR (term ((let x 0 (fetch x)) ((0 122))))))
;; traces for figure 3
#;(traces -->PythonRR [(term ((set 12 (list 12 13 14 15)) ()))]
        [(traces -->PythonRR (term ((tuple 12 (list 12 13 14 15)) ())))]
        [(traces -->PythonRR (term ((obj-type 4 "str") ())))])

#;(begin
  (traces -->PythonRR (term ((set 12 (list 12 13 14 15)) ())))
  (traces -->PythonRR (term ((tuple 12 (list 12 13 14 15)) ())))
  (traces -->PythonRR (term ((obj-type 4 "str") ()))))

#;(traces -->PythonRR (term ((let x 3 x)
                           ())))
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
#;(traces -->PythonRR (term ((list-ref 1 2)
                           ((2 (triple 0 "str" (dict)))
                            (1 (triple 3 "na" (dict)))
                            (3 (triple 4 "na" (dict ("__mro__" 4))))
                            (4 5)
                            (5 (triple 0 (list 0 6) (dict)))
                            (6 (triple 0 "na" (dict ("str" 7))))
                            (0 (triple 0 "na" (dict)))))))
;; (class-lookup 1 (triple 4 mval (dict)) string Σ)

#;(traces -->PythonRR
        (term
         ((fetch 3)
          ,(second
            (first
             (apply-reduction-relation -->PythonRR
                                       (term
                                        ((set! 2 (triple 4 "str" (dict)))
                                         ((3 14) (2 45) (1 13) (0 12))))))))))
#;(traces -->PythonRR
        (term ((alloc 44)
              ,(second
                (first
                 (apply-reduction-relation -->PythonRR
                                           (term
                                            ((fetch 3)
                                             ,(second
                                               (first
                                                (apply-reduction-relation -->PythonRR
                                                                          (term
                                                                           ((set! 2 (triple 4 "str" (dict)))
                                                                            ((3 14) (2 45) (1 13) (0 12)))))))))))))))

;(traces -->PythonRR (term ((alloc 12) ((0 4) (1 55) (2 34)))))
;(traces -->PythonRR (term ((let x 3 x) ())))
;(traces -->PythonRR (term (2 ((3 1) (2 skull) (1 15) (0 60)))))

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