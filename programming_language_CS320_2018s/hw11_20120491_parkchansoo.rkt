#lang plai-typed

;;; our grammar given by instructor
;; <EXPR> ::= <num>
;;          | true
;;          | false
;;          | {+ <EXPR> <EXPR>}
;;          | {- <EXPR> <EXPR>}
;;          | {= <EXPR> <EXPR>}            // equality test between numbers
;;          | <id>
;;          | {fun {<id> : <TE>} <EXPR>}
;;          | {<EXPR> <EXPR>}
;;          | {if <EXPR> <EXPR> <EXPR>}
;;          | {rec {<id> : <TE> <EXPR>} <EXPR>}
;;          | {withtype {<tyid> {<id> <TE>} {<id> <TE>}} <EXPR>}
;;          | {cases <tyid> <EXPR> {<id> {<id>} <EXPR>} {<id> {<id>} <EXPR>}}
;;          | [tyfun [<tyid>] <EXPR>]
;;          | [@ <EXPR> <TE>]
;;         
;; <TE> ::= num
;;        | bool
;;        | (<TE> -> <TE>)
;;        | <tyid>
;;        | (forall <tyid> <TE>)


;; our AST which given by instructor
; s-expressions
(define-type EXPR
  [num (n : number)]
  [bool (b : boolean)]
  [add (lhs : EXPR) (rhs : EXPR)]
  [sub (lhs : EXPR) (rhs : EXPR)]
  [equ (lhs : EXPR) (rhs : EXPR)]
  [id (name : symbol)]
  [fun (param : symbol) (paramty : TE) (body : EXPR)]
  [app (fun-expr : EXPR) (arg-expr : EXPR)]
  [ifthenelse (test-expr : EXPR) (then-expr : EXPR) (else-expr : EXPR)]
  [rec (name : symbol) (ty : TE) (named-expr : EXPR) (body : EXPR)]
  [with-type (name : symbol)
             (var1-name : symbol) (var1-ty : TE)
             (var2-name : symbol) (var2-ty : TE)
             (body-expr : EXPR)]
  [cases (name : symbol)
         (dispatch-expr : EXPR)
         (var1-name : symbol) (bind1-name : symbol) (rhs1-expr : EXPR)
         (var2-name : symbol) (bind2-name : symbol) (rhs2-expr : EXPR)]
  [tfun (name : symbol) (expr : EXPR)]
  [tapp (body : EXPR) (type : TE)])

; type expressions
(define-type TE
  [numTE]
  [boolTE]
  [arrowTE (param : TE) (result : TE)]
  [polyTE (forall : symbol) (body : TE)]
  [idTE (name : symbol)]
  [tvTE (name : symbol)])

; Type definition
(define-type Type
  [numT]
  [boolT]
  [arrowT (param : Type) (result : Type)]
  [polyT (forall : symbol) (body : Type)]
  [idT (name : symbol)]
  [tvT (name : symbol)])

; Expresion value AST
(define-type EXPR-Value
  [numV (n : number)]
  [boolV (b : boolean)]
  [closureV (param : symbol) (body : EXPR) (ds : DefrdSub)]
  [variantV (right? : boolean) (val : EXPR-Value)]
  [constructorV (right? : boolean)])

; our type environment
(define-type TypeEnv
  [mtEnv]
  [aBind (name : symbol)
         (type : Type)
         (rest : TypeEnv)]
  [tBind (name : symbol)
         (var1-name : symbol) (var1-type : Type)
         (var2-name : symbol) (var2-type : Type)
         (rest : TypeEnv)])


;____________________________________________________________________________________________________
;; add some ASTs for helping interp

; defrdSub syntax-tree
;; deffered substitution get and contains BWAE-value with matched ids
;; mostly value contains address(boxV) but somtimes get parameter with argument(appParamV)
(define-type DefrdSub
  [mtSub]
  [aSub (namd : symbol) (value : EXPR-Value) (rest : DefrdSub)]
  [aRecSub (named : symbol) (value : (boxof EXPR-Value)) (rest : DefrdSub)])

(define-type Pbind
  [mtP]
  [aP (name : symbol) (rest : Pbind)])


;____________________________________________________________________________________________________

;; do parsing user given s-expressions and types expressions

;(define (parse sexpr)
;  (match sexpr
;    [(list '+ l r) (add (parse l) (parse r))]
;    [(list '- l r) (sub (parse l) (parse r))]
;    [(list '= l r) (equ (parse l) (parse r))]
 ;   [(list 'fun p pty body) (fun (parse p) (parse-type pty) (parse body))]
 ;;   [(list 'if test-e then-e else-e) (if (parse test-e) (parse then-e) (parse else-e))]
 ;   [(list 'rec id ty named-e body-e) (rec (parse id) (parse-type te) (parse named-e) (parse body-e))]
 ;   [(list 'withtype tyid id1 ty1 id2 ty2 body) (withype (parse-type tyid) (parse id1) (parse-type id


;______________________________________________________________________________________________________
;; interp with helper functions
;; we have helper functions as....
;; bin-op, lookup, 

;; interp: EXPR DefrdSub -> EXPR-Value
(define (interp expr ds)
  (type-case EXPR expr
    [num (n) (numV n)]
    [bool (b) (boolV b)]
    [add (l r) (numV (num+ (interp l ds) (interp r ds)))]
    [sub (l r) (numV (num- (interp l ds) (interp r ds)))]
    [equ (l r) (boolV (num= (interp l ds) (interp r ds)))]
    [id (name) (lookup name ds)]
    [fun (param ty body) (closureV param body ds)]
    [app (fun arg) (local [(define fun-val (interp fun ds))
                              (define arg-val (interp arg ds))]
                        (type-case EXPR-Value fun-val
                          [closureV (param body ds)
                                    (interp body (aSub param arg-val ds))]
                          [constructorV (right?)
                                        (variantV right? arg-val)]
                          [else (error 'interp "not applicable")]))]
    [ifthenelse (test-e then-e else-e)
                (local [(define test-v (interp test-e ds))]
                  (type-case EXPR-Value test-v
                    [boolV (b) (if b (interp then-e ds) (interp else-e ds))]
                    [else (error 'interp (string-append "ifthenelse get wrong boolV ~a" (to-string test-v)))]))]
    [rec (name ty name-e body-e)
         (local [(define value-holder (box (numV 42)))
                 (define new-ds
                         (aRecSub name value-holder ds))]
           (begin
             (set-box! value-holder (interp name-e new-ds))
             (interp body-e new-ds)))]
    [with-type (name var1-name var1-ty
                     var2-name var2-ty body)
      (interp body (aSub var1-name
                         (constructorV false)
                         (aSub var2-name
                               (constructorV true)
                               ds)))]
    [cases (name dispatch-expr
                 var1-id bind1-name var1-rhs
                 var2-id bind2-name var2-rhs)
      (type-case EXPR-Value (interp dispatch-expr ds)
        [variantV (right? val)
                  (if (not right?)
                      (interp var1-rhs (aSub var1-id val ds))
                      (interp var2-rhs (aSub var2-id val ds)))]
        [else (error 'interp "not a variant result")])]
    [tfun (name body) (interp body ds)]      ;ToDo. check the type is right
    [tapp (body ty) (interp body ds)]        ;ToDo.
    ))


; bin-op : opertation, numV, numV -> numV
;; get two numV and do operations. add or sub
(define (bin-op op l r)
  (cond
    [(numV? l)
     (cond
       [(numV? r) (op (numV-n l) (numV-n r))]
       [else (error 'bin-op (string-append "get wrong input" (to-string r)))])]
    [else (error 'bin-op (string-append "get wrong input" (to-string l)))]))
(define (num+ l r)
  (bin-op + l r))
(define (num- l r)
  (bin-op - l r))
(define (num= l r)
  (bin-op = l r))

; lookup : id defrdSub -> EXPR-Value
(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub() (error 'lookup (string-append "free indentifier ~a" (to-string name)))]
    [aSub (sub-name val rest)
          (if (symbol=? name sub-name) val (lookup name rest))]
    [aRecSub (sub-name val rest)
             (if (symbol=? name sub-name) (unbox val) (lookup name rest))]))


;______________________________________________________________________________________________________
;; typecheck with helper functions
;; we have helper functions as...
;;; validtype, get-type, find-type-id,

;; validtype: TE -> boolean
(define (validtype ty env)
  (type-case Type ty
    [numT () (mtEnv)]
    [boolT () (mtEnv)]
    [arrowT (a b) (begin (validtype a env)
                         (validtype b env))]
    [idT (id) (find-type-id id env)]
    [tvT (id) (mtEnv)]
    [polyT (id body) (mtEnv)]))

;; get-type: symbol TypeEnv -> Type
(define (get-type name-to-find env)
  (type-case TypeEnv env
    [mtEnv () (error 'get-type "free variable, no type")]
    [aBind (name ty rest)
           (if (symbol=? name-to-find name)
               ty
               (get-type name-to-find rest))]
    [tBind (name var1-name var1-ty var2-name var2-ty rest)
           (get-type name-to-find rest)]
    ))

;; find-type-id: symbol TypeEnv -> TypeEnv
(define (find-type-id name-to-find env)
  (type-case TypeEnv env
    [mtEnv () (error 'find-type-id "free type name, no type")]
    [aBind (name ty rest)
           (find-type-id name-to-find rest)]
    [tBind (name var1-name var1-ty var2-name var2-ty rest)
           (if (symbol=? name-to-find name)
               env
               (find-type-id name-to-find rest))]
    ))

;; parse-type: TE TypeEnv -> Type
(define (parse-type te env)
  (type-case TE te
    [numTE () (numT)]
    [boolTE () (boolT)]
    [arrowTE (param result) (arrowT (parse-type param env) (parse-type result env))]
    [polyTE (forall body) (polyT forall (parse-type body env))]
    [idTE (name) (idT name)]
    [tvTE (name) (tvT name)]
    ))

(define (compare-type t1 t2)
  (type-case Type t1
    [arrowT (a b) (type-case Type t2
                    [arrowT (a2 b2) (and (compare-type a a2) (compare-type b b2))]
                    [else #f])]
    [polyT (s1 b1)
           (type-case Type t2
             [polyT (s2 b2) (equal? t1 (repPoly t2 s1))]
             [else #f])]
    [else (equal? t1 t2)]))

(define (repPoly poly alpha)
  (type-case Type poly
    [arrowT (a b) (arrowT (repPoly a alpha) (repPoly b alpha))]
    [polyT (s1 b1) (polyT alpha (repPoly b1 alpha))]
    [tvT (s1) (tvT alpha)]
    [else poly]))

(define (type-error expr msg)
  (error 'typecheck (string-append
                     "no type: "
                     (string-append
                      (to-string expr)
                      (string-append " not "
                                     msg)))))

(define (foralltv t-body alpha-val ty)
  (type-case Type t-body
    [arrowT (a b) (arrowT (foralltv a alpha-val ty) (foralltv b alpha-val ty))]
    [tvT (ty1) (if (symbol=? ty1 alpha-val) ty t-body)]
    [else t-body]))
                                

     
;; typecheck-do: EXPR TypeEnv -> Type
(define typecheck-do : (EXPR TypeEnv -> Type)
  (lambda (expr env)
    (type-case EXPR expr
      [num (n) (numT)]
      [bool (b) (boolT)]
      [add (l r) (type-case Type (typecheck-do l env)
                   [numT ()
                         (type-case Type (typecheck-do r env)
                           [numT () (numT)]
                           [else (type-error r "num")])]
                   [else (type-error l "num")])]
      [sub (l r) (type-case Type (typecheck-do l env)
                   [numT ()
                         (type-case Type (typecheck-do r env)
                           [numT () (numT)]
                           [else (type-error r "num")])]
                   [else (type-error l "num")])]
      [equ (l r) (type-case Type (typecheck-do l env)
                   [numT ()
                         (type-case Type (typecheck-do r env)
                           [numT () (boolT)]
                           [else (type-error r "num")])]
                   [else (type-error l "num")])]
      [id (name) (get-type name env)]
      [fun (param te body) (local [(define param-ty (parse-type te env))]
                             (begin
                               (validtype param-ty env)
                               (arrowT param-ty
                                       (typecheck-do body
                                                  (aBind param param-ty env)))))]
      [app (fun arg) (type-case Type (typecheck-do fun env)
                       [arrowT (param-ty result-ty)
                               (if (equal? param-ty (typecheck-do arg env))
                                   result-ty
                                   (type-error arg (to-string param-ty)))]
                       [else (type-error fun "function")])]
      [ifthenelse (test-e then-e else-e)
                  (type-case Type (typecheck-do test-e env)
                    [boolT ()
                           (if (compare-type (typecheck-do then-e env)
                                             (typecheck-do else-e env))
                               (typecheck-do then-e env)
                               (type-error then-e (to-string else-e)))]
                    [else (type-error test-e "bool" )])]
      [rec (name ty name-e body-e)
        (local [(define name-ty (parse-type ty env))
                (define new-env (aBind name name-ty env))]
          (begin (validtype name-ty env)
                 (if (equal? name-ty (typecheck-do name-e new-env))
                     (typecheck-do body-e new-env)
                     (type-error name-e (to-string name-ty)))))]
      [with-type (name var1-name var1-te
                       var2-name var2-te body)
        (local [(define var1-ty (parse-type var1-te env))
                (define var2-ty (parse-type var2-te env))
                (define new-env (tBind name var1-name var1-ty
                                            var2-name var2-ty env))]
          (begin
            (validtype var1-ty new-env)
            (validtype var2-ty new-env)
            (typecheck-do body
                       (aBind var1-name
                              (arrowT var1-ty
                                      (idT name))
                              (aBind var2-name
                                     (arrowT var2-ty
                                             (idT name))
                                     new-env)))))]
      [cases (type-name dispatch-e var1-name var1-id var1-body
                        var2-name var2-id var2-body)
        (local [(define bind (find-type-id type-name env))]
          (if (and (equal? var1-name (tBind-var1-name bind))
                   (equal? var2-name (tBind-var2-name bind)))
              (type-case Type (typecheck-do dispatch-e env)
                [idT (name)
                     (if (equal? name type-name)
                         (local [(define body1-ty (typecheck-do var1-body
                                                             (aBind var1-id (tBind-var1-type bind) env)))
                                 (define body2-ty (typecheck-do var2-body
                                                             (aBind var2-id (tBind-var2-type bind) env)))]
                           (if (equal? body1-ty body2-ty)
                               body1-ty
                               (type-error body2-ty (to-string body1-ty))))
                         (type-error dispatch-e (to-string type-name)))]
                [else (type-error dispatch-e (to-string type-name))])
              (type-error expr "Variant name matching")))]
      
      [tfun (name body) (polyT name (typecheck-do body env))]
      
      [tapp (body te) (local [(define ty (parse-type te env))
                              (define tfun-ty (typecheck-do body env))
                              (define new-env (type-case Type tfun-ty
                                                [polyT (s b) (aBind (polyT-forall tfun-ty) ty env)]
                                                [else (error 'tapp "no")]))]
                        (begin
                          (validtype ty new-env)
                          (foralltv (polyT-body tfun-ty) (polyT-forall tfun-ty) ty)))])))


(define pValidation : (Type Pbind -> Type)
  (lambda (ty pbind)
    (type-case Type ty
      [numT () ty]
      [boolT () ty]
      [arrowT (a b) (arrowT (pValidation a pbind) (pValidation b pbind))]
      [polyT (s body) (polyT s (pValidation body (aP s pbind)))]
      [idT (s) (error 'pValidation (string-append "idT can't found after typecheck is done" (to-string s)))]
      [tvT (s) (if (inPbind s pbind) ty (error 'pValidation "free"))])))
                
(define inPbind : (symbol Pbind -> boolean)
  (lambda (s pbind)
    (type-case Pbind pbind
      [mtP () #f]
      [aP (s2 rest) (if (symbol=? s s2) #t (inPbind s rest))])))

(define typecheck : (EXPR TypeEnv -> Type)
  (lambda (expr env)
    (pValidation (typecheck-do expr env) (mtP))))

;_________________________________________________________________________________________________________________________________________________________
;;given test cases

(test (typecheck (tfun 'alpha (num 3)) (mtEnv))
      (polyT 'alpha (numT)))
 
(test (typecheck (tfun 'alpha (tfun 'beta (num 3))) (mtEnv))
      (polyT 'alpha (polyT 'beta (numT))))

(test (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))) (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))

(test (typecheck-do (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))) (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))
 
(test (typecheck (tapp (id 'f) (numTE)) (aBind 'f (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))) (mtEnv)))
      (arrowT (numT) (numT)))

(test (typecheck (tfun 'alpha (tfun 'beta (fun 'x (polyTE 'alpha (polyTE 'beta (tvTE 'alpha))) (id 'x)))) (mtEnv))
      (polyT 'alpha (polyT 'beta (arrowT (polyT 'alpha (polyT 'beta (tvT 'alpha)))
                                         (polyT 'alpha (polyT 'beta (tvT 'alpha)))))))

(test (typecheck (tapp (tfun 'alpha (tfun 'beta (fun 'x (polyTE 'alpha (polyTE 'beta (tvTE 'alpha))) (id 'x)))) (numTE)) (mtEnv)) (polyT 'beta (arrowT (polyT 'alpha (polyT 'beta (tvT 'alpha))) (polyT 'alpha (polyT 'beta (tvT 'alpha))))))

(test (typecheck (fun 'x (polyTE 'alpha (tvTE 'alpha)) (id 'x)) (mtEnv)) (arrowT (polyT 'alpha (tvT 'alpha)) (polyT 'alpha (tvT 'alpha))))

(test/exn (typecheck (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'beta))) (id 'x)) (mtEnv)) "free")

(test/exn (typecheck (tfun 'alpha (fun 'x (tvTE 'beta) (id 'x))) (mtEnv)) "free")

(test/exn (typecheck (tapp (id 'f) (numTE)) (aBind 'f (arrowT (numT) (numT)) (mtEnv))) "no")

(test/exn (typecheck (tfun 'alpha (fun 'x (tvTE 'beta) (id 'x))) (mtEnv)) "free")

(test/exn (typecheck (tapp (tfun 'alpha (fun 'x (tvTE 'beta) (id 'x))) (numTE)) (mtEnv)) "free")

(test/exn (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha) (tfun 'beta (fun 'y (tvTE 'beta) (add (id 'x) (id 'y)))))) (mtEnv)) "no")

(test/exn (typecheck (tfun 'alpha (fun 'x (tvTE 'beta) (id 'x))) (mtEnv)) "free")

(test (interp (app (app (tapp (tfun 'alpha (fun 'f (tvTE 'alpha) (id 'f))) (arrowTE (numTE) (numTE))) (fun 'x (numTE) (id 'x))) (num 10)) (mtSub)) (numV 10))

(test (interp (tapp (tfun 'alpha (fun 'f (tvTE 'alpha) (id 'f))) (arrowTE (numTE) (numTE))) (mtSub)) (closureV 'f (id 'f) (mtSub)))

(test (interp (tapp (tapp (tfun 'alpha (tfun 'beta (num 3))) (numTE)) (numTE)) (mtSub)) (numV 3))

(test (interp (tfun 'alpha (fun 'x (tvTE 'beta) (id 'x))) (mtSub)) (closureV 'x (id 'x) (mtSub)))

(test (interp (tfun 'alpha (fun 'x (tvTE 'beta) (id 'x))) (mtSub)) (closureV 'x (id 'x) (mtSub)))

(test (interp (id 'x)
              (aSub 'x (numV 10) (mtSub)))
      (numV 10))

(test (interp (app (fun 'x (numTE)
                        (app (fun 'f (arrowTE (numTE) (numTE))
                                  (add (app (id 'f) (num 1))
                                       (app (fun 'x (numTE)
                                                 (app (id 'f)
                                                      (num 2)))
                                            (num 3))))
                             (fun 'y (numTE)
                                  (add (id 'x) (id 'y)))))
                   (num 0))
              (mtSub))
      (numV 3))

(test (typecheck (tfun 'alpha (num 3)) (mtEnv))
      (polyT 'alpha (numT)))

(test (typecheck (tfun 'alpha (tfun 'beta (num 3))) (mtEnv))
      (polyT 'alpha (polyT 'beta (numT))))

(test (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))) (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))

(test (typecheck (tapp (id 'f) (numTE)) (aBind 'f (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))) (mtEnv)))
      (arrowT (numT) (numT)))

(test (typecheck (tapp (id 'f) (numTE)) (aBind 'f (polyT 'alpha (polyT 'alpha (tvT 'alpha))) (mtEnv)))
      (polyT 'alpha (tvT 'alpha)))
      
(test (interp (tapp (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))) (numTE))
              (mtSub))
      (closureV 'x (id 'x) (mtSub)))
      
(test (typecheck
       (tapp (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x)))
             (polyTE 'beta (arrowTE (tvTE 'beta) (tvTE 'beta))))
       (mtEnv))
      (arrowT (polyT 'beta (arrowT (tvT 'beta) (tvT 'beta)))
              (polyT 'beta (arrowT (tvT 'beta) (tvT 'beta)))))
              
(test (typecheck (tfun 'alpha (tfun 'beta (num 3))) (mtEnv))
      (polyT 'alpha (polyT 'beta (numT))))

(test (interp (tfun 'alpha (tfun 'beta (num 3))) (mtSub))
      (numV 3))

(test (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))) (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))
      
(test (interp (app (app (tapp (tfun 'alpha (fun 'f (tvTE 'alpha) (id 'f))) (arrowTE (numTE) (numTE))) (fun 'x (numTE) (id 'x))) (num 10)) (mtSub)) (numV 10))

(test (interp (tapp (tfun 'alpha (fun 'f (tvTE 'alpha) (id 'f))) (arrowTE (numTE) (numTE))) (mtSub)) (closureV 'f (id 'f) (mtSub)))

(test (interp (tapp (tapp (tfun 'alpha (tfun 'beta (num 3))) (numTE)) (numTE)) (mtSub)) (numV 3))

(test (interp (tapp (tfun 'alpha (fun 'f (tvTE 'alpha) (id 'f))) (arrowTE (numTE) (numTE))) (mtSub)) (closureV 'f (id 'f) (mtSub)))

(test (typecheck (tfun 'alpha (tfun 'beta (fun 'x (tvTE 'alpha) (id 'x))))  (mtEnv)) (polyT 'alpha (polyT 'beta (arrowT (tvT 'alpha) (tvT 'alpha)))))

(test (typecheck (ifthenelse (bool true)
                             (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x)))
                             (tfun 'beta (fun 'y (tvTE 'beta) (id 'y))))
                 (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))

(test (typecheck (ifthenelse (bool true)
                             (tfun 'beta (fun 'y (tvTE 'beta) (id 'y)))
                             (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))))
                 (mtEnv))
      (polyT 'beta (arrowT (tvT 'beta) (tvT 'beta))))


(test (typecheck (ifthenelse (equ (num 8) (num 8))
                             (tfun 'alpha (tfun 'beta (fun 'x (tvTE 'alpha) (id 'x))))
                             (tfun 'beta (tfun 'gamma (fun 'x (tvTE 'beta) (id 'x)))))
                 (mtEnv))
      (polyT 'alpha (polyT 'beta (arrowT (tvT 'alpha) (tvT 'alpha)))))

(test (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha)
                                   (tfun 'beta (fun 'y (tvTE 'alpha)
                                                    (ifthenelse (bool true)
                                                                (id 'x)
                                                                (id 'y))))))
                 (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha)
                            (polyT 'beta (arrowT (tvT 'alpha)
                                                 (tvT 'alpha))))))

(test (interp (app
               (tapp (ifthenelse (bool true)
                                 (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x)))
                                 (tfun 'beta (fun 'x (tvTE 'beta) (id 'x))))
                     (numTE)) (num 30)) (mtSub))
      (numV 30))
      
(test (interp
       (app (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha)))
                 (app (tapp (id 'x) (numTE)) (num 10)))
        (tfun 'beta (fun 'y (tvTE 'beta) (id 'y)))) (mtSub)) (numV 10))
        
(test (typecheck
       (tfun 'alpha
             (fun 'alpha (arrowTE (numTE) (numTE))
                  (fun 'x (tvTE 'alpha)
                       (id 'x)))) (mtEnv))
      (polyT 'alpha (arrowT (arrowT (numT) (numT)) (arrowT (tvT 'alpha) (tvT 'alpha)))))
      
(test (typecheck
       (fun 'alpha (arrowTE (numTE) (numTE))
            (tfun 'alpha
                  (fun 'x (tvTE 'alpha)
                       (id 'x)))) (mtEnv))
      (arrowT (arrowT (numT) (numT)) (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha)))))

(test (interp (tapp (tfun 'alpha (fun 'x (tvTE 'alpha) (num 5))) (numTE)) (mtSub)) (closureV 'x (num 5) (mtSub)))

(test (interp (tapp (tfun 'alpha (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha))) (id 'x))) (numTE)) (mtSub)) (closureV 'x (id 'x) (mtSub)))

(test (typecheck (tapp (tfun 'alpha (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha))) (id 'x))) (numTE)) (mtEnv)) (arrowT (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))) (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha)))))

(test (typecheck (tapp (tfun 'alpha (fun 'x (tvTE 'alpha) (num 5))) (numTE)) (mtEnv)) (arrowT (numT) (numT)))

(test (interp (app (app (tapp (tapp (tfun 'alpha (tfun 'beta (fun 'x (arrowTE (tvTE 'alpha) (tvTE 'beta)) (id 'x))))
                                    (numTE))
                              (numTE))
                        (fun 'x (numTE) (add (num 5) (id 'x))))
                   (num 3))
              (mtSub))
      (numV 8))

(test (interp (app (app (tapp (tapp (tfun 'alpha (tfun 'alpha (fun 'x (arrowTE (tvTE 'alpha) (tvTE 'alpha)) (id 'x))))
                                    (numTE))
                              (numTE))
                        (fun 'x (numTE) (add (num 5) (id 'x))))
                   (num 3))
              (mtSub))
      (numV 8))
(test (typecheck (ifthenelse (equ (num 8) (num 10))
                             (tfun 'alpha (tfun 'beta (fun 'x (tvTE 'alpha) (fun 'y (tvTE 'beta) (id 'y)))))
                             (tfun 'beta (tfun 'alpha (fun 'x (tvTE 'beta) (fun 'y (tvTE 'alpha) (id 'y))))))
                 (mtEnv))
      (polyT 'alpha (polyT 'beta (arrowT (tvT 'alpha) (arrowT (tvT 'beta) (tvT 'beta))))))

(test (typecheck (tapp (tfun 'alpha
                                 (fun 'alpha (tvTE 'alpha)
                                      (app (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha)))
                                           (app (tapp (id 'x) (numTE)) (num 10))) (tfun 'beta
                                                                                        (fun 'beta (tvTE 'beta)
                                                                                             (id 'beta)))))) (arrowTE (numTE) (numTE)))
          (mtEnv)) (arrowT (arrowT (numT) (numT)) (numT)))
(test (typecheck (tapp (tfun 'alpha
                                 (fun 'alpha (tvTE 'alpha)
                                      (app (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha)))
                                           (app (tapp (id 'x) (numTE)) (num 10))) (tfun 'beta
                                                                                        (fun 'beta (tvTE 'beta)
                                                                                             (id 'beta)))))) (numTE))
          (mtEnv)) (arrowT (numT) (numT)))
(test (typecheck (tapp (tfun 'alpha
                                 (fun 'alpha (tvTE 'alpha)
                                      (app (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha)))
                                           (app (tapp (id 'x) (numTE)) (num 10))) (tfun 'alpha
                                                                                        (fun 'alpha (tvTE 'alpha)
                                                                                             (id 'alpha)))))) (numTE))
          (mtEnv)) (arrowT (numT) (numT)))

(test (typecheck (tfun 'alpha (num 3)) (mtEnv))
      (polyT 'alpha (numT)))

(test (typecheck (tfun 'alpha (tfun 'beta (num 3))) (mtEnv))
      (polyT 'alpha (polyT 'beta (numT))))

(test (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))) (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))

(test (typecheck (tapp (id 'f) (numTE)) (aBind 'f (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))) (mtEnv)))
      (arrowT (numT) (numT)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(test (typecheck (ifthenelse (bool true)
                             (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x)))
                             (tfun 'beta (fun 'y (tvTE 'beta) (id 'y))))
                 (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha) (tvT 'alpha))))

(test (typecheck (ifthenelse (bool true)
                             (tfun 'beta (fun 'y (tvTE 'beta) (id 'y)))
                             (tfun 'alpha (fun 'x (tvTE 'alpha) (id 'x))))
                 (mtEnv))
      (polyT 'beta (arrowT (tvT 'beta) (tvT 'beta))))


(test (typecheck (ifthenelse (bool true)
                             (tfun 'alpha (tfun 'beta (fun 'x (tvTE 'alpha) (id 'x))))
                             (tfun 'beta (tfun 'gamma (fun 'x (tvTE 'beta) (id 'x)))))
                 (mtEnv))
      (polyT 'alpha (polyT 'beta (arrowT (tvT 'alpha) (tvT 'alpha)))))


(test (interp (tapp (tapp (tfun 'alpha (tfun 'beta (num 3))) (numTE)) (numTE)) (mtSub))
      (numV 3))

(test (typecheck (tfun 'alpha (fun 'x (tvTE 'alpha)
                                   (tfun 'beta (fun 'y (tvTE 'alpha)
                                                    (ifthenelse (bool true)
                                                                (id 'x)
                                                                (id 'y))))))
                 (mtEnv))
      (polyT 'alpha (arrowT (tvT 'alpha)
                            (polyT 'beta (arrowT (tvT 'alpha)
                                                 (tvT 'alpha))))))

(test (typecheck (app (fun 'x (polyTE 'alpha (arrowTE (tvTE 'alpha) (tvTE 'alpha))) (num 42)) (id 'f)) (aBind 'f (polyT 'beta (arrowT (tvT 'beta) (tvT 'beta))) (mtEnv))) (numT))

;;; tests on noah 234th article
(test (typecheck (fun 'x (polyTE 'alpha (tvTE 'alpha)) (id 'x)) (mtEnv))
      (arrowT (polyT 'alpha (tvT 'alpha)) (polyT 'alpha (tvT 'alpha))))

;;; tests on noah 236th article
(test (typecheck (tapp (tfun 'alpha (tfun 'beta (fun 'x (polyTE 'alpha (polyTE 'beta (tvTE 'alpha))) (id 'x)))) (numTE)) (mtEnv))
      (polyT 'beta (arrowT (polyT 'alpha (polyT 'beta (tvT 'alpha))) (polyT 'alpha (polyT 'beta (tvT 'alpha))))))

(test (typecheck (app (tapp (tapp (tfun 'alpha (tfun 'beta (fun 'x (tvTE 'alpha) (id 'x)))) (numTE)) (numTE)) (num 10)) (mtEnv)) (numT))
(test (interp (app (tapp (tapp (tfun 'alpha (tfun 'beta (fun 'x (tvTE 'alpha) (id 'x)))) (numTE)) (numTE)) (num 10)) (mtSub)) (numV 10))