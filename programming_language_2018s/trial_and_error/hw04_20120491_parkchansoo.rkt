#lang plai

(require (for-syntax racket/base) racket/match racket/list racket/string
         (only-in mzlib/string read-from-string-all))

;; build a regexp that matches restricted character expressions, can use only
;; {}s for lists, and limited strings that use '...' (normal racket escapes
;; like \n, and '' for a single ')
(define good-char "(?:[ \t\r\na-zA-Z0-9_{}!?*/<=>:+-]|[.][.][.])")
;; this would make it awkward for students to use \" for strings
;; (define good-string "\"[^\"\\]*(?:\\\\.[^\"\\]*)*\"")
(define good-string "[^\"\\']*(?:''[^\"\\']*)*")
(define expr-re
  (regexp (string-append "^"
                         good-char"*"
                         "(?:'"good-string"'"good-char"*)*"
                         "$")))
(define string-re
  (regexp (string-append "'("good-string")'")))

(define (string->sexpr str)
  (unless (string? str)
    (error 'string->sexpr "expects argument of type <string>"))
    (unless (regexp-match expr-re str)
      (error 'string->sexpr "syntax error (bad contents)"))
    (let ([sexprs (read-from-string-all
                 (regexp-replace*
                  "''" (regexp-replace* string-re str "\"\\1\"") "'"))])
    (if (= 1 (length sexprs))
      (car sexprs)
      (error 'string->sexpr "bad syntax (multiple expressions)"))))

(test/exn (string->sexpr 1) "expects argument of type <string>")
(test/exn (string->sexpr ".") "syntax error (bad contents)")
(test/exn (string->sexpr "{} {}") "bad syntax (multiple expressions)")


;; MUWAE abstract syntax trees
;change num variant get list of number instead only one number
;add muwae-min and muwae-max with three inputs
(define-type FWAE
  [num  (n number?)]
  [add  (left FWAE?) (right FWAE?)]
  [sub  (left FWAE?) (right FWAE?)]
  [with (name id?) (init FWAE?) (body FWAE?)]
  [id   (name symbol?)]
  ;[ids  (namelist (listof symbol?))]
  [fun  (param (listof id?)) (body FWAE?)]
  [app  (ftn FWAE?) (arg (listof FWAE?))]
  ;[fun  (param list?) (body FWAE?)]
  ;[app  (ftn list?) (arg FWAE?)]
  [record (namelist (listof id?)) (fwaelist (listof FWAE?))]
  [access (recorded FWAE?) (name id?)]
  )

; parse-sexpr : sexpr -> MUWAE
;; to convert s-expressions into MUWAEs
(define (parse-sexpr sexp)
  (match sexp
    [(list '+ l r) (add (parse-sexpr l) (parse-sexpr r))]
    [(list '- l r) (sub (parse-sexpr l) (parse-sexpr r))]
    [(list 'with (list x i) b) (with (id x) (parse-sexpr i) (parse-sexpr b))]

    ;;add record and access!
    [(list 'record ids ...) (record (map recording ids) (map recording-fwae ids))]
    [(list 'access recorded id) (access (parse-sexpr recorded) (parse-sexpr id))]
    ;[(? (listof symbol?)) (ids sexp)]
    [(list 'fun param body) (fun (if (false? (check-duplicates param))
                                     (map parse-sexpr param)
                                     (error 'parse "bad syntax: ~a" sexp))
                                 (parse-sexpr body))]
    [(list ftn arg ...) (app (parse-sexpr ftn) (map parse-sexpr arg))]
    
    [(list singleVar) (parse-sexpr singleVar)] ;to cover case one symbol or number is on parenthesis {3} {d}
    [(? number?) (num sexp)] ;if we get number element, we change into list form
    [(? symbol?) (id sexp)]
    [else (error 'parse "bad syntax: ~a" sexp)]))

(define (recording sexp)
  (match sexp
    [(list id fwae) (parse-sexpr id)]))

(define (recording-fwae sexp)
  (match sexp
    [(list id fwae) (parse-sexpr fwae)]))


;; parses a string containing a WAE expression to a MUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;(parse "{+ 3 3}")
;(parse "{fun {} {5}}")
;(parse "{3}")
;(parse "{3 x}")
;;test case when we able to parse list on FWAE
;;(parse "{3 3 3}")
;(parse "{fun {a} {+ 2 2}}")
;(parse "{{fun {a} {+ 2 2}} {a}}")

;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define-type FWAE-value
  [numV (n number?)]
  ;TODO. check closures param type as list to fit on problem!
  [closureV (param (listof id?)) (body FWAE?) (ds defrdSub?)]
  [recordV (record-ds defrdSub?)])

(define-type defrdSub
  [mtSub]
  [aSub (name id?) (value FWAE-value?) (ds defrdSub?)])

;interp : FWAE -> FWAE-value
;;get FWAE and interprete with each methods which is add, sub , with, id, fun, app, num.
;;and return FWAE-value. FWAE-value can be numV, closureV, or records
(define (interp fwae ds)
  (type-case FWAE fwae
    [num (n) (numV n)]
    [add (l r) (numV (bin-op + (interp l ds) (interp r ds)))]
    [sub (l r) (numV (bin-op - (interp l ds) (interp r ds)))]
    [with (x i b) (interp b (aSub x (interp i ds) ds))]
    [id (s) (lookup s ds)]
    [fun (ftn body) (closureV ftn body ds)]
    [app (ft args) (local [(define ft-val (interp ft ds))
                          (define args-val
                            (map (lambda (x) (interp x ds)) args))]
                    (interp (closureV-body ft-val)
                            (argsToSub (closureV-param ft-val) args-val (closureV-ds ft-val))))]
    [record (namelist fwaelist) (recordV (if (false? (check-duplicates namelist))
                                             (r2Sub namelist fwaelist ds)
                                             (error "duplicate fields")))]
    [access (recorded id) (interp id (recordV-record-ds (interp recorded ds)))]
    ))

(define (argsToSub params args-val ds)
  (cond
    [(null? params) ds]
    [else (argsToSub (rest params) (rest args-val)
           (aSub (first params) (first args-val) ds))]))

(define (r2Sub namelist args-val ds)
  (cond
    [(null? namelist) ds]
    [else (r2Sub (rest namelist) (rest args-val)
                 (aSub (first namelist) (interp (first args-val) ds) ds))]))

;bin-op : (number number -> number) (listof number) (listof number) -> (listof number)
;; by two lists of number, produce all of combination for each elements.
;(define (bin-op op ls rs)
;  (define (helper l rs)
;    (define (f r) ;map function to mapping l into rs
;      (op l r))
;   (map f rs))
;  (if (null? ls)
;    null
;    (append (helper (first ls) rs) (bin-op op (rest ls) rs)))) ;recursively build list by combination of ls and rs
(define (bin-op op ls rs)
  (op (if (numV? ls) (numV-n ls) (numV-n (first ls))) (if (numV? rs) (numV-n rs) (numV-n (first rs)))))

(define (lookup s ds)
  (type-case defrdSub ds
    [mtSub () (error "no such field" s)]
    [aSub (x val rest) (if (symbol=? (id-name x) s) val
                           (lookup s rest))]))


(interp (parse "{+ 2 3}") (mtSub))
(parse "{fun {x} {+ 2 2}}")
(interp (parse "{fun {x} {+ 2 2}}") (mtSub))
(parse "{with {x 2} {+ x 3}}")
(interp (parse "{with {x 2} {+ x 3}}") (mtSub))
(parse "{with {f {fun {a b} {+ a b}}}
                  {with {g {fun {x} {- x 5}}}
                        {with {x {f 2 5}} {g x}}}}")
(parse "{with {f {fun {a b} {+ a b}}} {f 2 5}}")
(parse "{{fun {a b} {+ a b}} 2 5}")
(interp (parse "{{fun {a b} {+ a b}} 2 5}") (mtSub))
(interp (parse "{with {f {fun {a b} {+ a b}}} {f 2 5}}") (mtSub))
(interp (parse "{with {f {fun {a b} {+ a b}}}
                  {with {g {fun {x} {- x 5}}}
                        {with {x {f 2 5}} {g x}}}}") (mtSub))



; eval: FWAE-value -> number 
;; evaluates MUWAE expressions by reducing them to numbers
(define (eval fwaev)
  (type-case FWAE-value fwaev
    [numV (n) n]
    [closureV (x i b) 'function]
    [recordV (namelist) 'record]
    ;[else 'pass "all type-case on eval"]
    ))


; run : string -> listof number
;; evaluate a MUWAE program contained in a string
(define (run str)
  (eval (interp (parse str) (mtSub))))


(test (run "{record {a 10} {b {+ 1 2}}}")
      'record)
(test (run "{access {record {a 10} {b {+ 1 2}}} b}")
      3)
(test/exn (run "{access {record {b 10} {b {+ 1 2}}} b}")
          "duplicate fields")
(test/exn (run "{access {record {a 10}} b}")
          "no such field")
(test (run "{with {g {fun {r} {access r c}}}
                  {g {record {a 0} {c 12} {b 7}}}}")
      12)
(test (run "{access {record {r {record {z 0}}}} r}")
      'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}")
      0)
(test/exn (run "{record {z {access {record {z 0}} y}}}")
          "no such field")
(test (run "{with {f {fun {a b} {+ a b}}}
                  {with {g {fun {x} {- x 5}}}
                        {with {x {f 2 5}} {g x}}}}") 2)
(test (run "{with {f {fun {x y} {+ x y}}} {f 1 2}}") 3)
(test (run "{with {f {fun {} 5}}
                  {+ {f} {f}}}") 10)
(test (run "{with {h {fun {x y z w} {+ x w}}}
                  {h 1 4 5 6}}") 7) 
(test (run "{with {f {fun {} 4}}
                  {with {g {fun {x} {+ x x}}}
                        {with {x 10} {- {+ x {f}} {g 4}}}}}") 6)
(test (run "{record {a 10} {b {+ 1 2}}}") 'record)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)
(test (run "{with {x 3} {with {y 5} {access {record {a x} {b y}} a}}}") 3)
(test (run "{with {f {fun {a b} {+ {access a a} b}}}
                  {with {g {fun {x} {+ 5 x}}}
                        {with {x {f {record {a 10} {b 5}} 2}} {g x}}}}") 17)
(test (run "{with {f {fun {a b c d e} {record {a a} {b b} {c c} {d d} {e e}}}}
                  {access {f 1 2 3 4 5} c}}") 3)
(test (run "{with {f {fun {a b c} {record {a a} {b b} {c c}}}}
                  {access {f 1 2 3} b}}") 2)
(test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                  {access {f 1 2 3} y}}") 2)
(test (run "{with {f {fun {a b c} {record {x a} {y b} {z c} {d 2} {e 3}}}}
                  {access {f 1 2 3} d}}") 2)
(test (run "{with {f {fun {x} {+ 5 x}}}
                  {f {access {access {record {a {record {a 10} {b {- 5 2}}}} {b {access {record {x 50}} x}}} a} b}}}") 8)
(test (run "{access {record {a 10} {b {+ 1 2}}} b}") 3)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)
(test (run "{record {a 10}}") `record)
(test (run "{access {record {a 10}} a}") 10)
(test (run "{access {record {a {+ 1 2}}} a}") 3)
(test (run "{fun {x} x}") 'function)
(test (run "{access {record {a {record {b 10}}}} a}") `record)
(test (run "{access {access {record {a {record {a 10}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} b}") 20)
(test (run "{+ {access {record {a 10}} a} {access {record {a 20}} a}}") 30)
(test (run "{+ {access {record {a 10}} a} {access {record {a 20}} a}}") 30)
(test (run "{record {a 10}}") `record)
(test (run "{record {a {- 2 1}}}") `record)
(test (run "{access {record {a 10}} a}") 10)
(test (run "{access {record {a {- 2 1}}} a}") 1)
(test (run "{access {record {a {record {b 10}}}} a}") `record)
(test (run "{access {access {record {a {record {a 10}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} a}") 10)
(test (run "{access {access {record {a {record {a 10} {b 20}}}} a} b}") 20)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)
(test (run "{with {y {record {x 1} {y 2} {z 3}}} {access y y}}") 2)
(test (run "{with {y {record {x 1} {y 2} {z 3}}} {access y z}}") 3)
(test (run "{record {a 10} {b {+ 1 2}}}") 'record)
(test (run "{access {record {a 10} {b {+ 1 2}}} b}") 3)
(test (run "{with {g {fun {r} {access r c}}}
                  {g {record {a 0} {c 12} {b 7}}}}") 12)
(test (run "{access {record {r {record {z 0}}}} r}") 'record)
(test (run "{access {access {record {r {record {z 0}}}} r} z}") 0)