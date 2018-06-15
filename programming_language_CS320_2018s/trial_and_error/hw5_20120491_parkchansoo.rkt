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


;; BFAE abstract syntax trees
; BFAE is consist of operations(add, sub) and items (num, id)
; and funtion with application(fun, app), and box with box-operations(newbox, setbox, openbox) and helper(seqn) which helps to operate sequence by sequence
(define-type BWAE
  [num (n number?)]
  [id (name symbol?)]
  [add (lft BWAE?) (rht BWAE?)]
  [sub (lft BWAE?) (rht BWAE?)]
  [fun (param id?) (body BWAE?)]
  [app (ft BWAE?) (arg BWAE?)]
  [newbox (cont BWAE?)]
  [openbox (cont BWAE?)]
  [setbox (box BWAE?) (cont BWAE?)]
  [seqn (fst BWAE?) (sec BWAE?)]
  )

(define (parse-expr sexp)
  (match sexp
    [(list '+ l r) (add (parse-expr l) (parse-expr r))]
    [(list '- l r) (sub (parse-expr l) (parse-expr r))]
    [(list 'fun param body) (fun (parse-expr param) (parse-expr body))]
    [(list 'newbox cont) (newbox (parse-expr cont))]
    [(list 'openbox cont) (openbox (parse-expr cont))]
    [(list 'setbox box cont) (setbox (parse-expr box) (parse-expr cont))]
    [(list 'seqn fst sec) (seqn (parse-expr fst) (parse-expr))]
    [(list ftn arg) (app (parse-expr ftn) (parse-expr arg))]
    [(list single) (parse-expr single)]
    [(? symbol?) (id sexp)]
    [(? number?) (num sexp)]
    [else (error 'parse "bad-syntax: ~a" sexp)])



(define (parse sexp)
   (parse-expr (string->sexpr sexp)))


(define-type BWAE-value
  [numV (n number?)]
  [closureV (param id?) (body BWAE?)]
  [boxV (address integer?)])

(define-type defrdSub
  [mtSub]
  [aSub (namd id?) (value BWAE-value?) (rest defrdSub?)])

(define-type Store
  [mtSto]
  [aSto (address integer?) (value BWAE-value?) (rest store?)])

(define-type Value*Store
  [v*s (value BWAE-value?) (store Store?)])

(define (interp-two bwae1 bwae2 ds st oprt)
  (type-case Value*Store (interp bwae1 ds st)
    [v*s (V1 st1)
         (type-case Value*Store (interp bwae2 ds st1)
           [v*s (V2 st2) (oprt V1 V2 st2)])
         ]))

(define (bin-op op l r)
  (op (numV-n l) (numV-n r)))

(define (lookup s ds)
  (type-case defrdSub ds
    [mtSub (error 'parse "free identitifier error: ~a" s)]
    [else (if (symbol=? s (aSub-name ds))
              (aSub-value ds) (lookup s (aSub-rest ds)))]))

(define (st-lookup addr st)
  (type-case Store st
    [mtSto (error' parse ""

(define 


(define (interp bwae ds st)
  (type-case BWAE bwae
    [num (n) (numV n)]
    [id (s) (lookup 
    [add (l r) (numV (bin-op + (interp l ds st) (interp r ds st)))]
    [sub (l r) (numV (bin-op - (interp l ds st) (interp r ds st)))]
    [










     


