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


;; BWAE abstract syntax trees
; BWAE is consist of operations(add, sub) and items (num, id)
; and funtion with application(fun, app), and box with box-operations(newbox, setbox, openbox) and helper(seqn) which helps to operate sequence by sequence
; furthermore, now we have rec, get, newset which contains variables on rec and modify or use by newset and get
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
  [seqn (fst BWAE?) (sec (listof BWAE?))]
  [rec (namelist (listof id?)) (valuelist (listof BWAE?))]
  [get (reclist BWAE?) (name id?)]
  [newset (reslist BWAE?) (name id?) (value BWAE?)]
  )


;; parse-expr : sexp -> BWAE
; get s-expression which modified by string->sepr function.
; and parse into BWAE syntax.
(define (parse-expr sexp)
  (match sexp
    [(list '+ l r) (add (parse-expr l) (parse-expr r))]
    [(list '- l r) (sub (parse-expr l) (parse-expr r))]
    [(list 'fun param body) (fun (parse-expr param) (parse-expr body))]
    [(list 'newbox cont) (newbox (parse-expr cont))]
    [(list 'openbox cont) (openbox (parse-expr cont))]
    [(list 'setbox box cont) (setbox (parse-expr box) (parse-expr cont))]
    [(list 'seqn fst sec ...) (seqn (parse-expr fst) (map parse-expr sec))]
    [(list 'rec fst ...) (rec (if (false? (check-duplicates (map first fst)))
                                  (map parse-expr (map first fst))
                                  (error 'rec "duplicated field"))
                           (map parse-expr (map second fst)))]
    [(list 'get reclist name) (get (parse-expr reclist) (parse-expr name))]
    [(list 'set reclist name value) (newset (parse-expr reclist) (parse-expr name) (parse-expr value))]
    [(list ftn arg) (app (parse-expr ftn) (parse-expr arg))]
    [(list single) (parse-expr single)]
    [(? symbol?) (id sexp)]
    [(? number?) (num sexp)]
    [else (error 'parse "bad-syntax: ~a" sexp)]))


; parse : string -> sexp
;; get promised strings and convert into BWAE with two function, string->sexpr and parse-expr.
(define (parse sexp)
   (parse-expr (string->sexpr sexp)))

;; test ;;
(parse "{rec {x 1}}")

; BWAE-value syntax-tree
;; when interprete BWAE, there has several type of outputs.
;; number, function, address, and recored
;; without number, the others acts as cache. especially, the appParamV is only used when passing argument into function parameters.
(define-type BWAE-value
  [numV (n number?)]
  [closureV (param id?) (body BWAE?) (ds defrdSub?)]
  [boxV (address integer?)]
  [recordV (ds defrdSub?)]
  [appParamV (value BWAE-value?)])

; defrdSub syntax-tree
;; deffered substitution get and contains BWAE-value with matched ids
;; mostly value contains address(boxV) but somtimes get parameter with argument(appParamV)
(define-type defrdSub
  [mtSub]
  [aSub (namd id?) (value BWAE-value?) (rest defrdSub?)])

; Store syntax-tree
;; store is mutable data type which contains value with matched address
(define-type Store
  [mtSto]
  [aSto (address integer?) (value BWAE-value?) (rest Store?)])

; Value*Store
;; to build our Store as mutable data. we have to pass value and store both, when interprete is on-going
;; but Racket only pass one return, so we pass new data-type.
(define-type Value*Store
  [v*s (value BWAE-value?) (store Store?)])

; interp-two : BWAE, BWAE, defrdSub, Store, operation -> Value*Store
;; when we interprete two BWAE-expressions, our Store has to change after first one is interpreted.
;; so we get first interpreted data, with Store. base on the interpreted Store, we interpret next expression and
;; do operatin which lambda given on interpreter.
(define (interp-two bwae1 bwae2 ds st oprt)
  (type-case Value*Store (interp bwae1 ds st)
    [v*s (V1 st1)
         (type-case Value*Store (interp bwae2 ds st1)
           [v*s (V2 st2) (oprt V1 V2 st2)])
         ]))

; interp-seqn : BWAE, listof BWAE, defrdSub, Store -> Value*Store
;; to interpret sequences of BWAE-expressions, we have to pass interpreted Store sequence by sequences.
;; we pass Store and BWAE-value recursively, so we pass all Store to next.
(define (interp-seqn bwae1 bwaelist ds st)
  (type-case Value*Store (interp bwae1 ds st)
    [v*s (V1 st1)
         (if (null? bwaelist) (v*s V1 st1)
             (interp-seqn (first bwaelist) (rest bwaelist) ds st1))]
         ))

(define (interp-recV bwae1 bwaelist recVlist ds st)
  (type-case Value*Store (interp bwae1 ds st)
    [v*s (V1 st1)
         (if (null? bwaelist) recVlist
             (interp-recV (first bwaelist) (rest bwaelist) (append recVlist (list V1)) ds st1))]
         ))

; bin-op : opertation, numV, numV -> numV
;; get two numV and do operations. add or sub
(define (bin-op op l r)
  (cond
    [(numV? l)
     (cond
       [(numV? r) (op (numV-n l) (numV-n r))]
       [else (error 'bin-op "get wrong input" r)])]
    [else (error 'bin-op "get wrong input" l)]))
(define (num+ l r)
  (bin-op + l r))
(define (num- l r)
  (bin-op - l r))

; lookup : symbol, defrdSub -> BWAE-value
;; lookup symbol in defrdsub and if there is matched symbol, return the value.
;; most of value would be boxV or appParamV
(define (lookup s ds)
  (type-case defrdSub ds
    [mtSub () (error "no such field" s)]
    [aSub (name value rest) (if (symbol=? s (id-name name)) value (lookup s rest))]))

; store-lookup : BWAE-value, Store -> BWAE-value
;; get BWAE-value which is appParamV and boxV.
;; if get boxV, search same address on Store and return value.
;; if get appParamV, just return Value on it.
(define (store-lookup addr st)
  (type-case BWAE-value addr
    [appParamV (v) v]
    [boxV (v)
     (type-case Store st
       [mtSto () (error 'unallocated)]
       [aSto (sto-a sto-v sto-rest) (if (= v sto-a) sto-v (store-lookup addr sto-rest))]
       )]
    [else (error 'store-lookup "got wrong item" addr)]))

; malloc : Store -> integer
;; our Store has maximum address on top
;; get top address of Store and return integer with integer which increased by 1.
(define (malloc s)
  (match s
    [(? Store?)
     (type-case Store s
       [mtSto () 1]
       [aSto (sto-a sto-v sto-rest) (+ 1 sto-a)])]
    [else (error 'parse "malloc input error: ~a" s)]
    ))

; reset-box : Store, boxV, BWAE-value -> Store
;; get Store and address to change the value, and the value we will input. so return modified Store.
(define (reset-box st addr val)
  (type-case Store st
    [mtSto () (error 'parse "setbox no address error: ~a" addr)]
    [aSto (sto-a sto-v sto-rest) (if (= (boxV-address addr) sto-a)
                                     (aSto (boxV-address addr) val sto-rest)
                                     (aSto sto-a sto-v (reset-box sto-rest addr val)))]))

; garbageC : Store, integer -> Store
;; find the given address as integer, and delete the address with value. and return Store.
(define (garbageC st addr)
  (type-case Store st
    [mtSto () (error 'gerbageC "wrong addr error: ~a" addr)]
    [aSto (a v rest) (if (= a addr) rest (aSto a v (garbageC rest addr)))]))

; recording-id : listof id, listof BWAE-value, defrdSub, Store -> Store
;; get ids and BWAE-value which soreted.
;; put into defrdSub and Store sequently.
;; return Store with recorded defrdSub.
(define (recording-id namelist valuelist r2ds st)
  (cond
    [(null? namelist) (list r2ds st)]
    [else (local [(define a (malloc st))]
            (recording-id (rest namelist) (rest valuelist)
                          (aSub (first namelist) (boxV a) r2ds) (aSto a (v*s-value (first valuelist)) st)))]))

; interp : BWAE, defrdSub, Store -> Value*Store.
;; interprete epressions which we parsed.
(define (interp bwae ds st)
  (type-case BWAE bwae
    [num (n) (v*s (numV n) st)]
    [id (s) (v*s (store-lookup (lookup s ds) st) st)]
    [add (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1) (v*s (numV (num+ v1 v2)) st1)))]
    [sub (l r) (interp-two l r ds st
                           (lambda (v1 v2 st1) (v*s (numV (num- v1 v2)) st1)))]
    [newbox (b) (type-case Value*Store (interp b ds st)
                  [v*s (v1 st1) (local [(define a (malloc st1))]
                                  (v*s (boxV a) (aSto a v1 st1)))])]
    [openbox (b) (type-case Value*Store (interp b ds st)
                   [v*s (b-cont st1)
                        (v*s (store-lookup b-cont st1) st1)])]
    [setbox (b val) (interp-two b val ds st
                                (lambda (v1 v2 st1) (v*s v2 (reset-box st1 v1 v2))))]
    [seqn (exp1 exp2) (interp-seqn exp1 exp2 ds st)]
    [fun (param body) (v*s (closureV param body ds) st)]
    [app (ftn arg) (interp-two ftn arg ds st
                               (lambda (ft-val arg-val st1)
                                   (local [(define interpV
                                             (interp (closureV-body ft-val)
                                                     (aSub (closureV-param ft-val) (appParamV arg-val) (closureV-ds ft-val))
                                                     st1))
                                           (define interpV-value (v*s-value interpV))]
                                     ;(v*s interpV-value (garbageC (v*s-store interpV) a))
                                     (v*s interpV-value (v*s-store interpV))
                                     
                                     )))]
    [rec (ids values) (local [(define recorded (recording-id ids
                                                             ;(interp-recV (first values) (rest values) null ds st)
                                                             (map (lambda (x) (interp x ds st)) values)
                                                             (mtSub) st))]
                        (v*s (recordV (first recorded)) (first (rest recorded))))]
    [get (reclist id) (if (id? id)
                          (local [(define rec-ds (interp reclist ds st))]
                            (interp id (recordV-ds (v*s-value rec-ds)) (v*s-store rec-ds)))
                          (error 'get "id-field get wrong input error ~a" id))]
    [newset (reclist id val) (local [(define recorded (interp reclist ds st))
                                  (define interped-val (interp val ds st))
                                  (define recorded-box (v*s-store recorded))]
                            (v*s (v*s-value interped-val) (reset-box recorded-box (lookup (id-name id) (recordV-ds (v*s-value recorded))) (v*s-value interped-val))))]
    ))

; getvalue : Value*Store -> integer or string.
;; from interpreted value, deliver information which kind of output came out.
(define (getvalue v-s)
  (type-case  Value*Store v-s
    [v*s (v s)
         (type-case BWAE-value v
           [numV (v) v]
           [closureV (p b d) 'func]
           [boxV (a) 'box]
           [recordV (d) 'record]
           [appParamV (v) (error 'interp-expr "get wrong type" v)])]))

(define (interp-expr expr)
  (getvalue (interp expr (mtSub) (mtSto))))


;; test case temp
(interp (parse "{{{fun {b} {fun {a} {openbox {newbox 11}}}} {newbox 9}} {newbox 10}}") (mtSub) (mtSto))
;(interp (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}") (mtSub) (mtSto))


;test case
(parse "{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}")
(interp (parse "{newbox 1}") (mtSub) (mtSto))
(parse "{{fun {b} {openbox b}} {newbox 1}}")
;(interp (parse "{{fun {b} {openbox b}} {newbox 1}}") (mtSub) (mtSto))
(interp (parse "{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}") (mtSub) (mtSto))
(test (interp (parse "{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}") (mtSub) (mtSto)) (v*s (numV 2) (aSto 1 (numV 2) (mtSto))))

 (test (interp (parse "{{fun {b}
                          {seqn
                           {setbox b {+ 2 {openbox b}}}
                           {setbox b {+ 3 {openbox b}}}
                           {setbox b {+ 4 {openbox b}}}
                           {openbox b}}}
                         {newbox 1}}")
                (mtSub)
                (mtSto))
        (v*s (numV 10)
             (aSto 1 (numV 10) (mtSto))))

(parse "{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}")
(interp (parse "{{fun {r} {set r x 5}} {rec {x 1}}}") (mtSub) (mtSto))
(interp-expr (parse "{{fun {r} {seqn {set r x 5} {get r x}}} {rec {x 1}}}"))
     

(test (interp (parse "{seqn 1 2}")
              (mtSub)
              (mtSto))
      (v*s (numV 2) (mtSto)))

(test (interp (parse "{{fun {b} {openbox b}}
                       {newbox 10}}")
              (mtSub)
              (mtSto))
      (v*s (numV 10)
           (aSto 1 (numV 10) (mtSto))))

(test (interp (parse "{{fun {b} {seqn
                                 {setbox b 12}
                                 {openbox b}}}
                       {newbox 10}}")
              (mtSub)
              (mtSto))
      (v*s (numV 12)
           (aSto 1
                 (numV 12)
                 (mtSto))))

(test (interp-expr (parse "{{fun {b} {seqn
                                      {setbox b 12}
                                      {openbox b}}}
                            {newbox 10}}"))
      12)

(test (interp (parse "{{fun {b} {openbox b}}
                       {seqn
                        {newbox 9}
                        {newbox 10}}}")
              (mtSub)
              (mtSto))
      (v*s (numV 10)
           (aSto 2 (numV 10)
                 (aSto 1 (numV 9) (mtSto)))))

;;Todo
(parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}")
(app-ft (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"))
(app-arg (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"))

(app-ft (app-ft (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}")))
(app-arg (app-ft (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}")))

(interp (app-ft (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}")) (mtSub) (mtSto))
(interp (app-arg (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}")) (mtSub) (mtSto))
(interp (app-ft (app-ft (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"))) (mtSub) (mtSto))
(interp (app-arg (app-ft (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}"))) (mtSub) (mtSto))

(test (interp (parse "{{{fun {b} {fun {a} {openbox b}}} {newbox 9}} {newbox 10}}") (mtSub) (mtSto)) (v*s (numV 9) (aSto 2 (numV 10) (aSto 1 (numV 9) (mtSto)))))
(interp (parse "{{{fun {b} {fun {a} {openbox {newbox 11}}}} {newbox 9}} {newbox 10}}") (mtSub) (mtSto))

(test (interp (parse "{{fun {b} {seqn {setbox b 2} {openbox b}}} {newbox 1}}") (mtSub) (mtSto)) (v*s (numV 2) (aSto 1 (numV 2) (mtSto))))

;;Todo
(interp (parse "{{fun {b}
                            {seqn
                             {setbox b {+ 2 {openbox b}}}
                             {setbox b {+ 3 {openbox b}}}
                             {setbox b {+ 4 {openbox b}}}
                             {openbox b}}}
                       {newbox 1}}")
              (mtSub)
              (mtSto))

(test (interp (parse "{{fun {b}
                            {seqn
                             {setbox b {+ 2 {openbox b}}}
                             {setbox b {+ 3 {openbox b}}}
                             {setbox b {+ 4 {openbox b}}}
                             {openbox b}}}
                       {newbox 1}}")
              (mtSub)
              (mtSto))
        (v*s (numV 10)
             (aSto 1 (numV 10) (mtSto))))


(test/exn (interp (parse "{openbox x}")
                  (aSub (id 'x) (boxV 1) (mtSub))
                  (mtSto))
          "unallocated")

;; records

(test (interp-expr (parse "{{fun {r}
                                 {get r x}}
                            {rec {x 1}}}"))
      1)

(test (interp-expr (parse "{{fun {r}
                                 {seqn
                                  {set r x 5}
                                  {get r x}}}
                            {rec {x 1}}}"))
      5)

(interp-expr (parse "{{{{{fun {g}
                                    {fun {s}
                                         {fun {r1}
                                              {fun {r2}
                                                   {+ {get r1 b}
                                                      {seqn
                                                       {{s r1} {g r2}}
                                                       {+ {seqn
                                                           {{s r2} {g r1}}
                                                           {get r1 b}}
                                                          {get r2 b}}}}}}}}
                               {fun {r} {get r a}}}            
                              {fun {r} {fun {v} {set r b v}}}}
                             {rec {a 0} {b 2}}}               
                            {rec {a 3} {b 4}}}"))

(test (interp-expr (parse "{{{{{fun {g}
                                    {fun {s}
                                         {fun {r1}
                                              {fun {r2}
                                                   {+ {get r1 b}
                                                      {seqn
                                                       {{s r1} {g r2}}
                                                       {+ {seqn
                                                           {{s r2} {g r1}}
                                                           {get r1 b}}
                                                          {get r2 b}}}}}}}}
                               {fun {r} {get r a}}}            
                              {fun {r} {fun {v} {set r b v}}}}
                             {rec {a 0} {b 2}}}               
                            {rec {a 3} {b 4}}}"))               ; r2
      5)

(test (interp-expr (parse "{fun {x} x}"))
      'func)
(test (interp-expr (parse "{newbox 1}"))
      'box)
(test (interp-expr (parse "{rec}"))
      'record)
