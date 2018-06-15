#lang plai

; 0. setting WAE
; constructor num: number -> WAE
; constructor add: WAE WAE -> WAE 
; constructor sub: WAE WAE -> WAE
; constructor with: symbol WAE WAE -> WAE
; constructor id: symbol -> WAE
; defines type of formula of WAE, recursively

(define-type WAE
      [num (n number?)]
      [add (l WAE?) (r WAE?)]
      [sub (l WAE?) (r WAE?)]
      [with (s symbol?)
            (i WAE?)
            (b WAE?)]
      [id (s symbol?)])

(define waeA (num 2))
(test (WAE? waeA) true)
(test (num? waeA) true)
(test (add? waeA) false)
(test (num-n waeA) 2)
(define waeB (add waeA waeA))
(test (WAE? waeB) true)
(test (add? waeB) true)
(test (add-l waeB) waeA)
(test (add-r waeB) waeA)
(test (WAE? (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) true)
(define waeC (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x)))))
(test (with-s waeC) 'x)
(test (with-i waeC) (num 3))
(test (with-b waeC) (add (id 'x) (sub (num 3) (id 'x))))
(test (WAE? (with-b waeC)) true)
(test (add-l (with-b waeC)) (id 'x))
(test (add-r (with-b waeC)) (sub (num 3) (id 'x)))
(test (sub-l (add-r (with-b waeC))) (num 3))
(test (sub-r (add-r (with-b waeC))) (id 'x))
(define wae-a (id 'x))
(test (WAE? wae-a) true)
(define wae-b (with 'x (id 'y) (add (id 'x) (id 'z))))
(with-s wae-b)
(with-i wae-b)
(with-b wae-b)

;; problem1. free-ids

; subst : WAE symbol WAE -> WAE
; get new WAE which substites all symbol that introduced on 'with' constructor 
(define (subst wae s val)
  (type-case WAE wae
    [num (n) wae]
    [add (l r) (add (subst l s val) (subst r s val))]
    [sub (l r) (sub (subst l s val) (subst r s val))]
    [with (t i r) (with t (subst i s val)
                        (if (symbol=? s t) (subst r t (subst i s val)) (subst (subst r t (subst i s val)) s (subst i s val))))]
    [id (t) (if (symbol=? s t) val (id t))]))

;subst tests
(test (subst (num 3) 'x (num 3)) (num 3))
(test (subst (sub (num 3) (num 3)) 'x (num 3)) (sub (num 3) (num 3)))
(test (subst (id 'x) 'x (num 3)) (num 3))
(test (subst (id 'y) 'x (num 3)) (id 'y))
(test (subst (sub (num 3) (id 'x)) 'x (num 3)) (sub (num 3) (num 3)))
(test (subst (add (id 'x) (sub (num 3) (id 'x))) 'x (num 3)) (add (num 3) (sub (num 3) (num 3))))
(define waeD (with 'x (num 3) (add (id 'y) (sub (num 3) (id 'y)))))
(test (subst waeD 'x (num 3)) (with 'x (num 3) (add (id 'y) (sub (num 3) (id 'y)))))
(define waeE (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'y)))))
(test (subst waeE 'z (num 3)) (with 'x (num 3) (add (num 3) (sub (num 3) (id 'y)))))

; get-free-ids : WAE -> list-of-sym
; get WAE formula and find out free identifiers. so return symbol list of free identifiers. but don't consider about duplicate and order
(define (get-free-ids wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (get-free-ids l) (get-free-ids r))]
    [sub (l r) (append (get-free-ids l) (get-free-ids r))]
    [with (s i r) (append (get-free-ids (subst i 'default_symbol '())) (get-free-ids (subst r s i)))]
    [id (s) (list s)]
    ))
(get-free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x)))))
(get-free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x)))))
(get-free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x)))))
(get-free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b))))))

; given sorting policy
(define (symbol<? a b) (string<? (symbol->string a) (symbol->string b)))

; free-ids : WAE -> list-of-sym
; get WAE formula and find out free identifiers. so return symbol list of free identifiers by order, and each symbol appears most once.
(define (free-ids wae)
  (sort (remove-duplicates (get-free-ids wae)) symbol<?))

(test (free-ids (with 'x (num 3) (add (id 'x) (sub (num 3) (id 'x))))) '())
(test (free-ids (with 'x (num 3) (sub (id 'a) (add (num 4) (id 'x))))) '(a))
(test (free-ids (with 'x (num 3) (sub (id 'b) (sub (id 'a) (id 'x))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'a) (sub (id 'b) (add (id 'x) (id 'b)))))) '(a b))
(test (free-ids (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b y))
(test (free-ids (with 'x (id 't) (sub (id 'x) (with 'y (id 'y) (add (id 'x) (sub (id 'b) (id 'a))))))) '(a b t y))
(test (free-ids (with 'x (with 'y (num 3) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y)))) '(x y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'a) (id 'a)))) '(a b c y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(b c d y))
(test (free-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(b c d y z))


;; problem2. binding-ids : WAE → list-of-sym

; get-binding-ids : WAE → list-of-sym
; get WAE formula and find out binding occurance. so return symbol list of free identifiers. but don't consider about duplicate and order
(define (get-binding-ids wae)
    (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (get-binding-ids l) (get-binding-ids r))]
    [sub (l r) (append (get-binding-ids l) (get-binding-ids r))]
    [with (s i r) (append (list s)
                          (get-binding-ids i)
                          (get-binding-ids r)
                          )]
    [id (s) '()]
    ))

; binding-ids : WAE → list-of-sym
; get WAE formula and find out binding occurence. so return symbol list of free identifiers by order, and each symbol appears most once.
(define (binding-ids wae)
  (sort (remove-duplicates (get-binding-ids wae)) symbol<?))

(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))


;; problem3. bound-ids : WAE → list-of-sym

;get-all: WAE -> list-of-symbols
;from WAE extract all symbol they got
(define (get-all wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (get-all l) (get-all r))]
    [sub (l r) (append (get-all l) (get-all r))]
    [with (t i r) (append (get-all (id t)) (get-all i) (get-all r))]
    [id (t) (list t)]))

; get-bound-ids : WAE → list-of-sym
; get WAE formula and find out bound occurance. so return symbol list of free identifiers. but don't consider about duplicate and order
(define (get-bound-ids wae)
  (type-case WAE wae
    [num (n) '()]
    [add (l r) (append (get-bound-ids l) (get-bound-ids r))]
    [sub (l r) (append (get-bound-ids l) (get-bound-ids r))]
    [with (t i r) (append (if (member t (get-all r)) (list t) '())
                          (get-bound-ids r))]
    [id (t) '()]))

;; bound-ids : WAE → list-of-sym
; get WAE formula and find out bound occurence. so return symbol list of free identifiers by order, and each symbol appears most once.
(define (bound-ids wae)
  (sort (remove-duplicates (get-bound-ids wae)) symbol<?))

(test (bound-ids (with 'x (num 3) (add (id 'y) (num 3)))) '())
(test (bound-ids (with 'x (num 3) (add (id 'x) (sub (id 'x) (id 'y))))) '(x))
(test (bound-ids (with 'x (num 3) (add (id 'x) (with 'y (num 7) (sub (id 'x) (id 'y)))))) '(x y))
(test (bound-ids (with 'x (num 3) (with 'y (id 'x) (sub (num 3) (id 'y))))) '(x y))
(test (bound-ids (with 'x (num 3) (add (id 'y) (with 'y (id 'x) (sub (num 3) (num 7)))))) '(x))
(test (bound-ids (with 'x (id 'x) (add (id 'y) (with 'y (id 'y) (sub (num 3) (with 'z (num 7) (sub (id 'z) (id 'x)))))))) '(x z))
(test (bound-ids (with 'x (with 'y (num 3) (add (id 'x) (id 'y))) (add (id 'y) (with 'y (id 'y) (sub (num 3) (num 7)))))) '(y))
(test (bound-ids (with 'x (id 'a) (with 'y (id 'b) (with 'z (id 'c) (add (id 'd) (sub (id 'x) (add (id 'y) (id 'z)))))))) '(x y z))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'a)))) '(a x))
(test (bound-ids (add (with 'x (num 10) (with 'x (num 3) (sub (id 'y) (with 'y (num 7) (add (id 'x) (sub (id 'c) (id 'b))))))) (with 'a (id 'd) (id 'z)))) '(x))