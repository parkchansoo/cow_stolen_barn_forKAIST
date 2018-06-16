;; tests for free-ids
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

(test (free-ids (add
                  (add
                    (with 'z (id 'a) (id 'c))
                    (sub (sub (id 'z) (id 'z)) (sub (id 'y) (id 'b))))
                  (with 'b (id 'a) (with 'a (id 'y) (add (id 'y) (id 'c))))))
      '(a b c y z))

(test (free-ids (with
                  'c
                  (add (id 'a) (sub (add (id 'a) (id 'a)) (add (id 'a) (id 'y))))
                  (with 'a (id 'z) (id 'z))))
      '(a y z))

(test (free-ids (with
                  'x
                  (add
                    (add (with 'z (id 'z) (id 'b)) (add (id 'x) (id 'b)))
                    (sub (add (id 'x) (id 'x)) (sub (id 'b) (id 'a))))
                  (with 'x (sub (add (id 'a) (id 'a)) (sub (id 'c) (id 'c))) (id 'b))))
      '(a b c x z))

(test (free-ids (add
                  (sub (id 'c) (add (add (id 'x) (id 'z)) (with 'y (id 'c) (id 'a))))
                  (with
                    'c
                    (sub (with 'z (id 'c) (id 'b)) (id 'b))
                    (with 'z (id 'z) (add (id 'x) (id 'y))))))
      '(a b c x y z))

(test (free-ids (add
                  (add
                    (with 'c (add (id 'x) (id 'c)) (with 'y (id 'z) (id 'a)))
                    (add (add (id 'b) (id 'x)) (id 'b)))
                  (add (with 'y (with 'x (id 'a) (id 'y)) (sub (id 'x) (id 'z))) (id 'y))))
      '(a b c x y z))

(test (free-ids (with
                  'x
                  (sub (id 'z) (id 'a))
                  (with
                    'a
                    (add (sub (id 'b) (id 'b)) (sub (id 'b) (id 'z)))
                    (sub (sub (id 'c) (id 'x)) (with 'y (id 'b) (id 'y))))))
      '(a b c z))

(test (free-ids (add
                  (add
                    (add (add (id 'c) (id 'b)) (with 'z (id 'b) (id 'a)))
                    (add (id 'c) (with 'b (id 'c) (id 'x))))
                  (with 'b (id 'z) (with 'b (add (id 'c) (id 'z)) (sub (id 'x) (id 'z))))))
      '(a b c x z))

(test (free-ids (sub
                  (sub (with 'y (sub (id 'a) (id 'z)) (id 'y)) (add (id 'a) (id 'c)))
                  (with
                    'y
                    (add (sub (id 'x) (id 'b)) (sub (id 'x) (id 'c)))
                    (add (sub (id 'a) (id 'c)) (id 'x)))))
      '(a b c x z))

(test (free-ids (sub
                  (sub
                    (add (sub (id 'x) (id 'a)) (with 'a (id 'x) (id 'y)))
                    (add (id 'a) (sub (id 'b) (id 'y))))
                  (add (sub (add (id 'x) (id 'y)) (id 'x)) (id 'y))))
      '(a b x y))

(test (free-ids (sub
                  (sub (add (with 'b (id 'y) (id 'z)) (id 'y)) (with 'y (id 'b) (id 'b)))
                  (sub (id 'z) (id 'x))))
      '(b x y z))

;; tests for binding-ids
(test (binding-ids (add (num 3) (sub (id 'x) (id 'y)))) '())
(test (binding-ids (with 'y (num 3) (with 'x (id 'x) (id 'y)))) '(x y))
(test (binding-ids (with 'y (num 3) (with 'y (id 'x) (add (id 'x) (id 'y))))) '(y))
(test (binding-ids (with 'y (num 3) (with 'y (with 'x (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (add (id 'x) (id 'y))))) '(x y))
(test (binding-ids (with 'z (num 3) (with 'w (with 'z (add (num 3) (id 'y)) (sub (id 'x) (id 'y))) (with 'w (id 'y) (add (num 7) (id 'w)))))) '(w z))

(test (binding-ids (add
                     (add
                       (with 'z (id 'a) (id 'c))
                       (sub (sub (id 'z) (id 'z)) (sub (id 'y) (id 'b))))
                     (with 'b (id 'a) (with 'a (id 'y) (add (id 'y) (id 'c))))))
      '(a b z))

(test (binding-ids (with
                     'c
                     (add (id 'a) (sub (add (id 'a) (id 'a)) (add (id 'a) (id 'y))))
                     (with 'a (id 'z) (id 'z))))
      '(a c))

(test (binding-ids (with
                     'x
                     (add
                       (add (with 'z (id 'z) (id 'b)) (add (id 'x) (id 'b)))
                       (sub (add (id 'x) (id 'x)) (sub (id 'b) (id 'a))))
                     (with 'x (sub (add (id 'a) (id 'a)) (sub (id 'c) (id 'c))) (id 'b))))
      '(x z))

(test (binding-ids (add
                     (sub (id 'c) (add (add (id 'x) (id 'z)) (with 'y (id 'c) (id 'a))))
                     (with
                       'c
                       (sub (with 'z (id 'c) (id 'b)) (id 'b))
                       (with 'z (id 'z) (add (id 'x) (id 'y))))))
      '(c y z))

(test (binding-ids (add
                     (add
                       (with 'c (add (id 'x) (id 'c)) (with 'y (id 'z) (id 'a)))
                       (add (add (id 'b) (id 'x)) (id 'b)))
                     (add (with 'y (with 'x (id 'a) (id 'y)) (sub (id 'x) (id 'z))) (id 'y))))
      '(c x y))

;; tests for bound-ids
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

(test (bound-ids (add
                   (add
                     (with 'z (id 'a) (id 'c))
                     (sub (sub (id 'z) (id 'z)) (sub (id 'y) (id 'b))))
                   (with 'b (id 'a) (with 'a (id 'y) (add (id 'y) (id 'c))))))
      '())

(test (bound-ids (with
                   'c
                   (add (id 'a) (sub (add (id 'a) (id 'a)) (add (id 'a) (id 'y))))
                   (with 'a (id 'z) (id 'z))))
      '())

(test (bound-ids (with
                   'x
                   (add
                     (add (with 'z (id 'z) (id 'b)) (add (id 'x) (id 'b)))
                     (sub (add (id 'x) (id 'x)) (sub (id 'b) (id 'a))))
                   (with 'x (sub (add (id 'a) (id 'a)) (sub (id 'c) (id 'c))) (id 'b))))
      '())

(test (bound-ids (add
                   (sub (id 'c) (add (add (id 'x) (id 'z)) (with 'y (id 'c) (id 'a))))
                   (with
                     'c
                     (sub (with 'z (id 'c) (id 'b)) (id 'b))
                     (with 'z (id 'z) (add (id 'x) (id 'y))))))
      '())

(test (bound-ids (add
                   (add
                     (with 'c (add (id 'x) (id 'c)) (with 'y (id 'z) (id 'a)))
                     (add (add (id 'b) (id 'x)) (id 'b)))
                   (add (with 'y (with 'x (id 'a) (id 'y)) (sub (id 'x) (id 'z))) (id 'y))))
      '())

(test (bound-ids (with
                   'x
                   (sub (id 'z) (id 'a))
                   (with
                     'a
                     (add (sub (id 'b) (id 'b)) (sub (id 'b) (id 'z)))
                     (sub (sub (id 'c) (id 'x)) (with 'y (id 'b) (id 'y))))))
      '(x y))

(test (bound-ids (add
                   (add
                     (add (add (id 'c) (id 'b)) (with 'z (id 'b) (id 'a)))
                     (add (id 'c) (with 'b (id 'c) (id 'x))))
                   (with 'b (id 'z) (with 'b (add (id 'c) (id 'z)) (sub (id 'x) (id 'z))))))
      '())

(test (bound-ids (sub
                   (sub (with 'y (sub (id 'a) (id 'z)) (id 'y)) (add (id 'a) (id 'c)))
                   (with
                     'y
                     (add (sub (id 'x) (id 'b)) (sub (id 'x) (id 'c)))
                     (add (sub (id 'a) (id 'c)) (id 'x)))))
      '(y))

(test (bound-ids (sub
                   (sub
                     (add (sub (id 'x) (id 'a)) (with 'a (id 'x) (id 'y)))
                     (add (id 'a) (sub (id 'b) (id 'y))))
                   (add (sub (add (id 'x) (id 'y)) (id 'x)) (id 'y))))
      '())

(test (bound-ids (sub
                   (sub (add (with 'b (id 'y) (id 'z)) (id 'y)) (with 'y (id 'b) (id 'b)))
                   (sub (id 'z) (id 'x))))
      '())
