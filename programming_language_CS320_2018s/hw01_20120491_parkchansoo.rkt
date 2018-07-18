#lang plai


;dollar→won: number -> number
;problem1. convert given dollor as won, and rate is 1100

(define (dollar→won dollor)
  (* 1100 dollor))

(test (dollar→won 0) 0)
(test (dollar→won 1) 1100)
(test (dollar→won 10) 11000)
(test (dollar→won -4) -4400)


;volume-cuboid: number number number -> number
;problem2. get three integers as length of cuboid and return volume

(define (volume-cuboid x y z)
  (* x y z))

(test (volume-cuboid 1 1 1) 1)
(test (volume-cuboid 0 1 1) 0)
(test (volume-cuboid 2 3 4) 24)


;is-even: number -> boolean
;problem3. get one integer and check whether is even number, by recursively minus or plus 2

(define (is-even? testnum)
  (cond
    [(= testnum 0) true]
    [(= testnum 1) false]
    [(< testnum 0) (is-even? (+ testnum 2))]
    [(> testnum 1) (is-even? (- testnum 2))]
    [else 'notIntegerError]))

(test (is-even? 0) true)
(test (is-even? 1) false)
(test (is-even? 6) true)
(test (is-even? 7) false)
(test (is-even? -2) true)
(test (is-even? -7) false)


;gcd: number number -> number
;problem4. return greatest common divisor with Ueclid dialytic method by given two numbers

(define (gcd x y)
  (cond
    [(= x 0) y]
    [(< x 0) (gcd (- x) y)]
    [(= y 0) x]
    [(< y 0) (gcd x (- y))]
    [(< x y) (gcd (- y x) x)]
    [else (gcd (- x y) y)]))

(test (gcd 0 2) 2)
(test (gcd 1 1) 1)
(test (gcd 2 1) 1)
(test (gcd 4 2) 2)
(test (gcd 12 4) 4)
(test (gcd 21 35) 7)
(test (gcd 34 57) 1)
(test (gcd -10 2) 2)
(test (gcd 15 -6) 3)
(test (gcd -20 -30) 10)


;lcm: number number -> number
;problem5. return least common multiple of two given numbers by using gcd function

(define (lcm x y)
  (cond
    [(= x 0) y]
    [(< x 0) (lcm (- x) y)]
    [(= y 0) x]
    [(< y 0) (lcm x (- y))]
    [else (/ (* x y) (gcd x y))]))

(test (lcm 1 1) 1)
(test (lcm 24 12) 24)
(test (lcm 35 15) 105)
(test (lcm 30 45) 90)
(test (lcm -15 5) 15)
(test (lcm 17 -2) 34)
(test (lcm -6 -9) 18)


;Type definition: create type 'COURSE'
;type named COURSE has three classes. each class has some number-type attributes.CS320 has quiz & homework, CS311 has homework, CS330 has projects and homework

(define-type COURSE
  [CS320 (quiz integer?)
         (homework integer?)]
  [CS311 (homework integer?)]
  [CS330 (projects integer?)
         (homework integer?)])

(define classA (CS320 2 1))
(test (COURSE? classA) true)
(test (CS311? classA) false)
(test (CS320? classA) true)
(test (CS320-quiz classA) 2)
(test (CS320-homework classA) 1)

(define classB (CS311 3))
(test (COURSE? classB) true)
(test (CS311? classB) true)
(test (CS311-homework classB) 3)

(define classC (CS330 10 83))
(test (COURSE? classC) true)
(test (CS311? classC) false)
(test (CS330? classC) true)
(test (CS330-projects classC) 10)
(test (CS330-homework classC) 83)


;have-homework: COURSE -> number
;problem7. get COURSE name, and returns homework value

(define (have-homework course)
  (cond
    [(CS320? course) (CS320-homework course)]
    [(CS311? course) (CS311-homework course)]
    [(CS330? course) (CS330-homework course)]
    [else "courseIsNotDefined"]))

(test (have-homework classA) 1)
(test (have-homework classB) 3)
(test (have-homework classC) 83)


;have-projects: COURSE -> boolean
;problem8. get COURSE name, and check projects field on CS330. if the value is bigger than two, return true. else, return false

(define (have-projects course)
  (cond
    [(CS330? course) (count-projects course)]
    [else false]))

(define (count-projects course)
    (cond
      [(<= 2 (CS330-projects course)) true]
      [else false]))

(test (have-projects classA) false)
(test (have-projects classB) false)
(test (have-projects classC) true)
(define classD (CS330 1 1))
(test (have-projects classD) false)
(define classE (CS330 0 33))
(test (have-projects classE) false)
(define classF (CS330 2 1))
(test (have-projects classF) true)


;name-pets: list -> list
;problem9. get list of pet species. give name of pets, and return list of name. dog > happy/ cat > smart/ pig > pinky/

(define (name-pets pet-list)
  (cond
    [(null? pet-list) pet-list]
    [else (naming-pet pet-list '() )]))

(define (naming-pet pet-list name-list)
  (cond
    [(null? pet-list) name-list]
    [else (naming-pet (rest pet-list)
                      (append name-list (label-name (first pet-list))) )]))

(define (label-name pet)
  (cond
    [(symbol=? pet 'dog) '(happy)]
    [(symbol=? pet 'cat) '(smart)]
    [(symbol=? pet 'pig) '(pinky)]
    [else (list pet)]))

(test (name-pets '()) '())
(test (name-pets '(dog)) '(happy))
(test (name-pets (list 'cat)) '(smart))
(test (name-pets (list 'pig)) '(pinky))
(test (name-pets (list 'dog 'cat 'pig)) '(happy smart pinky))
(test (name-pets (list 'puppy 'piggy)) '(puppy piggy))
(test (name-pets (list 'doggy 'cat 'pig)) '(doggy smart pinky))


;give-name: list -> list
;get list of pet, find pet named on "old" parameter and replace by "new" parameter

(define (give-name old new pet-list)
  (cond
    [(null? pet-list) pet-list]
    [else (naming-pet-alter old new pet-list '() )]))

(define (naming-pet-alter old new pet-list name-list)
  (cond
    [(null? pet-list) name-list]
    [(symbol=? old (first pet-list)) (append name-list
                                             (list new)
                                             (rest pet-list))]
    [else (naming-pet-alter old new (rest pet-list) (append name-list (list (first pet-list))))]))

(test (give-name 'dog 'happy '()) '())
(test (give-name 'dog 'happy '(dog)) '(happy))
(test (give-name 'puppy 'pop '(puppy cat)) '(pop cat))
(test (give-name 'puddle 'pot '(cat puddle)) '(cat pot))
(test (give-name 'bear 'pooh (cons 'pig (cons 'cat (cons 'bear empty)))) '(pig cat pooh))



;; Problem 1.
(test (dollar→won 1) 1100)
(test (dollar→won 2) 2200)
(test (dollar→won 3) 3300)
(test (dollar→won 4) 4400)
(test (dollar→won 5) 5500)
(test (dollar→won 6) 6600)
(test (dollar→won 7) 7700)
(test (dollar→won 8) 8800)
(test (dollar→won 9) 9900)
(test (dollar→won 42) 46200)

;; Problem 2.
(test (volume-cuboid 1 2 3) 6)
(test (volume-cuboid 4 5 7) 140)
(test (volume-cuboid 6 3 10) 180)
(test (volume-cuboid 2 4 3) 24)
(test (volume-cuboid 10 20 30) 6000)
(test (volume-cuboid 100 200 300) 6000000)
(test (volume-cuboid 10 3 1) 30)
(test (volume-cuboid 3 10 12) 360)
(test (volume-cuboid 2 4 6) 48)
(test (volume-cuboid 42 42 42) 74088)

;; Problem 3.
(test (is-even? 10) #t)
(test (is-even? 14) #t)
(test (is-even? 123) #f)
(test (is-even? 0) #t)
(test (is-even? 1) #f)
(test (is-even? 2343) #f)
(test (is-even? 19) #f)
(test (is-even? 32) #t)
(test (is-even? 1023123) #f)
(test (is-even? 129380198312) #t)

;; Problem 4.
(test (gcd 14 24) 2)
(test (gcd 123 125) 1)
(test (gcd 242 34) 2)
(test (gcd 293 10482) 1)
(test (gcd 29292 92991) 3)
(test (gcd 24 48) 24)
(test (gcd 123123 2398023) 3)
(test (gcd 28423 12) 1)
(test (gcd 5 25) 5)
(test (gcd 124124124 124124124) 124124124)

;; Problem 5.
(test (lcm 14 24) 168)
(test (lcm 123 125) 15375)
(test (lcm 242 34) 4114)
(test (lcm 293 10482) 3071226)
(test (lcm 29292 92991) 907964124)
(test (lcm 24 48) 48)
(test (lcm 123123 2398023) 98417261943)
(test (lcm 28423 12) 341076)
(test (lcm 5 25) 25)
(test (lcm 124124124 124124124) 124124124)

;; Problem 6.
(test (COURSE? (CS320 3 3)) #t)
(test (CS320? (CS320 3 3)) #t)
(test (CS320-quiz (CS320 3 3)) 3)
(test (CS320-homework (CS320 3 3)) 3)
(test (COURSE? (CS311 42)) #t)
(test (CS311? (CS311 42)) #t)
(test (CS311-homework (CS311 42)) 42)
(test (COURSE? (CS330 5 5)) #t)
(test (CS330-projects (CS330 5 5)) 5)
(test (CS330-homework (CS330 5 5)) 5)

;; Problem 7.
(test (have-homework (CS320 3 3)) 3)
(test (have-homework (CS320 24 24)) 24)
(test (have-homework (CS320 48 48)) 48)
(test (have-homework (CS320 10 10)) 10)
(test (have-homework (CS311 43)) 43)
(test (have-homework (CS311 20)) 20)
(test (have-homework (CS330 10 10)) 10)
(test (have-homework (CS330 49 49)) 49)
(test (have-homework (CS330 30 30)) 30)
(test (have-homework (CS330 123 123)) 123)

;; Problem 8.
(test (have-projects (CS320 3 3)) #f)
(test (have-projects (CS320 24 24)) #f)
(test (have-projects (CS320 48 48)) #f)
(test (have-projects (CS320 10 10)) #f)
(test (have-projects (CS311 43)) #f)
(test (have-projects (CS311 20)) #f)
(test (have-projects (CS330 10 10)) #t)
(test (have-projects (CS330 0 0)) #f)
(test (have-projects (CS330 1 1)) #f)
(test (have-projects (CS330 123 123)) #t)

;; Problem 9.
(test (name-pets '(dog cat pig)) '(happy smart pinky))
(test (name-pets '(cat bat bear)) '(smart bat bear))
(test (name-pets '(cat dog dog pig)) '(smart happy happy pinky))
(test (name-pets '(cat tiger lion)) '(smart tiger lion))
(test (name-pets '(cat dolphin)) '(smart dolphin))
(test (name-pets '())'())
(test (name-pets '(a b c)) '(a b c))
(test (name-pets '(dog dog dog dog)) '(happy happy happy happy))
(test (name-pets '(cat cat pig dog dog)) '(smart smart pinky happy happy))
(test (name-pets '(pig pig pig)) '(pinky pinky pinky))

;; Problem 10.
(test (give-name 'bear 'pooh '(pig cat bear)) '(pig cat pooh))
(test (give-name 'a 'b '(a a c a c a)) '(b b c b c b))
(test (give-name 'dog 'happy '(dog cat bear dog)) '(happy cat bear happy))
(test (give-name 'happy 'dog '(dog happy happy cat)) '(dog dog dog cat))
(test (give-name 'bear 'bear '(bear cat tiger)) '(bear cat tiger))
(test (give-name 'tiger 'lion '(tiger tiger tiger)) '(lion lion lion))
(test (give-name 'a 'b '(a a a a a a a a a a a)) '(b b b b b b b b b b b))
(test (give-name 'c 'a '(c e f d s c e f c w t c)) '(a e f d s a e f a w t a))
(test (give-name 'dog 'tiger '()) '())
(test (give-name 'dog 'happy
                 (give-name 'cat 'smart
                            (give-name 'pig 'pinky
                                       '(pig tiger dog dolphin cat)))) '(pinky tiger happy dolphin smart))
