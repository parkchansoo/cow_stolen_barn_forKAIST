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

(define (is-even testnum)
  (cond
    [(= testnum 0) true]
    [(= testnum 1) false]
    [(< testnum 0) (is-even (+ testnum 2))]
    [(> testnum 1) (is-even (- testnum 2))]
    [else 'notIntegerError]))

(test (is-even 0) true)
(test (is-even 1) false)
(test (is-even 6) true)
(test (is-even 7) false)
(test (is-even -2) true)
(test (is-even -7) false)


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