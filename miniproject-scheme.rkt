#lang racket

;read the file
(define list-students (car (file->list "all-students.lsp")))

;helper to map together elements in a list
(define pair-up
  (lambda (ls1 ls2)
    (map cons ls1 ls2)))

;turning the list from file into an association list
(define (student-to-ass-list lst)
  (if (null? lst)
      '()
      (cons (pair-up '("id" "name" "sex" "nationality" "age") (car lst)) (student-to-ass-list (cdr lst)))))

;the associated-list, which will be refered to in the later functions
(define associated-list (student-to-ass-list list-students))

;function to return one element with attribute from a list
(define (find-one-with-attr attr detail lst)
  (cond ((null? lst) '())
        ((equal? (cdr (assq attr (car lst))) detail) (car lst))
        (else (find-one-with-attr attr detail (cdr lst)))))

;function to return all elements with attribute from a list
(define (find-all-with-attr attr detail lst)
  (cond ((null? lst) '())
        ((equal? (cdr (assq attr (car lst))) detail) (cons (car lst) (find-all-with-attr attr detail (cdr lst))))
        (else (find-all-with-attr attr detail (cdr lst)))))

;function to return all elements with different attributes than the provided one (not in use)
(define (find-all-where-not-attr attr detail lst)
  (cond ((null? lst) '())
        ((equal? (cdr (assq attr (car lst))) detail) (find-all-where-not-attr attr detail (cdr lst)))
        (else (cons (car lst) (find-all-where-not-attr attr detail (cdr lst))))))

;function decides if element can be found in the list
(define (is-in-list lst x)
  (cond ((null? lst) #f)
        ((equal? (car lst) x) #t)
        (else (is-in-list (cdr lst) x))))

;makes sure the element will not appear twice or more in a list
(define (no-repeat-all-rand n r-lst o-lst s-lst)
  (if (< (length r-lst) n)
      (let ([rand (random n)])
        (if (is-in-list r-lst n)
            (no-repeat-all-rand n r-lst o-lst s-lst)
            (no-repeat-all-rand n (cons rand r-lst) o-lst (cons (list-ref o-lst rand) s-lst))))
      s-lst))

;function takes in a list and randomizes the order of its elements
(define (shuffle-list original-lst)
  (let ([repeat-helper-lst '()] [shuffled-lst '()])
    (no-repeat-all-rand (length original-lst) repeat-helper-lst original-lst shuffled-lst)))

;function for random grouping
(define (random-grouping sl gsl)
  (if (null? sl)
      '()
      (if (not (null? gsl))
          (students-to-random-group (shuffle-list sl) gsl gsl 0 0)
          '())))

;function that help the random grouping, takes in a student list, the group size limits and its copy operates,
;the current value inside a specific group size and the current group number
(define (students-to-random-group sl gsl gsl-copy gsl-value group-number)
  (cond ((null? sl) '())
        ((null? gsl-copy) (students-to-random-group sl gsl gsl gsl-value group-number))
        ((not (number? gsl-value)) '())
        ((eqv? 0 gsl-value) (students-to-random-group sl gsl (cdr gsl-copy) (car gsl-copy) (+ group-number 1)))
        (else (cons (cons (cons "group-number" group-number) (car sl)) (students-to-random-group (cdr sl) gsl gsl-copy (- gsl-value 1) group-number)))))

;a helper function which actually orders the students into groups by counting
(define (students-to-group-counting sl gsl gsl-copy)
  (if (null? sl)
      '()
      (if (eqv? gsl-copy 0)
          (students-to-group-counting sl gsl gsl)
          (cons (cons (cons "group-number" gsl-copy) (car sl)) (students-to-group-counting (cdr sl) gsl (- gsl-copy 1))))))

;function to group the students by counting
(define (group-by-counting sl k)
  (cond ((null? sl) '())
        ((> k (length sl)) (eqv? k (length sl)) sl)
        (else (students-to-group-counting sl k k))))

;calculates the percentage of an attribute in a list
(define (attr-percentage-calculator lst attr val)
  (/ (length (find-all-with-attr attr val lst)) (/ (length lst) 100)))

;rounds up the number, in some cases it was not an option to round down
(define (round-up n)
  (if (number? n)
      (if (>= (round n) n)
          (round n)
          (+ 1 (round n)))
      '()))

;checks if the given ethnicity already exists in the group
(define (is-ethnicity-in-group groups group-nr new-member)
  (cond ((null? (find-all-with-attr "group-number" group-nr groups))
         #f)
        ((>= (length (find-all-with-attr "nationality" (cdr (assq "nationality" new-member)) (find-all-with-attr "group-number" group-nr groups))) 1)
         #t)
        (else #f)))

;groups the list elements in a more balanced way regarding the sex and ethnicity
(define (balanced-grouping ls k)
  (let (
        [members-per-group (/ (length ls) k)]
        [female-per-group (round-up (* (/ (length ls) k) (/ (attr-percentage-calculator associated-list "sex" "female") 100)))]
        [balanced-group '()])
    (cond ((null? ls) '())
          ((eqv? k (length ls)) ls)
          (else (students-with-balanced-grouping ls k k members-per-group female-per-group balanced-group #f 0)))))

;helper function for balanced grouping
(define (students-with-balanced-grouping lst gn gn-copy st-in-g g-in-g b-g was-here was-here-when)
  (cond ((null? lst) b-g)
        ;resets gn-copy
        ((eqv? 0 gn-copy)
         (students-with-balanced-grouping lst gn gn st-in-g g-in-g b-g was-here was-here-when))
        ;sets the first element of the balanced-group
        ((null? b-g)
         (students-with-balanced-grouping (cdr lst) gn (- gn-copy 1) st-in-g g-in-g (cons (cons (cons "group-number" gn-copy) (car lst)) b-g) #f 0))
        ;check if the group exists
        ((and (< (length b-g) gn) (null? (find-one-with-attr "group-number" gn-copy b-g)))
         (students-with-balanced-grouping (cdr lst) gn (- gn-copy 1) st-in-g g-in-g (cons (cons (cons "group-number" gn-copy) (car lst)) b-g) #f 0))
        ;checks if the group is too big
        ((eqv? (length (find-all-with-attr "group-number" gn-copy b-g)) st-in-g)
         (students-with-balanced-grouping lst gn (- gn-copy 1) st-in-g g-in-g b-g was-here was-here-when))
        (else (if (eqv? (is-ethnicity-in-group (find-all-with-attr "group-number" gn-copy b-g) gn-copy (car lst)) #t)
                  (if (eqv? was-here #t)
                      (if (eqv? gn-copy was-here-when)
                          (students-with-balanced-grouping (cdr lst) gn (- gn-copy 1) st-in-g g-in-g (cons (cons (cons "group-number" gn-copy) (car lst)) b-g) #f 0)
                          (students-with-balanced-grouping lst gn (- gn-copy 1) st-in-g g-in-g b-g was-here was-here-when))
                      (students-with-balanced-grouping lst gn (- gn-copy 1) st-in-g g-in-g b-g #t gn-copy))
                  (if (equal? (cdr (assq "sex" (car lst))) "female")
                      (if (not (eqv? (length (find-all-with-attr "sex" "female" (find-all-with-attr "group-number" gn-copy b-g))) g-in-g))
                          (students-with-balanced-grouping (cdr lst) gn (- gn-copy 1) st-in-g g-in-g (cons (cons (cons "group-number" gn-copy) (car lst)) b-g) #f 0)
                          (students-with-balanced-grouping lst gn (- gn-copy 1) st-in-g g-in-g b-g #t gn-copy))
                      (students-with-balanced-grouping (cdr lst) gn (- gn-copy 1) st-in-g g-in-g (cons (cons (cons "group-number" gn-copy) (car lst)) b-g) #f 0))))))

;gives a list with the students who are equal or older than a given age
(define (age-equal-older lst age)
  (cond ((null? lst) '())
        ((>= (cdr (assq "age" (car lst))) age) (cons (car lst) (age-equal-older (cdr lst) age)))
        (else (age-equal-older (cdr lst) age))))

;lists students that are younger than a given age
(define (age-younger lst age)
  (cond ((null? lst) '())
        ((>= (cdr (assq "age" (car lst))) age) (age-younger (cdr lst) age))
        (else (cons (car lst) (age-younger (cdr lst) age)))))

;the function for predicate grouping
(define predicate-grouping
  (lambda (lst function [additional1 "not set"] [additional2 "not set"])
    (cond ((null? lst) "list is empty")
          ((equal? function students-with-same-age-starter)
           (if (and (number? additional1) (number? additional2))
               (function (shuffle lst) additional1 additional2)
               "one ore more additional values were invalid"))
          ((equal? function only-female-groups)
           (if (list? additional1)
               (function (shuffle lst) additional1)
               "the additional value was not a list"))
          ((equal? function no-same-age) (function (shuffle lst)))
          (else "invalid predicate"))))

;start of the n students with a age function
(define (students-with-same-age-starter lst n a)
  (let ([randomized-lst (shuffle lst)])
    (students-with-same-age (age-equal-older randomized-lst a) (age-younger randomized-lst a) n)))

;function that decides how many groups are necessary
(define (students-with-same-age lst-target lst-rest n)
  (let ([group-number (round-up (/ (length lst-target) n))])
    (cond ((> n (length lst-target)) '())
          (else (students-with-same-helper lst-target lst-rest n group-number group-number '())))))

;helper function for the n students with a age function
(define (students-with-same-helper lst-target lst-rest n gn gn-copy lst-helper)
  (cond ((and (null? lst-rest) (null? lst-target))
         lst-helper)
        ((eqv? gn-copy 0)
         (students-with-same-helper lst-target lst-rest n gn gn lst-helper))
        ((null? lst-target)
         (students-with-same-helper lst-target (cdr lst-rest) n gn (- gn-copy 1) (cons (cons (cons "group-number" gn-copy) (car lst-rest)) lst-helper)))
        (else (if (eqv? (length (find-all-with-attr "group-number" gn-copy lst-helper)) n)
                  (students-with-same-helper lst-target lst-rest n gn (- gn-copy 1) lst-helper)
                  (students-with-same-helper (cdr lst-target) lst-rest n gn gn-copy (cons (cons (cons "group-number" gn-copy) (car lst-target)) lst-helper))))))

;function to group only the female students in a similar manner as the random grouping
(define (only-female-groups lst gsl)
  (let ([female-students (find-all-with-attr "sex" "female" lst)])
    (if (null? female-students)
        '()
        (random-grouping female-students gsl))))

;function to group with not having the same age in the group
(define (no-same-age lst)
  (if (null? lst)
      '()
      (no-same-age-helper lst '())))

;helper function for na same age function
(define (no-same-age-helper lst helper-lst)
  (cond ((null? lst) helper-lst)
        ((null? helper-lst)
         (no-same-age-helper (cdr lst) (cons (cons (cons "group-number" 1) (car lst)) helper-lst)))
        (else (no-same-age-helper (cdr lst) (cons (cons (cons "group-number" (give-group-number-to-st (car lst) helper-lst 1)) (car lst)) helper-lst)))))

;function that calculates the group number the students should receive
(define (give-group-number-to-st st lst group-number)
  (if (null? (find-one-with-attr "age" (cdr (assq "age" st)) (find-all-with-attr "group-number" group-number lst)))
      group-number
      (give-group-number-to-st st lst (+ group-number 1))))

;method to return a group with a given group number
(define (return-group group-n lst)
  (find-all-with-attr "group-number" group-n lst))