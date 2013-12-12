(load "../base.scm")
; abstraction for rational number
; Constructor and selectors
;(define make-rat cons)
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))
(define numer car)
(define denom cdr)

; compound procedures for rational number
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))
(define (display-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (rat=? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d))
        (sign (if (negative? (/ n d)) (- 1) 1)))
    (cons (* sign (abs (/ n g)))
          (abs (/ d g)))))
(tm "2.1")
(display-rat (make-rat (- 4) 6))
(display-rat (make-rat (- 4) (- 6)))
(display-rat (make-rat 4 6))

; Exercise 2.2
(define make-point cons)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)
(define x-point car)
(define y-point cdr)
(define (display-point p)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")")
  (display " "))
(define (midpoint-segment segment)
  (make-point (average (x-point (start-segment segment))
                       (x-point (end-segment segment)))
              (average (y-point (start-segment segment))
                       (y-point (end-segment segment)))))
(tm "2.2")
(define p11 (make-point 1 1))
(define p35 (make-point 4 5))
(display-point p11)
(display-point p35)
(display-point (midpoint-segment (make-segment p11 p35)))

; Exercise 2.3
; This is not a good to make vector!!!!!
(define (make-vect start end)
  (cons start end))
(define origin-vect car)
(define direct-vect cdr)
(define (x-length vect)
  (abs (- (x-point (direct-vect vect))
          (x-point (origin-vect vect)))))
(define (y-length vect)
  (abs (- (y-point (direct-vect vect))
          (y-point (origin-vect vect)))))
(define (length-vect vect)
  (sqrt ( + (square (x-length vect))
            (square (y-length vect)))))
(define (display-vect vect)
  (newline)
  (display-point (origin-vect vect))
  (display "->")
  (display-point (direct-vect vect)))
(tm "Vector")
(define vect (make-vect p11 p35))
(display-vect vect)
(newline)
(display "x-length: ")
(display (x-length vect))
(newline)
(display "y-length: ")
(display (y-length vect))
(newline)
(display "length: ")
(display (length-vect vect))

; A better way to represent vector by angle and length
(define mk-vect cons)
(define angle-vect car)
(define length-of-vect cdr)
(define (y-length-vect  vect)
  (* (sin (angle-vect vect))
     (length-of-vect vect)))
(define (x-length-vect vect)
  (* (cos (angle-vect vect))
     (length-of-vect vect)))

; rectangle
(define (make-rect origin w-vect h-vect)
  (cons origin (cons w-vect h-vect)))
(define w-rect cadr)
(define h-rect cddr)
(define origin-rect car)
(define (width-rect rect)
  (length-vect (w-rect rect)))
(define (height-rect rect)
  (length-vect (h-rect rect)))
(define (perimeter-rect rect)
  (+ (* 2 (width-rect rect))
     (* 2 (height-rect rect))))
(define (area-rect rect)
  (* (width-rect rect)
     (height-rect rect)))
(tm "exercise 2.3")
(define width-vect (make-vect p11 (make-point 4 5)))
(define height-vect (make-vect p11 (make-point -5 9)))
(define rect (make-rect p11 width-vect height-vect))
(newline)
(display "rect width: ")
(display (width-rect rect))
(newline)
(display "rect height ")
(display (height-rect rect))
(newline)
(display "rect perimeter: ")
(display (perimeter-rect rect))
(newline)
(display "rect area:")
(display (area-rect rect))
(newline)


; cons car and cdr could be implemented by procedures like:
(define (cons-i x y)
  (lambda (i)
    (cond ((= i 0) x)
          ((= i 1) y)
          (else (error "something is wrong with cons-i")))))
(define (car-i z) (z 0))
(define (cdr-i z) (z 1))

; Exercise 2.5
(define (remove-n base result)
  (if (= (gcd result base) 1) result
    (remove-n base (/ result base))))
(define (cons-integer a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car-integer z)
  (/ (log (remove-n 3 z))
     (log 2)))
(define (cdr-integer z)
  (/ (log (remove-n 2 z))
     (log 3)))

; Exercise 2.6
(define zero (lambda (f) (lambda (x) x)))
(define add-1 (lambda (n)
                (lambda (f)
                    (lambda (x) (f ((n f)x))))))
(define one (lambda (f)
              (lambda (x)
                (f x))))
(define two (lambda (f)
              (lambda (x)
                (f (f x)))))
(define plus (lambda (m)
               (lambda (n)
                 (lambda (f)
                   (lambda (x)
                     (m f (n f x)))))))

; interval-arithmetic
; Exercise 2.7
(define (make-interval a b) (cons a b))
(define (upper-bound i) (car i))
(define (lower-bound i) (cdr i))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((ll (* (lower-bound x) (lower-bound y)))
        (lu (* (lower-bound x) (lower-bound y)))
        (ul (* (upper-bound x) (lower-bound y)))
        (uu (* (upper-bound x) (upper-bound y))))
    (make-interval (min ll lu ul uu)
                   (max ll lu ul uu))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))
; Exercise 2.8
(define (sub-interval x y)
  (add-interval x
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))

; Exercise 2.10
(define (div-interval-im x y)
  (if (>= 0 (* (lower-bound y) (upper-bound y)))
        (error "interval spans 0")
        (div-interval x y)))

; Exercise 2.17
(define (last-pair items)
  (if (= (length items) 1)
    items
    (last-pair (cdr items))))

; Exercise 2.18
(define (reverse-list items)
  (define (rl-iter l result)
    (if (null? l) result
      (rl-iter (cdr l) (cons (car l) result))))
  (rl-iter items '()))

; Exercise  2.20
(define (same-parity . w)
  (let ((predicate (if (even? (car w)) 
                     even?
                     odd?)))
    (filter predicate w)))

(define (same-parity-im . w)
  (filter (if (even? (car w)) even? odd?) w))

; Exercise 2.21
(define (square-list items)
  (if (null? items)
    '()
    (cons (square (car items))
          (square-list (cdr items)))))
(define (square-list-by-map items)
  (map square items))

; Exercise 2.22
(define (square-list-iter items)
  (define (iter things answer)
    (if (null? things) answer
      (iter (cdr things)
            (cons answer
                  (square (car things))))))
  (iter items '()))

; Exercise 2.23
(define (i-for-each action items)
  (define (iter eles)
    (cond ((not (null? eles))
           (action (car eles))
           (iter (cdr eles)))))
  (iter items))

; count the leaves number of a tree
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

; Exercise 2.27
(define x (list (list 1 2 (list 3 4) 5) 6 7 (list 8 9 (list 10 11) 12) 13 14))
(define (deep-reverse tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) tree)
        (else
          (let ((drl (deep-reverse (cdr tree)))
                (drr (deep-reverse (car tree))))
            (append drl (list drr))))))

; Exercise 2.18
(define (i-reverse items)
  (if (null? items) (list)
    (append (i-reverse (cdr items))
          (list (car items)))))

; Exercise 2.28
(define (fringe tree)
  (define (iter st leaves)
      (cond ((null? st) '())
            ((not (pair? st)) (append leaves (list st)))
            (else (append leaves 
                          (iter (car st) leaves)
                          (iter (cdr st) leaves)))))
  (iter tree '()))

; Exercise 2.29
; Constructors and Selectors
(define (make-mobile left right)
  (list left right))
(define left-branch car)
(define right-branch cadr)
(define (make-branch len structure)
  (list len structure))
(define branch-length car)
(define branch-structure cadr)
(define (is-mobile? structure)
  (and (pair? structure)
       (pair? (car structure))))

(define (total-weight-ugly mobile)
  (define (iter st)
    (cond ((or (not (pair? st)) 
               (null? st)) 
           0)
          ((and (not (pair? (car st)))
                (not (pair? (cadr st))))
           (cadr st))
          (else (+ (iter (car st))
                   (iter (cdr st))))))
  (iter mobile))

(define (totle-weight node)
  (define (branch-weight branch)
    (if (null? branch) 0
     (let ((structure (branch-structure branch)))
      (if (not (is-mobile? structure)) structure ; if it's not a mobile, it must be weight value itself
        (+ (branch-weight (left-branch structure)); otherwise, get the total weight of this mobile node
           (branch-weight (right-branch structure)))))))
  (if (is-mobile? node)
      (+ (branch-weight (left-branch node))
         (branch-weight (right-branch node)))
      (branch-weight node)))
      ;(error "It seems the node you give is not a mobile, check it again please")))

  (define (totle-length node)
    (define (b-length branch)
        (if (null? branch) 0
          (let ((structure (branch-structure branch))
                (len (branch-length branch)))
            (if (is-mobile? structure)
              (+ len
                 (b-length (left-branch structure))
                 (b-length (right-branch structure)))
              len))))
    (if (is-mobile? node)
      (+ (b-length (left-branch node))
         (b-length (right-branch node)))
      (b-length node)))

;(define (balance? mobile)
  ;(cond ((not (is-mobile? mobile))
         ;(error "It seems the node you give is not like a mobile, check it again please"))
        ;(else (= 


(define 4labs (make-branch 1 4))
(define jira (make-branch 1 3))
(define das (make-branch 2 10))
(define suma (make-branch 2 7))
(define s (make-mobile 4labs jira))
(define j (make-mobile das suma))
(define lb (make-branch 3 s))
(define rb (make-branch 2 j))
(define pds (make-mobile lb rb))
