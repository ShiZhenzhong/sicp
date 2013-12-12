; Exercise 2.12
(define make-interval cons)
(define lower-bound car)
(define upper-bound cdr)
(define (display-interval x)
  (newline)
  (display "[")
  (display (lower-bound x))
  (display " , ")
  (display (upper-bound x))
  (display "]"))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((ll (* (lower-bound x) (lower-bound y)))
        (lu (* (lower-bound x) (upper-bound y)))
        (ul (* (upper-bound x) (lower-bound y)))
        (uu (* (upper-bound x) (upper-bound y))))
    (make-interval (min ll lu ul uu)
                   (max ll lu ul uu))))
(define (div-interval x y)
  (if (>= 0 (* (lower-bound y) 
               (upper-bound y)))
    (error "WARNING: interval spans 0!")
    (mul-interval x 
                  (make-interval (/ 1. (upper-bound y))
                                 (/ 1. (lower-bound y))))))
(define (sub-interval x y)
  (add-interval x 
                (make-interval (- (upper-bound y))
                               (- (lower-bound y)))))
(define (make-center-percent c p)
  (let ((deviation (* c p)))
       (make-interval (- c deviation)
                      (+ c deviation))))
(define (center i)
  (/ (+ (lower-bound i)
        (upper-bound i))
     2))
    
(define (percent i)
  (/ (- (upper-bound i)
        (lower-bound i))
     (* 2 (center i))))

; Exercise 2.13
; ( (1 + p1)*(1 + p2) - (1 - p1) * (1 - p2) ) / ( (1 + p1) * (1 + p2) + (1 - p1) * (1 - p2))
(define (mul-percent x y)
  (/ (- (* (1+ (percent x))
           (1+ (percent y)))
        (* (-1+ (percent x))
           (-1+ (percent y))))
     (+ (* (1+ (percent x))
           (1+ (percent y)))
        (* (-1+ (percent x))
           (-1+ (percent y))))))

(define (part1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))
(define (part2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval one
                  (add-interval (div-interval one r1)
                                (div-interval one r2)))))