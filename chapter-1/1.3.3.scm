(define (search f neg pos)
  (define (close-enuf?)
    (< (abs (- pos neg)) .0001))
  (let ((mid (/ (+ neg pos) 2)))
    (if (close-enuf?) mid
          (let ((test-value (f mid)))
            (cond ((positive? test-value)(search f neg mid))
                  ((negative? test-value)(search f mid pos))
                  (else mid))))))

(define (half-interval-method f a b)
  (let ((left-value (f a))
        (right-value (f b)))
    (cond ((and (positive? right-value) (negative? left-value))
           (search f a b))
          ((and (positive? left-value) (negative? right-value))
           (search f b a))
          (else 
            (error "Values are not of opposite sign" a b)))))

; modified fixed-point procedure
(define tolerance .00001)
(define (close-enuf? old new)
  (< (abs (- new old)) tolerance))
(define fixed-point
  (lambda (f guess)
    (define try 
      (lambda (g)
         (newline)
         (display g)
         (let ((next (f g)))
            (if (close-enuf? guess next) next
                (fixed-point f next)))))
     (try guess)))

; Exercise 1.36
(define (magic-x x)
  (fixed-point (lambda (y) (/ (log x) (log y))) 10.0))

(define (magic-x-ave x)
  (define (average x y)
    (/ (+ x y) 2))
  (fixed-point (lambda (y) (average y
                                    (/ (log x)
                                       (log y)))) 10.0))

; Exercise 1.37
(define (cont-frac n d k)
  ; recursion version
  (define (cont-frac-r c)
    (if (= c k) (/ (n k) (d k))
      (/ (n c)
         (+ (d c)
            (cont-frac-r (+ c 1))))))
  (cont-frac-r 1))
(define (cont-frac-i n d k)
  ; iterative version
  (define (cont-frac-iterative i result)
    (if (= i 0) result
      (cont-frac-iterative (- i 1)
                           (/ (n i)
                              (+ (d i)
                                 result)))))
  (cont-frac-iterative k 0.0))


(define (gold n)
  (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             n))
(define (gold-i n)
  (cont-frac-i (lambda (i) 1.0)
               (lambda (i) 1.0)
               n))

; Exercise 1.38
(define (get-e n)
  (+ 2.0 (cont-frac-i 
           (lambda (i) 1.0)
           (lambda (i)
             (if (or (= 0 (remainder i 3))
                     (= 0 (remainder (- i 1) 3)))
                 1
                 (* 2 (ceiling (/ i 3)))))
           n)))

; Exercise 1.39
(define (tan-cf x n)
  (cont-frac-i (lambda (i) 
                 (if (= i 1) x
                   (square x)))
               (lambda (i)
                 (- (* 2 i) 1))
               n))

