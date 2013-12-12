(load "high-order.scm")

(define (deriv f)
  (lambda (x)
      (let ((dx 0.00001))
        (/ (- (f (+ x dx)) (f x))
           dx))))

(define (newton-transform g)
  (lambda (x)
    (- x
       (/ (g x)
          ((deriv g) x)))))

(define (sqrt-newton x)
  (fixed-point (newton-transform (lambda (y) (- (square y) x))) 
               1.0))

; Exercise 1.40
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x) (* a x x) (* b x) c)))

; Exercise 1.41
(define (double g)
  (lambda (x)
    (g (g x))))

; Exercise 1.42
(define (compose f g)
  (lambda (x)
    (f (g x))))
; so double is (compose g g)

; Exercise 1.43
(define (repeated f n)
  (define (func times)
                (if (= times 0) (lambda (x) x)
                  (compose f (func (- times 1)))))
  (lambda (x)
    ((func n) x)))

; Exercise 1.44
(define (smooth f)
  (lambda (x)
    (let ((dx .001))
      (/ (+ (f x)
            (f (- x dx))
            (f (+ x dx)))
         3))))

(define (smooth-n f times)
  ((repeated smooth times) f))

; Exercise 1.45
(load "exp.scm")
(define (n-root x n)
  (let ((times (if (<= (- n 2) 0) 1
                    (- n 2))))
      (fixed-point ((repeated 
                      average-damp 
                      times) (lambda (y) (/ x (fast-exp-square y (- n 1)))))
               1.0)))

; Exercise 1.46
(define (iterative-improve enuf? improve)
  (define (iter guess)
    (if (enuf? guess) guess
      (iter (improve guess))))
  (lambda (x)
    (iter x)))

(define (sqrt-im x)
  ((iterative-improve (lambda (g)
                        (< (abs (- (square g) x)) 0.000000001))
                      (lambda (g)
                        (/ (+ g (/ x g)) 2)))
   1.0))

(define (fixed-point-im f)
  ((iterative-improve (lambda (g)
                        (< (abs (- (f g) g)) 0.0000001))
                      (lambda (g)
                        (f g)))
   1.0))

(define (sqrt-fixed-point-im x)
  (fixed-point-im (average-damp (lambda (y) (/ x y)))))
