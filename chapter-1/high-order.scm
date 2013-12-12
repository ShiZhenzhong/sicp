;; some basic function will be used in this file
(define (average x y) (/ (+ x y) 2))
(define (square x) (* x x))
(define accuracy .0000000001)

; Fixed point
(define (fixed-point f start)
  (define tolerance accuracy)
  (define (close-enuf? old new)
    (< (abs (- new old)) tolerance))
  (define (fp-iter old new)
    (newline)
    (display new)
      (if (close-enuf? old new)
          new
          (fp-iter new  (f new))))
  (fp-iter start (f start)))

; sq with fixed-point function
; use function f(y) = (y + x / y) / 2  the fixed point of f is the root square
(define (sq x)
  (fixed-point (lambda (y) (/
                             (+ y
                                (/ x y))
                             2))
               1.0))

; use average damping and function a simpler function  f(y) = x / y
(define (sq-new x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

(define average-damp
  (lambda (f)
    (lambda (y) (average y (f y)))))

; Newton's method to find the square root of x
(define (newton-method f guess)
  (define dx accuracy)
  (define derivative (lambda (f)
               (lambda (x)
                 (/ (- (f (+ x dx))
                       (f x))
                    dx))))
  (define df (derivative f))
  (fixed-point (lambda (x)
                    (- x (/ (f x) 
                            (df x))))
               guess))

(define (sq-newton x)
  (newton-method (lambda (y) (- x (square y)))
                 1.0))
