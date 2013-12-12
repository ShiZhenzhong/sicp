; basic procedures
(define (square x) (* x x))
(define (double x) (+ x x))
(define (halve x) (/ x 2))

; exp recursive version
(define (exp-r x n)
  (if (= n 0) 1
    (* x (exp-r x (- n 1)))))

; exp iterative version
(define (exp-i x n)
    (define (exp-iter x n p)
      (if (= n 0) p
        (exp-iter x (- n 1) (* x p))))
  (exp-iter x n 1))

; fast exp version
(define (fast-exp x n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp x (/ n 2))))
        (else (* x (fast-exp x (- n 1))))))

; fast exp by successive square
; Exercise 1.16
(define (fast-exp-square x n)
  (cond ((= 0 n) 1)
        ((even? n) (fast-exp-square (square x) (/ n 2)))
        (else (* x (fast-exp-square x (- n 1))))))

; Exercise 1.17
(define (*-by-+ x y)
  (cond ((= y 1) x)
        (else (+ x (*-by-+ x (- y 1))))))

(define (fast*-by-+ x y)
  (cond ((= y 0) 0)
        ((= y 1) x)
        ((even? y) (double (fast*-by-+ x (halve y))))
        (else (+ x (fast*-by-+ x (- y 1))))))

; Exercise 1.18
(define (smart* x y)
  (cond ((= y 0) 0)
        ((= y 1) x)
        ((even? y) (* (double x) (halve y)))
        (else (+ x (smart* x (- y 1))))))

; Exercise 1.19
; fib Q-matrix function
(define (fib-matrix n)
  (define (fib-iter a b p q c)
    (cond ((= c 0) b)
          ((even? c) (fib-iter a b (+ (square p) (square q)) (+ (* 2 p q) (square q)) (/ c 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p)) (+ (* b p) (* a q)) p q (- c 1)))))
  (fib-iter 1 0 0 1 n))

; recursive fib function
(define (fib-r n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib-r (- n 1)) (fib-r (- n 2))))))

; iterative fib function
(define (fib-i n)
  (define (fib-iter a b c)
    (if (= c 0) b
        (fib-iter (+ a b) a (- c 1))))
  (fib-iter 1 0 n))
