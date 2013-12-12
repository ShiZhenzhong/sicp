(define (smallest-divisor n)
  (define sd-iter 
    (lambda (test n)
      (cond ((> (square test) n) n)
            ((= (remainder n test) 0) test)
            (else (sd-iter (+ test 1) n)))))
  (sd-iter 2 n))

; normal test
(define (prime? n)
  (if (= (smallest-divisor n) n)
        true
        false))

; Fermat test procedure
(define expmod
  (lambda (base e m)
    (cond ((= e 0) 1)
          ((even? e)
           (remainder (square (expmod base (/ e 2) m))
                      m))
          (else (remainder (* base (expmod base (- e 1) m))
                           m)))))

(define (fermat-test n)
  (define (try-it a)
      (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

; exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define start-prime-test 
  (lambda (n start-time)
    (cond ((fast-prime? n 5)
            (report-prime (- (runtime) start-time)) true)
          (else false))))

(define (report-prime elapsed-time)
  (display " * * * ")
  (display elapsed-time))

(define (from start)
    (cond ((< start 3) 3)
      ((even? start) (+ start 1))
      (else start)))

(define (search-for-primes start end)
  (define (sfp s e)
    (cond ((> s e) (display "done"))
          (else (timed-prime-test s)
                (sfp (+ s 2) e))))
  (sfp (from start) end))

(define (search-primes start n)
  (define (sfp s n)
    (cond ((= n 0) (display "done"))
          ((timed-prime-test s) (sfp (+ s 2) (- n 1)))
          (else (sfp (+ s 2) n))))
  (sfp (from start) n))

