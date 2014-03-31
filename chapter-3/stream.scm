(define (expmod base e m)
  (cond ((= e 0) 1)
        ((even? e)
         (remainder (square (expmod base (/ e 2) m)) m))
        (else
         (remainder (* base (expmod base (- e 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (1+ (random (-1+ n)))))

(define (prime? n)
    (define (iter n times)
      (cond ((= times 0) true)
            ((fermat-test n) (iter n (-1+ times)))
            (else false)))
    (iter n 5))

(define (stream-car s) (car s))
(define (stream-cdr s) (force (cdr s)))

(define the-empty-stream '()) 
(define empty-stream? null?)

(define (stream-map proc stream)
  (if (empty-stream? stream) the-empty-stream
    (cons-stream (proc (stream-car stream))
                 (stream-map proc (stream-cdr stream)))))

(define (stream-ref stream n)
  (if (= n 0) (stream-car stream)
    (stream-ref (stream-cdr stream) (- n 1))))

(define (stream-for-each proc stream)
  (if (empty-stream? stream) 'done
    (begin (proc (stream-car stream))
           (stream-for-each proc (stream-cdr stream)))))

(define (display-line l)
  (newline)
  (display l))
(define (display-stream stream)
  (stream-for-each display-line stream))

(define (stream-enum-interval low high)
  (if (> low high) the-empty-stream
    (cons-stream low 
                 (stream-enum-interval (1+ low) high))))

(define (ints-from from)
  (cons-stream from (ints-from (1+ from))))

(define (stream-filter pred stream)
  (cond ((empty-stream? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (second-prime-from from)
  (stream-car
    (stream-cdr
      (stream-filter prime? (ints-from from)))))

; Exercise 3.51
(define (show x) (display-line x) x)
(define x (stream-map show (stream-enum-interval 0 10)))
;(stream-ref x 5)
;(stream-ref x 7)

(define (stream-mmap proc . streams)
  (if (empty-stream? (car streams)) the-empty-stream
    (cons-stream (apply proc (map stream-car streams))
                 (apply stream-mmap
                        (cons proc (map stream-cdr streams))))))

(define ones (cons-stream 1 ones))
(define (stream-add s1 s2) (stream-mmap + s1 s2))
(define integers (cons-stream 1 (stream-add ones integers)))
(define fibs (cons-stream 0
                          (cons-stream 1
                                       (stream-add (stream-cdr fibs)
                                                   fibs))))

(define (scale-stream stream scalar)
  (stream-map (lambda (x) (* x scalar)) stream))
(define double (cons-stream 1 (scale-stream double 2)))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))
