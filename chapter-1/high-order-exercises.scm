(define (sum-i a b)
  (if (> a b) 0
    (+ a (sum-i (+ 1 a) b))))

(define (sum-square a b)
  (if (> a b) 0
    (+ (square a) (sum-square (+ a 1) b))))

(define (sum-pi a b)
  (if (> a b) 0
    (+ (/ 1.0 (* a (+ a 2))) (sum-pi (+ a 4) b))))

; high-order abstraction
(define (inc x) (+ 1 x))
(define (sum term a next b)
  (if (> a b) 0
    (+ (term a)
       (sum term (next a) next b))))

(define (h-sum-i a b)
  (sum (lambda (x) x)
       a
       inc
       b))

(define (h-sum-square a b)
  (sum square
       a
       inc
       b))
(define (h-sum-pi a b)
  (sum (lambda (x)
         (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(define cube
  (lambda (x) (* x x x)))

; Exercise 1.29
(define (integral f a b dx)
  (* dx
     (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ dx x))
          b)))

(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (coefficient i)
    (cond ((or (= i n)
               (= i 0)) 1)
          ((even? i) 2)
          (else 4)))
  (* (/ h 3)
     (sum (lambda (x) (* (coefficient x)
                         (f (+ a (* x h)))))
          0
          inc
          n)))

; Exercise 1.30
; Abstract sum with a iterative way
(define (sum-iterative term a next b)
  (define (iter a result)
    (if (> a b) result
      (else (iter (next a) (+ result (term a))))))
  (iter a 0))

; Exercise 1.31
(define (product term a next b)
  (if (> a b) 1
    (* (term a)
       (product term (next a) next b))))

; iterative version
(define (product-iterative term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product-iterative (lambda (x) x)
           1
           inc
           n))

(define (pi n)
  (* 4 (product-iterative 
         (lambda (i)(* (/ (+ i 1.0) (+ i 2))
                       (/ (+ i 3) (+ i 2))))
           1
           (lambda (i) (+ i 2))
           n)))

;Exercise 1.32
(define (accumulate combiner null-value term a next b)
  (if (> a b) null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iterative combiner null-value term a next b)
  (define (iter a result)
    (if (> a b) result
      (iter (next a)
            (combiner (term a) result))))
  (iter a null-value))

(define (sum-accu a b)
  (accumulate + 0 (lambda (x) x) a inc b))
(define (product-accu a b)
  (accumulate * 1 (lambda (x) x) a inc b))

; Exercise 1.33
(define (filtered-accumulate combiner filter-predicate null-value term a next b)
  (cond ((> a b) null-value)
        ((not (filter-predicate a)) (filtered-accumulate combiner
                                                         filter-predicate
                                                         null-value
                                                         term
                                                         (next a)
                                                         next
                                                         b))
       (else (combiner (term a)
                (filtered-accumulate combiner
                                     filter-predicate
                                     null-value
                                     term
                                     (next a)
                                     next
                                     b)))))

(define (filtered-accu-iterative combiner predicate null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((not (predicate a)) (iter (next a) result))
          (else (iter (next a) (combiner (term a) result)))))
  (iter a null-value))

(define (sum-of-even a b)
  (filtered-accumulate + even? 0 (lambda (x) x) a inc b))

(define (sum-of-odd a b)
  (filtered-accu-iterative + odd? 0 (lambda (x) x) a inc b))
