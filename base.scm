; some basic high-order procedures
(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (cond  ((<= n 0) (lambda (x) x))
         ((= n 1) f)
         (else (compose f (repeated f (-1+ n))))))
