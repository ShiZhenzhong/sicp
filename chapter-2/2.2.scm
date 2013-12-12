(define (i-list-ref l n)
  (define (iter l n)
    (if (= n 0)
      (car l)
      (iter (cdr l) (- n 1))))
  (iter l n))

(define (i-length l)
  (define (iter items n)
    (if (null? items) n
      (iter (cdr items) (1+ n))))
  (iter l 0))

(define (i-append list1 list2)
    (if (null? list1) list2
      (cons (car list1) (i-append (cdr list1) list2))))
