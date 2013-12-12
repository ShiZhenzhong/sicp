;Exercise 2.54
(define (i-equal? list1 list2)
  (cond ((and (null? list1) (null? list2)) true)
        ((let ((car1 (car list1))
               (car2 (car list2)))
           (if (and (pair? car1) (pair? car2))
                 (and (i-equal? car1 car2)
                      (i-equal? (cdr list1) (cdr list2)))
               (and (eq? car1 car2)
                    (i-equal? cdr list1) (cdr list2)))))))

