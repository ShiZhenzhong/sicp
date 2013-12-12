; Chapter 2.4  genetic operations
(load "../base.scm")
(load "generic.scm")

;;;; Exercise 2.84
; a more generic version of apply-generic which can handle different type of data
(define (apply-generic op . args)
  (let ((coerced-list (apply raise-all args)))
    (let ((args-type-list (map type coerced-list)))
      (let ((proc (get op args-type-list)))
        (if proc 
          (let ((result (apply proc (map content coerced-list))))
            (drop result))
          (error "No operator found"))))))

(define (raise obj)
  ((get 'raise (type obj)) obj))
(define (raise-all . args)
  (apply (get 'raise 'all-type) args))

; Exercise 2.83 
(define (install-raises)
  (define type-tower (list 'integer 'rational 'complex))
  (define (coercion-weight type)
    (define (iter i l)
      (cond ((null? l) -1)
            ((eq? type (car l)) i)
            (else (iter (1+ i) (cdr l)))))
    (iter 0 type-tower))

  (define (raise-all . args)
    (let ((c-weight (map coercion-weight (map type args))))
      (if (< (apply min c-weight) 0) args
        (let ((roof (apply max (map coercion-weight (map type args)))))
          (map (lambda (n) 
                 (let ((steps (- roof (coercion-weight (type n)))))
                   ((repeated raise steps) n)))
               args)))))

  (define (raise-integer->rational i)
    (make-rational i 1))
  (define (raise-rational->complex r)
    (make-from-real-imag (/ (rat-numer r)
                            (rat-denom r))
                         0))
  (put 'raise 'integer raise-integer->rational)
  (put 'raise 'rational raise-rational->complex)
  (put 'raise 'all-type raise-all)
  'done)

(define (install-drop)
  (define (project-complex z)
    (make-rational (real z) 1))
  (define (project-rational r)
    (round (/ (rat-numer r)
              (rat-denom r))))

  (put 'project 'complex project-complex)
  (put 'project 'rational project-rational)
  (put 'project 'integer (lambda (x) (-1+ x))) ; integer is our lowest drop level, make it not be itself to give information to drop, in order to terminate drop process
  'done)

(define (project obj)
  (let ((proj-proc (get 'project (type obj))))
    (if proj-proc (proj-proc obj)
      false)))

; drop an object as much as it could be
(define (drop obj)
  (let ((dropped (project obj)))
    (if (not dropped) ob
        (if (equ? dropped obj)
          (drop dropped)
          obj))))

(install-raises)
(install-drop)
