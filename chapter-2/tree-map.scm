(define (scale-tree tree factor)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-map tree factor)
  (map (lambda (sub-tree )
         (if (pair? sub-tree)
           (scale-tree-map sub-tree factor)
           (* sub-tree factor)))
       tree))

(define tree (list 1 (list 2 3) (list 4 5 (list 6 7) 8) 9 10))

; Exercise 2.30
(define (square-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
              (square-tree (cdr tree))))))

(define (square-tree-map tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree)) (square sub-tree)
           (square-tree-map sub-tree)))
       tree))

;Exercise 2.31
(define (tree-map action tree)
  (map (lambda (sub-tree)
         (if (not (pair? sub-tree)) (action sub-tree)
           (tree-map action sub-tree)))
       tree))

; Exercise 2.32
(define (subsets s)
  (if (null? s) (list s)
    (let ((rest (subsets (cdr s))))
      (append rest 
              (map (lambda (ele)
                       (cons (car s) ele))
                   rest)))))
