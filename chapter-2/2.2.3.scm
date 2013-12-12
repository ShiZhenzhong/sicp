(define (enum-interval low high)
  (if (> low high) '()
    (cons low
          (enum-interval (1+ low) high))))

(define (accumulate op init sequence)
  (if (null? sequence) init
    (op (car sequence)
        (accumulate op init (cdr sequence)))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                    (enumerate-tree (cdr tree))))))

; Exercise 2.33
(define (a-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y))
              '()
              sequence))

(define (i-append seq1 seq2)
  (if (null? seq1)
    seq2
    (cons (car seq1)
          (i-append (cdr seq1) seq2))))

(define (a-append . s)
  (accumulate (lambda (x y)
                (define (iter s1 s2)
                  (if (null? s1) s2
                    (cons (car s1) (iter (cdr s1) s2))))
                (iter x y))
              '()
             s))

(define (i-length sequence)
  (if (null? sequence) 0
    (+ 1 (i-length (cdr sequence)))))

(define (a-length sequence)
  (accumulate (lambda (x y) 
                (+ y 1))
              0
              sequence))

; Exercise 2.34
(define (horner-eval x coefficient-seq)
  (accumulate (lambda (a r)
                (+ a (* x r)))
              0
              coefficient-seq))

; Exercise 2.35
(define (count-leaves t)
  (accumulate + 
              0
              (map (lambda (x) 1)
                   (enumerate-tree t))))

; Exercise 2.36
(define seqs (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)
                   (list 10 11 12)))

; **************************
; This does not work
(define (accumulate-n op init seqs)
  ;(display seqs)
  ;(newline)
  (if (null? (car seqs))  '()
    (let ((rest (accumulate-n op init (cdr seqs))))
    (cons (accumulate op init (cons (caar seqs)
                                    (car rest)))
          (accumulate-n op init (list (cdar seqs)
                                      (cdr rest)))))))
; **************************
(define (accumulate-n op init seqs)
  (if (null? (car seqs)) '()
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(define (seq-add seqs)
  (accumulate-n + 0 seqs))

; Exercise 2.37
(define m (list (list 1 2 3 4)
                (list 2 3 4 5)
                (list 3 4 5 6)))
(define n (list (list 1 2 1)
                (list 2 3 2)
                (list 3 4 3)
                (list 4 5 4)))
(define v (list 2 3 4 5))
(define w (list 3 4 5 6))
; vector * vector 
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
; matrix * vector
(define (m-*-v m v)
  (map (lambda (row)
         (accumulate +  0 (map * v row)))
       m))
; matrix transpose
(define (transpose m)
  (accumulate-n cons '() m))
; matrix * matrix
(define (m-*-m m n)
  (let ((cols (transpose n)))
    (map (lambda (row)
           (map (lambda (col)
                  (dot-product row col))
                cols))
         m)))

; Exercise 2.39
(define (reverse-by-fold-right seq)
  (fold-right (lambda (x y)
                (append y (list x)))
              '()
              seq))
(define (reverse-by-fold-left seq)
  (fold-left (lambda (x y)
               (cons y x))
             '()
             seq))

(define (prime-sum-pairs n)
  (define (all-pairs)
    (accumulate append '()
        (map
          (lambda (i)
            (map (lambda (j) (list i j))
              (enum-interval 1 (-1+ i))))
          (enum-interval 1 n))))

  (all-pairs))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (permutations s)
  (if (null? s) (list '())
    (flatmap
      (lambda (x)
        (map (lambda (y) (cons x y))
             (permutations (filter (lambda (r) (not (= r x))) 
                                   s))))
      s)))

; Exercise 2.40
; see all-pairs above
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enum-interval 1 (-1+ i))))
           (enum-interval 1 n)))

; Exercise 2.41
(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (j) (cons i j))
                  (unique-pairs (-1+ i))))
           (enum-interval 1 n)))
(define (triples-of-s n s)
  (filter (lambda (t)
            (= s (accumulate + 0 t)))
          (unique-triples n)))

; Exercise 2.42 Queens
(define (queens board-size)
  (define empty-board '())
  (define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens (list (cons k new-row))))

  (define (safe? k positions)
    (define (logic-accumulate seq)
      (accumulate (lambda (x y) (and x y)) true seq))
    (define (position-value row col)
      (+ row (* board-size (-1+ col))))

    (let ((new-added-row (cdar (reverse positions)))
          (base-remainder (list (-1+ board-size) board-size (1+ board-size))))
        (let ((new-pv (position-value new-added-row k)))
          (logic-accumulate
            (map (lambda (position)
                   (let ((r (cdr position))
                         (c (car position)))
                     (logic-accumulate
                       (map (lambda (br)
                              (let ((pv-difference (- new-pv (position-value r c)))
                                    (col-difference (- k c)))
                                (not (= pv-difference (* br col-difference)))))
                            base-remainder))))
                 (cdr (reverse positions)))))))

  (define (queen-cols k)
    (if (= k 0) (list empty-board)
      (filter
        (lambda (positions)
          (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enum-interval 1 board-size)))
          (queen-cols (-1+ k))))))
  (draw-queens-position (queen-cols board-size) board-size))

(define (draw-queens-position solutions board-size)
    (define (draw-header)
      (for-each display (map (lambda (x) "----") (enum-interval 1 board-size)))
      (newline))
    (define (draw-row queen-position)
      (let ((bs (enum-interval 1 board-size)))
          (for-each display (map (lambda (x)
                                         (if (= 1 x) (display "|") (display ""))
                                         (if (= queen-position x) " O |" "   |"))
                                 bs))
          (newline)
          (for-each display (map (lambda (x) "----") bs))
          (newline)))
    (for-each (lambda (s)
                (display "Queens Positions:")
                (newline)
                (draw-header)
                (for-each (lambda (row) (draw-row  (cdr row))) 
                          s)) 
              solutions)
    (display (length solutions))
    (display " Solutions Found!"))
