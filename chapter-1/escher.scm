;(define (make-vec x y) (cons x y))
;(define (xcor p) (car p))
;(define (ycor p) (cdr p))
(define make-vec cons)
(define xcor car)
(define ycor cdr)

(define make-segment cons)
(define seg-start car)
(define seg-end cdr)

(define (+vect x y) 
    (make-vec 
      (+ (xcor x) (xcor y))
      (+ (ycor x) (ycor y))))

(define (scale v factor)
  (make-vec (* factor (xcor v))
            (* factor (ycor v))))

; rectangle constructor
(define make-rect ())
; rectangle seletors
(define (horiz rect) ())
(define (vert rect) ())
(define (origin rect) ())

; rectangle transformation
(define (coord-map rect)
  (lambda (point)
    (+vect 
      (+vect
       (scale (xcor point)
              (horiz rect))
       (scale (ycor point)
              (vert rect)))
      (origin rect))))

; picture constructor
(define (make-picture segmentList)
  (lambda (rect)
    (for-each (lambda (s)
                (drawline 
                  ((coord-map rect) (seg-start s))
                  ((coord-map rect) (seg-end s))))
              segmentList)))

;(define R (make-rect (args here)))
;(define G (make-picture (args here)))
;(G R) to get a picture inside its rectangle

(define (beside p1 p2 factor)
  (lambda (rect)
    (p1 (make-rect 
          (origin rect)
          (scale factor (horiz rect))
          (vert rect)))
    (p2 (make-rect
          (+vert (origin rect)
                 (scale factor (horiz rect)))
          (scale (- 1 factor) (horiz rect))
          (vert rect)))))

(define (rotate pict)
  (lambda (rect)
    (pict (make-rect
            ; make rect here
            ))))

(define (right-push pict n a)
  (if (= n 0) 
      pict
      (beside pict
              (right-push pict (- n 1) a)
              a)))

(define (push comb)
  (lambda (pict N A)
    ((repeated (lambda (p)
                 (comb pict p A))
              N)
    pict)))

(define (i-map f l)
  (if (null? l) '()
      (cons (f (car l))
            (i-map f (cdr l)))))

