(define (flipped-pairs painter)
  (let ((wave2 (beside painter (flip-vert painter))))
    (below wave2 wave2)))

(define (right-split painter n)
  (if (= n 0) painter
    (let ((smaller (right-split painter (-1+ n))))
      (beside painter (below smaller smaller)))))

(define (up-split painter n)
  (if (= n 0) painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

(define (corner-split painter n)
  (if (= n 0) painter
    (let ((up (up-split painter (-1+ n)))
          (right (right-split painter (-1+ n))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (-1+ n))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((corner (corner-split painter n)))
    (let ((top (beside (flip-horiz corner) corner)))
      (below (flip-vert top) top))))

; painter transform
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((coord-map ((frame-coord-map frame))))
      (let ((new-origin (coord-map origin)))
        (painter (make-frame new-origin
                             (sub-vect (coord-map corner1) new-origin)
                             (sub-vect (coord-map corner2) new-origin)))))))

(define (flip-vert painter)
    (transform-painter painter
                       (make-vect 0 1)
                       (make-vect 1 1)
                       (make-vect 0 0)))

(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vert 0 0)
                     (make-vert 1 1)))
(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1 0)
                     (make-vect 1 1)
                     (make-vect 0 0)))
(define (shrink-to-upper-right painter)
  (transform-painter painter
                     (make-vect .5 .5)
                     (make-vect 1 .5)
                     (make-vect .5 1)))
(define (beside p1 p2)
  (lambda (frame)
    (let ((left-paint (transform-painter p1
                                         (make-vect 0.0 0.0)
                                         (make-vect .5 0.0)
                                         (make-vect 0.0 1)))
          (right-paint (transform-painter p2
                                          (make-vect .5 0.0)
                                          (make-vect 1.0 0.0)
                                          (make-vect .5 1.0))))
      (left-paint frame)
      (right-paint frame))))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))
(define (rotate180 painter)
  ((repeated rotate90 2) painter))
(define (rotate270 painter)
  ((repeated rotate90 3) painter))

(define (below p1 p2)
  (let ((upper-paint (transform-painter p1
                                        (make-vect 0.0 .5)
                                        (make-vect 1.0 .5)
                                        (make-vect 0.0 1.0)))
        (lower-paint (transform-painter p2
                                        (make-vect 0.0 0.0)
                                        (make-vect 1.0 0.0)
                                        (make-vect 0.0 .5))))
    (lambda (frame)
      (upper-paint frame)
      (lower-paint frame))))

(define (below p1 p2)
  (let ((rp1 (rotate90 p1))
        (rp2 (rotate90 p2)))
    (let ((br (beside rp1 rp2)))
      (rotate270 br))))



