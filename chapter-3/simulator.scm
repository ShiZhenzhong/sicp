(load "queue.scm")

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (and-gate a b c)
    (or-gate a b d)
    (inverter c e)
    (and-gate d e s)
    'done))

(define (full-adder a b cin sum cout)
  (let ((d (make-wire))
        (e (make-wire))
        (f (make-wire)))
    (half-adder b cio f d)
    (half-adder a d sum e)
    (or-gate e f cout)
    'ok))

(define (inverter input output)
  (define (logic-not i)
    (cond ((= i 0) 1)
          ((= i 1) 0)
          (else (error "Invalid signal" i))))
  (define (inverter-action)
    (let ((new-value (logic-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input inverter-action)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-logic s1 s2)
    (cond ((and (= s1 1) (= s2 1)) 1)
          ((or (and (= s1 1) (= s2 0))
               (and (= s1 0) (= s2 1))
               (and (= s1 0) (= s2 0)))
           0)
          (else (error "Invalid signal for -- AND-LOGIC"))))
  (define (and-action)
    (let ((s1 (get-signal a1))
          (s2 (get-signal a2)))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output (and-logic s1 s2))))))
  (add-action! a1 and-action)
  (add-action! a2 and-action)
  'ok)

(define (or-gate o1 o2 output)
  (define (or-logic s1 s2)
    (cond ((and (= s1 0) (= s2 0)) 0)
          ((or (and (= s1 1) (= s2 1))
               (and (= s1 0) (= s2 1))
               (and (= s1 1) (= s2 0)))
           1)
          (else (error "Invalid signals for -- OR-LOGIC"))))
  (define (or-action)
    (let ((s1 (get-signal o1))
          (s2 (get-signal o2)))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output (or-logic s1 s2))))))
  (add-action! o1 or-action)
  (add-action! o2 or-action)
  'ok)

(define (call-cach proces)
  (if (null? proces) 'done
    (begin ((car proces))
           (call-cach (cdr proces)))))

(define (make-wire)
  (let ((signal 0) (action-procedures '()))
    (define (get-signal) signal)
    (define (set-signal! sig) 
      (set! signal sig)
      (call-each action-procedures)
      'done)
    (define (add-action! action)
      (set! action-procedures (cons action action-procedures))
      (action))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) get-signal)
            ((eq? m 'set-signal!) set-signal!)
            ((eq? m 'add-action!) add-action!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))
(define (add-action! wire proc) ((wire 'add-action!) proc))
(define (get-signal wire) ((wire 'get-signal)))
(define (set-signal! wire signal) ((wire 'set-signal) signal))

(define (make-time-segment time queue) (cons time queue))
(define (segment-time seg) (car seg))
(define (segment-queue seg) (cdr seg))
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-agenda-time! agenda time) (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-agenda-segments! agenda segs) (set-cdr! agenda segs))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "agenda is empty" agenda)
    (let ((first-seg (first-segment agenda)))
      (set-agenda-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-agenda-segments! (rest-segments agenda)))))

(define (add-to-agenda! time action agenda)
  (define (blongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-time-segment time action)
    (let ((task-queue (make-queue)))
      (insert-queue! task-queue action)
      (make-time-segment time task-queue)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (blongs-before? rest)
          (set-cdr! segments
                    (cons (make-time-segment time action)
                          rest))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (blongs-before? segments)
      (set-agenda-segments! agenda
                            (cons (make-time-segment time action)
                                  segments))
      (add-to-segments! segments))))


(define (after-delay delay-time action)
  (add-to-agenda! (+ delay-time (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))