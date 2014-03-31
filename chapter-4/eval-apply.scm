(define (evali exps env)
  <body>
  )

(define (applyi procedure args)
  <body>
  )

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (evali (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if if-exp env)
  (if (true? (evali (if-predicate if-exp) env))
    (evali (if-consequent if-exp) env)
    (evali (if-alternative if-exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (evali (first-exp exps) env))
        (else (evali (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment expr env)
  (set-variable-value! (assignment-variable expr)
                       (evali (assignment-value expr) env)
                       env)
  'ok)

(define (eval-definition expr env)
  (define-variable! (definition-variable expr)
                    (evali (definition-value expr) env)
                    env)
  'ok)

; Exercise 4.1
(define (list-of-values exps env)

