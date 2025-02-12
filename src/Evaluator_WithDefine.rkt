#lang racket

(define frame0
  (list
   (cons '+ +)
   (cons '- -)
   (cons '* *)
   (cons '/ /)))

(define env0
  (list frame0))

(define (apply-primop op args)
  (op (car args) (cadr args)))

; Eval func with define
(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cadr exp))
          ((eq? (car exp) 'lambda)
           (list 'closure (cdr exp) env))
          ((eq? (car exp) 'define)
           (define-var (cadr exp)
             (eval (caddr exp) env)
             env))
          ((eq? (car exp) 'cond)
           (evcond (cdr exp) env))
          (else
           (apply (eval (car exp) env)
                  (evlist (cdr exp) env))))))

(define define-var
  (lambda (var val env)
    (let ((frame (car env)))
      (if (eq? (assq var frame) '())
          (set-mcar! env (cons (cons var val ) frame))
          (set-mcdr! (assq var frame) val)))))

; Default apply func
(define apply
  (lambda (proc args)
    (cond ((primitive? proc)
           (apply-primop proc args))
          ((eq? (car proc) 'closure)    ; (closure ((x) (+ x y)) <env>)
           (eval (cadadr proc)
                 (bind (caadr proc)
                       args
                       (caddr proc))))
          (else error))))

(define evlist
  (lambda (L env)
    (cond ((eq? L '()) '())
          (else
           (cons (eval (car L) env)
                 (evlist (cdr L) env))))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (eval (caar clauses) env))
           (evcond (cdr clauses) env))
          (else
           (eval (cadar clauses) env)))))

(define bind
  (lambda (vars vals env)
    (cons (pair-up vars vals)
          env)))

(define pair-up
  (lambda (vars vals)
    (cond
      ((eq? vars '())
       (cond ((eq? vals '()) '())
             (else (error 'TMA))))
      ((symbol? vars)
       (cons (cons vars vals) '()))
      ((eq? vals '()) (error 'TFA))
      (else
       (cons (cons (car vars)
                   (car vals))
             (pair-up (cdr vars)
                      (cdr vals)))))))

(define lookup
  (lambda (sym env)
    (cond ((eq? env '()) (error 'UBV))
          (else
           ((lambda (vcell)
              (cond ((eq? vcell '())
                     (lookup sym
                             (cdr env)))
                    (else (cdr vcell))))
            (assq sym (car env)))))))

(define assq
  (lambda (sym alist)
    (cond ((eq? alist '()) '())
          ((eq? sym (caar alist))
           (car alist))
          (else
           (assq sym (cdr alist))))))

(eval '(

        (define pow-num
          (lambda (x)
            (* x x)))

        (pow-num 10)

        ) env0)