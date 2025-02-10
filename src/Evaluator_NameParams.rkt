#lang racket

(define (apply-primop) '())

;; eval func with args name past feature
(define eval
  (lambda (exp env)
    (cond ((number? exp) exp)
          ((symbol? exp) (lookup exp env))
          ((eq? (car exp) 'quote) (cadr exp))
          ((eq? (car exp) 'lambda)
           (list 'closure (car exp) env))
          ((eq? (car exp) 'cond)
           (evcond (cdr exp) env))
          (else
           (apply (undelay (eval (car exp)
                                 env))
                  (cdr exp)
                  env)))))

;; apply func with args name past feature
(define apply
  (lambda (proc ops env)
    (cond ((primitive? proc)
           (apply-primop proc
                         (evlist ops env)))
          ((eq? (car proc) 'closure)    ; (closure ((x) (+ x y)) <env>)
           (eval (cadadr proc)
                 (bind (vnames (caadr proc))
                       (gevlist (caadr proc)
                                ops
                                env)
                       (caddr proc))))
          (else error))))

(define vnames
  (lambda (params)
    (cond ((null? params) '())
          ((pair? (car params)) ; if param is a pair, like:(name x)
           (cons (cadr (car params)) (vnames (cdr params))))
          (else (cons (car params) (vnames (cdr params)))))))

(define evlist
  (lambda (L env)
    (cond ((eq? L '()) '())
          (else
           (cons (undelay (eval (car L) env))
                 (evlist (cdr L) env))))))

(define gevlist
  (lambda (vars exps env)
    (cond
      ((eq? exps '()) '())
      ((symbol? (car vars))
       (cons (eval (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      ((eq? (caar vars) 'name)
       (cons (make-delay (car exps) env)
             (gevlist (cdr vars)
                      (cdr exps)
                      env)))
      (else error))))

(define evcond
  (lambda (clauses env)
    (cond ((eq? clauses '()) '())
          ((eq? (caar clauses) 'else)
           (eval (cadar clauses) env))
          ((false? (undelay
                    (eval (caar clauses)
                          env)))
           (evcond (cdr clauses) env))
          (else
           (eval (cadar clauses) env)))))

(define make-delay
  (lambda (exp env)
    (cons 'thunk (cons exp env))))

(define (undelay v)
  (cond ((pair? v)
         (cond ((eq? (car v) 'thunk)
                (undelay
                 (eval (cadr v)
                       (cddr v))))
               (else v)))
        (else v)))

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

; (define (unless p c a)
;   (cond ((not p) c)
;         (else a)))

(unless (= 1 0) 2 (/ 1 0))  ;; this will be error

(cond ((not (= 1 0)) 2)     ;; this will run well
      (else (/ 1 0)))

(define (unless p (name c) (name a))
  (cond ((not p) c)
        (else a)))