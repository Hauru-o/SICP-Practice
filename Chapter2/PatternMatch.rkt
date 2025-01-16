#lang racket

(define user-initial-environment (make-base-namespace))

(define (atom? x)
  (not (pair? x)))

;; Expressions

(define (compound? exp) (pair?   exp))
(define (constant? exp) (number? exp))
(define (variable? exp) (atom?   exp))

;; Rules

(define (pattern  rule) (car  rule))
(define (skeleton rule) (cadr rule))

;; Patterns

(define (arbitrary-constant?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?c) false))

(define (arbitrary-expression?  pattern)
  (if (pair? pattern) (eq? (car pattern) '? ) false))

(define (arbitrary-variable?    pattern)
  (if (pair? pattern) (eq? (car pattern) '?v) false))

(define (variable-name pattern) (cadr pattern))

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        ((arbitrary-constant? pat)
         (if (constant? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-variable? pat)
         (if (variable? exp)
             (extend-dictionary pat exp dict)
             'failed))
        ((arbitrary-expression? pat)
         (extend-dictionary pat exp dict))
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
           (cdr exp)
           (match (car pat)
             (car exp)
             dict)))))

(define (skeleton-evaluation?    skeleton)
  (if (pair? skeleton) (eq? (car skeleton) ':) false))

(define (eval-exp evaluation) (cadr evaluation))

(define (instantiate skeleton dict)
  (define (loop s)
    (cond ((atom? s) s)
          ((skeleton-evaluation? s)
           (evaluate (eval-exp s) dict))
          (else (cons (loop (car s))
                      (loop (cdr s))))))
  (loop skeleton))

(define (evaluate form dict)
  (if (atom? form)
      (lookup form dict)
      (apply
       (eval (lookup (car form) dict)
             user-initial-environment)
       (map (lambda (v)
              (lookup v dict))
            (cdr form)))))

(define (simplifier the-rules)
  (define (simplify-exp exp)
    (try-rules (if (compound? exp)
                   (simplify-parts exp)
                   exp)))
  (define (simplify-parts exp)
    (if (null? exp)
        '()
        (cons (simplify-exp (car exp))
              (simplify-parts (cdr exp)))))
  (define (try-rules exp)
    (define (scan rules)
      (if (null? rules)
          exp
          (let ((dict
                 (match (pattern (car rules))
                   exp
                   (empty-dictionary))))
            (if (eq? dict 'failed)
                (scan (cdr rules))
                (simplify-exp
                 (instantiate
                     (skeleton (car rules))
                   dict))))))
    (scan the-rules))
  simplify-exp)

(define (empty-dictionary) '())

(define (extend-dictionary pat dat dict)
  (let ((name (variable-name pat)))
    (let ((v (assq name dict)))
      (cond ((or (null? v) (not v))
             (cons (list name dat) dict))
            ((eq? (cadr v) dat) dict)
            (else 'failed)))))

(define (lookup var dict)
  (let ((v (assq var dict)))
    (if (or (null? v) (not v))
        var
        (cadr v))))


(define algebra-rules
  '(
    ( ((? op) (?c c1) (?c c2))                (: (op c1 c2))                )
    ( ((? op) (?  e ) (?c c ))                ((: op) (: c) (: e))          )
    ( (+ 0 (? e))                             (: e)                         )
    ( (* 1 (? e))                             (: e)                         )
    ( (* 0 (? e))                             0                             )
    ( (* (?c c1) (* (?c c2) (? e )))          (* (: (* c1 c2)) (: e))       )
    ( (* (?  e1) (* (?c c ) (? e2)))          (* (: c ) (* (: e1) (: e2)))  )
    ( (* (* (? e1) (? e2)) (? e3))            (* (: e1) (* (: e2) (: e3)))  )
    ( (+ (?c c1) (+ (?c c2) (? e )))          (+ (: (+ c1 c2)) (: e))       )
    ( (+ (?  e1) (+ (?c c ) (? e2)))          (+ (: c ) (+ (: e1) (: e2)))  )
    ( (+ (+ (? e1) (? e2)) (? e3))            (+ (: e1) (+ (: e2) (: e3)))  )
    ( (+ (* (?c c1) (? e)) (* (?c c2) (? e))) (* (: (+ c1 c2)) (: e))       )
    ( (* (? e1) (+ (? e2) (? e3)))            (+ (* (: e1) (: e2))
                                                 (* (: e1) (: e3)))         )
    ))

(define algsimp (simplifier algebra-rules))

;; Symbolic Differentiation

(define deriv-rules
  '(
    ( (dd (?c c) (? v))              0                                 )
    ( (dd (?v v) (? v))              1                                 )
    ( (dd (?v u) (? v))              0                                 )
    ( (dd (+ (? x1) (? x2)) (? v))   (+ (dd (: x1) (: v))
                                        (dd (: x2) (: v)))             )
    ( (dd (* (? x1) (? x2)) (? v))   (+ (* (: x1) (dd (: x2) (: v)))
                                        (* (dd (: x1) (: v)) (: x2)))  )
    ( (dd (** (? x) (?c n)) (? v))   (* (* (: n) (+ (: x) (: (- n 1))))
                                        (dd (: x) (: v)))              )
    ))

(define dsimp (simplifier deriv-rules))

;; 测试代数简化
(define (test-algebra-simplification)
  (displayln "Testing Algebra Simplification:")
  (displayln (list 'Input '(+ 0 x) 'Output (algsimp '(+ 0 x))))
  (displayln (list 'Input '(* 1 x) 'Output (algsimp '(* 1 x))))
  (displayln (list 'Input '(* 0 x) 'Output (algsimp '(* 0 x))))
  (displayln (list 'Input '(* 2 (* 3 x)) 'Output (algsimp '(* 2 (* 3 x)))))
  (displayln (list 'Input '(+ 2 (+ 3 x)) 'Output (algsimp '(+ 2 (+ 3 x)))))
  (displayln (list 'Input '(+ (* 2 x) (* 3 x)) 'Output (algsimp '(+ (* 2 x) (* 3 x)))))
  (displayln (list 'Input '(* x (+ y z)) 'Output (algsimp '(* x (+ y z)))))
  (newline))

;; 测试符号求导
(define (test-symbolic-differentiation)
  (displayln "Testing Symbolic Differentiation:")
  (displayln (list 'Input '(dd 3 x) 'Output (dsimp '(dd 3 x))))
  (displayln (list 'Input '(dd x x) 'Output (dsimp '(dd x x))))
  (displayln (list 'Input '(dd y x) 'Output (dsimp '(dd y x))))
  (displayln (list 'Input '(dd (+ x y) x) 'Output (dsimp '(dd (+ x y) x))))
  (displayln (list 'Input '(dd (* x y) x) 'Output (dsimp '(dd (* x y) x))))
  (displayln (list 'Input '(dd (* x y) y) 'Output (dsimp '(dd (* x y) y))))
  (displayln (list 'Input '(dd (** x 2) x) 'Output (dsimp '(dd (** x 2) x))))
  (newline))

;; 运行测试
(test-algebra-simplification)
(test-symbolic-differentiation)