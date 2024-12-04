#lang sicp

(define (match pat exp dict)
  (cond ((eq? dict 'failed) 'failed)
        ((atom? pat)
         (if (atom? exp)
             (if (eq? pat exp)
                 dict
                 'failed)
             'failed))
        *** Pattern variable clauses
        ((atom? exp) 'failed)
        (else
         (match (cdr pat)
           (cdr exp)
           (match (car pat)
             (car exp)
             dict)))))