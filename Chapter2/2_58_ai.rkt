#lang racket
;; 判断是否为数字，辅助函数
(define (=number? x n)
  (and (number? x) (= x n)))

;; 判断变量
(define (variable? x) (symbol? x))

;; 判断两个变量是否相同
(define (same-variable? x y)
  (and (variable? x) (variable? y) (eq? x y)))

;; 定义一个操作符优先级表
(define *precedence-table*
  '((+ . 0)
    (* . 1)))

;; 获取操作符的优先级
(define (precedence op)
  (cond ((assoc op *precedence-table*) => cdr)
        (else (error "Unknown operator" op))))

;; 判断是否是操作符
(define (operator? x)
  (assoc x *precedence-table*))

;; 找到表达式的最低优先级操作符
(define (smallest-op expr)
  (accumulate (lambda (a b)
                (if (operator? b)
                    (if (< (precedence b) (precedence a))
                        b
                        a)
                    a))
              '+  ;; 默认初始值，优先级最高
              expr))

;; 累积操作，用于递归解析表达式
(define (accumulate combiner null-value expr)
  (if (null? expr)
      null-value
      (combiner (car expr) (accumulate combiner null-value (cdr expr)))))

;; 检查是否为加法表达式
(define (sum? expr)
  (eq? (smallest-op expr) '+))

(define (prefix sym lst)
  (cond ((null? lst) '())               ; 如果列表为空，返回空列表
        ((eq? sym (car lst)) '())       ; 如果找到目标符号，返回空列表
        (else (cons (car lst)           ; 否则递归提取当前项
                    (prefix sym (cdr lst))))))


;; 提取加法的加数（左部分）
(define (addend expr)
  (let ((a (prefix '+ expr)))
    (if (= (length a) 1)
        (car a)
        a)))

;; 提取加法的被加数（右部分）
(define (augend expr)
  (let ((a (cdr (memq '+ expr))))
    (if (= (length a) 1)
        (car a)
        a)))

;; 检查是否为乘法表达式
(define (product? expr)
  (eq? (smallest-op expr) '*))

;; 提取乘法的第一个因子
(define (multiplier expr)
  (let ((m (prefix '* expr)))
    (if (= (length m) 1)
        (car m)
        m)))

;; 提取乘法的第二个因子
(define (multiplicand expr)
  (let ((m (cdr (memq '* expr))))
    (if (= (length m) 1)
        (car m)
        m)))

;; 构造加法表达式
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

;; 构造乘法表达式
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;; 主求导函数
(define (deriv expr var)
  (cond ((number? expr) 0)  ;; 数字求导为 0
        ((variable? expr)  ;; 如果是变量
         (if (same-variable? expr var) 1 0))
        ((sum? expr)  ;; 加法求导
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)  ;; 乘法求导
         (make-sum
          (make-product (multiplier expr)
                        (deriv (multiplicand expr) var))
          (make-product (deriv (multiplier expr) var)
                        (multiplicand expr))))
        (else
         (error "Unknown expression type -- DERIV" expr))))

(deriv '(x + y) 'x)