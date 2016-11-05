#lang racket

(require "tip.rkt")
(require rackunit)

(define (parse-stmt s)
  (match s
    [`(:= ,(? symbol? id) ,e) (Assign id (parse-expr e))]
    [`(:= (* ,l) ,e) (Assign (DeRef (parse-expr l))
                             (parse-expr e))]
    [`(output ,e) (Output (parse-expr e))]
    [`(if ,cnd ,thn ,els) (If (parse-expr cnd)
                              (parse-stmt thn)
                              (parse-stmt els))]
    [`(while ,cnd ,body) (While (parse-expr cnd)
                                (parse-stmt body))]
    [`(noop) (NoOp)]
    [(list fst rest ...)
     (Seq (map parse-stmt (cons fst rest)))]
    [else (error 'parse-stmt "can not parse statement")]))

(define (parse-expr e)
  (match e
    [(? symbol? s) s]
    [(? integer? i) i]
    [`(+ ,lhs ,rhs) (Plus (parse-expr lhs)
                          (parse-expr rhs))]
    [`(- ,lhs ,rhs) (Minus (parse-expr lhs)
                           (parse-expr rhs))]
    [`(* ,lhs ,rhs) (Mult (parse-expr lhs)
                          (parse-expr rhs))]
    [`(/ ,lhs ,rhs) (Div (parse-expr lhs)
                         (parse-expr rhs))]
    [`(> ,lhs ,rhs) (Greater (parse-expr lhs)
                             (parse-expr rhs))]
    [`(== ,lhs ,rhs) (Equal (parse-expr lhs)
                              (parse-expr rhs))]
    [`(input) (Input)]
    [`(null) (Null)]
    [`(& ,(? symbol? id)) (AddrOf id)]
    [`(* ,e) (DeRef (parse-expr e))]
    [`(,fun ,args ...) (App (parse-expr fun)
                            (map parse-expr args))]
    [else (error 'parse-expr "can not parse expression")]))

(define (parse-ret r)
  (match r
    [`(return ,e) (Return (parse-expr e))]))

(define (parse-function f)
  (match f
    [`(,fname (,vars ...) (var ,locals ...)
              ,stmt ,ret)
     (Fun fname vars locals (parse-stmt stmt) (parse-ret ret))]
    [else (error 'parse-function "can not parse function")]))

;;;;;;;;;;;;;;;;;

(check-equal? (parse-stmt '(while (== 1 2) (output 3)))
              (While (Equal 1 2) (Output 3)))
(check-equal? (parse-stmt '((:= a 3)))
              (Seq (list (Assign 'a 3))))
(check-equal? (parse-stmt '((:= a 3) (:= b 4) (:= c (+ a b))))
              (Seq (list (Assign 'a 3) (Assign 'b 4) (Assign 'c (Plus 'a 'b)))))
(check-equal? (parse-stmt '(if (== a b)
                               ((:= a 3) (:= b 4))
                               ((:= (* a) 4) (:= a 3))))
              (If (Equal 'a 'b)
                  (Seq (list (Assign 'a 3) (Assign 'b 4)))
                  (Seq (list (Assign (DeRef 'a) 4) (Assign 'a 3)))))

(define rec '(rec (n)
               (var f)
               (if (== n 0)
                   (:= f 1)
                   (:= f (* n (rec (- n 1)))))
               (return f)))
(define frec (Fun 'rec '(n) '(f)
                  (If (Equal 'n 0)
                      (Assign 'f 1)
                      (Assign 'f (Mult 'n (App 'rec (list (Minus 'n 1))))))
                  (Return 'f)))
(check-equal? (parse-function rec) frec)