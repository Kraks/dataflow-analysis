#lang racket

;; Parser of TIP language, converts s-exp to AST

(require rackunit)
(require "tip.rkt")

(provide (all-defined-out))

; sexp -> Stmt
(define (parse-stmt s)
  (match s
    [`{:= ,id ,e} (Assign (parse-expr id) (parse-expr e))]
    [`{output ,e} (Output (parse-expr e))]
    [`{if ,cnd ,thn ,els} (If (parse-expr cnd)
                              (parse-stmt thn)
                              (parse-stmt els))]
    [`{while ,cnd ,body} (While (parse-expr cnd)
                                (parse-stmt body))]
    [`{noop} (NoOp)]
    [`{return ,e} (Return (parse-expr e))]
    [`{,stmts ...} (Seq (map parse-stmt stmts))]
    [else (error 'parse-stmt "can not parse statement")]))

; sexp -> Expr
(define (parse-expr e)
  (match e
    [(? symbol? s) s]
    [(? integer? i) i]
    [`{+ ,lhs ,rhs} (Plus (parse-expr lhs)
                          (parse-expr rhs))]
    [`{- ,lhs ,rhs} (Minus (parse-expr lhs)
                           (parse-expr rhs))]
    [`{* ,lhs ,rhs} (Mult (parse-expr lhs)
                          (parse-expr rhs))]
    [`{/ ,lhs ,rhs} (Div (parse-expr lhs)
                         (parse-expr rhs))]
    [`{> ,lhs ,rhs} (Greater (parse-expr lhs)
                             (parse-expr rhs))]
    [`{== ,lhs ,rhs} (Equal (parse-expr lhs)
                            (parse-expr rhs))]
    [`{& ,(? symbol? id)} (AddrOf id)]
    [`{* ,e} (DeRef (parse-expr e))]
    [`{input} (Input)]
    [`{null} (Null)]
    {`{malloc} (Malloc)}
    [`{,fun ,args ...} (App (parse-expr fun)
                            (map parse-expr args))]
    [else (error 'parse-expr "can not parse expression")]))

; sexp -> Fun
(define (parse-function f)
  (match f
    [`{,fname {,vars ...} {var ,locals ...} ,stmts}
     (Fun fname vars locals (parse-stmt stmts))]
    [`{,fname {,vars ...} {} ,stmts}
     (Fun fname vars '() (parse-stmt stmts))]
    [else (error 'parse-function "can not parse function")]))

;;;;;;;;;;;;;;;;;

(module+ test
  (check-equal? (parse-stmt '{while {== 1 2} {output 3}})
                (While (Equal 1 2) (Output 3)))

  (check-equal? (parse-stmt '{{:= a 3}})
                (Seq (list (Assign 'a 3))))

  (check-equal? (parse-stmt '{{:= a 3} {:= b 4} {:= c {+ a b}}})
                (Seq (list (Assign 'a 3) (Assign 'b 4) (Assign 'c (Plus 'a 'b)))))

  (check-equal? (parse-stmt '{if {== a b}
                                 {{:= a 3} {:= b 4}}
                                 {{:= {* a} 4} {:= a 3}}})
                (If (Equal 'a 'b)
                    (Seq (list (Assign 'a 3) (Assign 'b 4)))
                    (Seq (list (Assign (DeRef 'a) 4) (Assign 'a 3)))))

  (check-equal? (parse-stmt '{while {> x 0}
                                    {:= x {- x 1}}})
                (While (Greater 'x 0) (Assign 'x (Minus 'x 1))))

  (check-equal? (parse-function '{rec {n}
                                   {var f}
                                   {{if {== n 0} {:= f 1}
                                        {:= f {* n {rec {- n 1}}}}}
                                    {return f}}})
                (Fun 'rec '(n) '(f)
                     (Seq (list
                           (If (Equal 'n 0)
                               (Assign 'f 1)
                               (Assign 'f (Mult 'n (App 'rec (list (Minus 'n 1))))))
                           (Return 'f)))))

  (check-equal? (parse-function '{add {x y} {}
                                      {return {+ x y}}})
                (Fun 'add '(x y) '() (Return (Plus 'x 'y))))

  (check-equal? (parse-function '{foo {p x}
                                      {var f q}
                                      {{if {== {* p} 0}
                                           {:= f 1}
                                           {{:= q {malloc}}
                                            {:= {* q} {- {* q} 1}}
                                            {:= f {* {* p} {{* x} q x}}}}}
                                       {return f}}})
                (Fun 'foo '(p x) '(f q)
                     (Seq (list
                           (If
                            (Equal (DeRef 'p) 0)
                            (Assign 'f 1)
                            (Seq
                             (list
                              (Assign 'q (Malloc))
                              (Assign (DeRef 'q) (Minus (DeRef 'q) 1))
                              (Assign 'f (Mult (DeRef 'p) (App (DeRef 'x) '(q x)))))))
                           (Return 'f)))))
  )