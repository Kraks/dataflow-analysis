#lang racket

;; AST of TIP language

;; TODO: handle literal bool and int value
;; TODO: parse Id

(provide (all-defined-out))

(struct Fun (name args locals body) #:transparent)

#|
Var ::= Symbol
Expr ::= Int | Bool | Var | Plus | Minus | Mult | Div |
         Greater | Equal | Input | App |
         AddrOf | Malloc | DeRef | Null
|#

;(struct LitInt (i) #:transparent)
;(struct Id (var) #:transparent)

(struct Plus (lhs rhs) #:transparent)
(struct Minus (lhs rhs) #:transparent)
(struct Mult (lhs rhs) #:transparent)
(struct Div (lhs rhs) #:transparent)
(struct Greater (lhs rhs) #:transparent)
(struct Equal (lhs rhs) #:transparent)

(struct Input () #:transparent)
(struct App (fun args) #:transparent)
(struct AddrOf (var) #:transparent)
(struct Malloc () #:transparent)
(struct DeRef (e) #:transparent)
(struct Null () #:transparent)

#|
Stmt ::= NoOp | Output | Return | While | Assign | If | Stmt*
|#

(struct NoOp () #:transparent)
(struct Output (expr) #:transparent)
(struct Return (expr) #:transparent)
(struct While (cnd body) #:transparent)
(struct Assign (id e) #:transparent)
(struct If (cnd thn els) #:transparent)
(struct Seq (stmts) #:transparent)

#|
Program ::= Fun*
|#

(struct Program (funs))