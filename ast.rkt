#lang racket

;; AST of TIP language

(provide (all-defined-out))

(struct Fun (name args locals body) #:transparent)

#|
Var ::= Symbol
Expr ::= Int | Bool | Var | Plus | Minus | Mult | Div |
         Greater | Equal | Input | App |
         AddrOf | Malloc | DeRef | Null
|#

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

;; Auxiliary functions

(define not-constant?
  (λ (x) (and (not (integer? x)) (not (boolean? x)))))

(define (expr-contains-var? expr var)
  (match expr
    [(? integer?) #f]
    [(? boolean? x) #f]
    [(? symbol? x) (eq? x var)]
    [(Plus l r) (or (expr-contains-var? l var)
                    (expr-contains-var? r var))]
    [(Minus l r) (or (expr-contains-var? l var)
                     (expr-contains-var? r var))]
    [(Mult l r) (or (expr-contains-var? l var)
                    (expr-contains-var? r var))]
    [(Div l r) (or (expr-contains-var? l var)
                   (expr-contains-var? r var))]
    [(Greater l r) (or (expr-contains-var? l var)
                       (expr-contains-var? r var))]
    [(Equal l r) (or (expr-contains-var? l var)
                     (expr-contains-var? r var))]
    ;; TODO: handle function pointer
    [(App f args) (ormap (λ (e) (expr-contains-var? e var)) args)]
    [(AddrOf x) (eq? x var)]
    [(DeRef e) (expr-contains-var? e var)]
    [else #f]))
