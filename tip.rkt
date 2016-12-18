#lang typed/racket

;; AST of TIP language

(provide (all-defined-out))

(struct Fun ([name : Id]
             [args : (Listof Id)]
             [locals : (Listof Id)]
             [body : (U Stmt (Listof Stmt))])
  #:transparent)

(define-type Expr (U Int Id Plus
                     Minus Mult Div
                     Greater Equal Input
                     App AddrOf Malloc DeRef
                     Null))

(define-type Id Symbol)
(define-type Int Integer)

(struct Plus ([lhs : Expr] [rhs : Expr]) #:transparent)
(struct Minus ([lhs : Expr] [rhs : Expr]) #:transparent)
(struct Mult ([lhs : Expr] [rhs : Expr]) #:transparent)
(struct Div ([lhs : Expr] [rhs : Expr]) #:transparent)
(struct Greater ([lhs : Expr] [rhs : Expr]) #:transparent)
(struct Equal ([lhs : Expr] [rhs : Expr]) #:transparent)

(struct Input ())

(struct App ([fun : Expr] [args : (Listof Expr)]) #:transparent)

(struct AddrOf ([var : Id]) #:transparent)
(struct Malloc ())
(struct DeRef ([e : Expr]) #:transparent)
(struct Null ())

(define-type Stmt (U Assign Output If NoOp While Return (Listof Stmt)))
(struct NoOp ())
(struct Assign ([id : (U Id DeRef)] [e : Expr]) #:transparent)
(struct Output ([e : Expr]) #:transparent)
(struct If ([cnd : Expr] [thn : Stmt] [els : Stmt]) #:transparent)
(struct While ([cnd : Expr] [body : Stmt]) #:transparent)
(struct Return ([e : Expr]) #:transparent)

(define-type Program (Listof Fun))
