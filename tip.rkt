#lang typed/racket

(struct Fun ([name : Id]
             [args : (List Id)]
             [locals : (List Id)]
             [body : Stmt]
             [ret : Expr]))

(define-type Expr (U Int Id Plus
                     Minus Mult Div
                     Greater Equal Input
                     App AddrOf Malloc DeRef
                     Null))

(struct App ([fun : Id] [args : (List Expr)]))
(define-type Id Symbol)
(define-type Int Integer)
(struct Plus ([lhs : Expr] [rhs : Expr]))
(struct Minus ([lhs : Expr] [rhs : Expr]))
(struct Mult ([lhs : Expr] [rhs : Expr]))
(struct Div ([lhs : Expr] [rhs : Expr]))
(struct Greater ([lhs : Expr] [rhs : Expr]))
(struct Equal ([lhs : Expr] [rhs : Expr]))
(struct Input ())

(struct AddrOf ([var : Id]))
(struct Malloc ())
(struct DeRef ([e : Expr]))
(struct Null ())

(define-type Stmt (U Assign Output Seq
                     If NoOp While))
(struct Assign ([id : (U Id DeRef)] [e : Expr]))
(struct Output ([e : Expr]))
(struct Seq ([stmts : (List Stmt)]))
(struct If ([cnd : Expr] [thn : Stmt] [els : Stmt]))
(struct NoOp ())
(struct While ([cnd : Expr] [body : Stmt]))

(define-type Program (List Fun))
