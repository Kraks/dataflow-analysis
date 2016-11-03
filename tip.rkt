#lang typed/racket

(struct Fun ([name : Id]
             [args : (Listof Id)]
             [locals : (Listof Id)]
             [body : Stmt]
             [ret : Expr]))

(define-type Expr (U Int Id Plus
                     Minus Mult Div
                     Greater Equal Input
                     App AddrOf Malloc DeRef
                     Null))

(struct App ([fun : Expr] [args : (Listof Expr)]))

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
(struct Seq ([stmts : (Listof Stmt)]))
(struct If ([cnd : Expr] [thn : Stmt] [els : Stmt]))
(struct NoOp ())
(struct While ([cnd : Expr] [body : Stmt]))

(define-type Program (Listof Fun))

;;;;;;;;;;;;;;;;;;;;;;;;

; Example programs

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
                  'f))


