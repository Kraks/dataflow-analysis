#lang typed/racket

;; Intraprocedure control flow graph, transformed from AST

(require "tip.rkt")

(struct Node ([body : (U Stmt Expr)]
              [pred : (Listof Node)]
              [succ : (Listof Node)]))
(struct Edge ([from : Node] [to : Node]))

;(define-type Stmt (U Assign Output Seq If NoOp While))
;(define (ast->cfg stmt)
