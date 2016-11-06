#lang typed/racket

;; Control flow graph
;; Node represents statement
;; Edge represents control flow

(require "tip.rkt")

(struct Node ([body : (U Stmt Expr)]
              [pred : (Listof Node)]
              [succ : (Listof Node)]))
(struct Edge ([from : Node] [to : Node]))
