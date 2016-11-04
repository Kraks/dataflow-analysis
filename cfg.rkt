#lang typed/racket

(require "tip.rkt")

(struct Node ([body : (U Stmt Expr)]
              [pred : (Listof Node)]
              [succ : (Listof Node)]))
(struct Edge ([from : Node] [to : Node]))