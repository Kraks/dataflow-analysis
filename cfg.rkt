#lang racket

;; Intraprocedure control flow graph, transformed from AST
;; Control flow graph
;; Node represents statement
;; Edge represents a directed control flow

(require "tip.rkt")
(require "parser.rkt")
(require rackunit)

(struct Node (body) #:transparent)
(struct Edge (from to) #:transparent)
(struct CFG (entry exit nodes edges) #:transparent)

(define (stmt->cfg stmt nodes edges)
  (match stmt
    [(If cnd thn els)
     (define thn-cfg (stmt->cfg thn '() '()))
     (define els-cfg (stmt->cfg els '() '()))
     (define entry (Node cnd))
     (define exit (Node (NoOp)))
     (CFG entry exit
          (append (list entry exit)
                  (CFG-nodes thn-cfg)
                  (CFG-nodes els-cfg))
          (append (list (Edge entry (CFG-entry thn-cfg))
                        (Edge entry (CFG-entry els-cfg))
                        (Edge (CFG-exit thn-cfg) exit)
                        (Edge (CFG-exit els-cfg) exit))
                  (CFG-edges thn-cfg)
                  (CFG-edges els-cfg)))]
    [(While cnd body)
     (define body-cfg (stmt->cfg body '() '()))
     (define entry (Node cnd))
     (define es (list (Edge entry (CFG-entry body-cfg))
                      (Edge (CFG-exit body-cfg) entry)))
     (CFG entry (CFG-exit body-cfg)
          (cons entry (CFG-nodes body-cfg))
          (append es (CFG-edges body-cfg)))]
    [(Seq stmts)
     (define cfgs (map (λ (s) (stmt->cfg s '() '())) stmts))
     (define entry (CFG-entry (first cfgs)))
     (define exit (CFG-exit (last cfgs)))
     (define conns (for/list ([exit cfgs]
                              [entry (cdr cfgs)])
                     (Edge (CFG-exit exit) (CFG-entry entry))))
     (define all-nodes (foldl (λ (c acc) (append (CFG-nodes c) acc)) '() cfgs))
     (define all-edges (foldl (λ (c acc) (append (CFG-edges c) acc)) '() cfgs))
     (CFG entry exit all-nodes (append conns all-edges))]
    [else (CFG (Node stmt) (Node stmt) (list (Node stmt)) '())]))

(module+ test
  (check-equal? (stmt->cfg (parse-stmt '{:= a 3}) '() '())
                (CFG (Node (Assign 'a 3)) (Node (Assign 'a 3)) (list (Node (Assign 'a 3))) '()))
  
  (check-equal? (stmt->cfg (parse-stmt '{{:= a 3} {:= b 4}}) '() '())
                (CFG
                 (Node (Assign 'a 3))
                 (Node (Assign 'b 4))
                 (list (Node (Assign 'b 4)) (Node (Assign 'a 3)))
                 (list (Edge (Node (Assign 'a 3)) (Node (Assign 'b 4))))))
  
  (check-equal? (stmt->cfg (parse-stmt '{{:= a 1}
                                         {:= b 2}
                                         {:= c 3}}) '() '())
                (CFG
                 (Node (Assign 'a 1))
                 (Node (Assign 'c 3))
                 (list (Node (Assign 'c 3)) (Node (Assign 'b 2)) (Node (Assign 'a 1)))
                 (list (Edge (Node (Assign 'a 1)) (Node (Assign 'b 2)))
                       (Edge (Node (Assign 'b 2)) (Node (Assign 'c 3))))))

  (check-equal? (stmt->cfg (parse-stmt '{{:= a 1}
                                         {if {== a 1} {:= b 2} {:= b 3}}
                                         {:= c 4}}) '() '())
                (CFG
                 (Node (Assign 'a 1))
                 (Node (Assign 'c 4))
                 (list
                  (Node (Assign 'c 4))
                  (Node (Equal 'a 1))
                  (Node (NoOp))
                  (Node (Assign 'b 2))
                  (Node (Assign 'b 3))
                  (Node (Assign 'a 1)))
                 (list
                  (Edge (Node (Assign 'a 1)) (Node (Equal 'a 1)))
                  (Edge (Node (NoOp)) (Node (Assign 'c 4)))
                  (Edge (Node (Equal 'a 1)) (Node (Assign 'b 2)))
                  (Edge (Node (Equal 'a 1)) (Node (Assign 'b 3)))
                  (Edge (Node (Assign 'b 2)) (Node (NoOp)))
                  (Edge (Node (Assign 'b 3)) (Node (NoOp))))))
  
  (check-equal? (stmt->cfg (parse-stmt '{{if {== x 1} {:= a 1} {:= b 2}}}) '() '())
                (CFG
                 (Node (Equal 'x 1))
                 (Node (NoOp))
                 (list (Node (Equal 'x 1)) (Node (NoOp)) (Node (Assign 'a 1)) (Node (Assign 'b 2)))
                 (list
                  (Edge (Node (Equal 'x 1)) (Node (Assign 'a 1)))
                  (Edge (Node (Equal 'x 1)) (Node (Assign 'b 2)))
                  (Edge (Node (Assign 'a 1)) (Node (NoOp)))
                  (Edge (Node (Assign 'b 2)) (Node (NoOp))))))

  (check-equal? (stmt->cfg (parse-stmt '{while {> 5 x} {:= x {+ x 1}}}) '() '())
                (CFG
                 (Node (Greater 5 'x))
                 (Node (Assign 'x (Plus 'x 1)))
                 (list (Node (Greater 5 'x)) (Node (Assign 'x (Plus 'x 1))))
                 (list
                  (Edge (Node (Greater 5 'x)) (Node (Assign 'x (Plus 'x 1))))
                  (Edge (Node (Assign 'x (Plus 'x 1))) (Node (Greater 5 'x))))))

  (check-equal? (stmt->cfg (parse-stmt '{while {> 5 x} {{if {== x 3} {:= x 4} {:= x 5}}
                                                        {:= x {- x 1}}}}) '() '())
                (CFG
                 (Node (Greater 5 'x))
                 (Node (Assign 'x (Minus 'x 1)))
                 (list
                  (Node (Greater 5 'x))
                  (Node (Assign 'x (Minus 'x 1)))
                  (Node (Equal 'x 3))
                  (Node (NoOp))
                  (Node (Assign 'x 4))
                  (Node (Assign 'x 5)))
                 (list
                  (Edge (Node (Greater 5 'x)) (Node (Equal 'x 3)))
                  (Edge (Node (Assign 'x (Minus 'x 1))) (Node (Greater 5 'x)))
                  (Edge (Node (NoOp)) (Node (Assign 'x (Minus 'x 1))))
                  (Edge (Node (Equal 'x 3)) (Node (Assign 'x 4)))
                  (Edge (Node (Equal 'x 3)) (Node (Assign 'x 5)))
                  (Edge (Node (Assign 'x 4)) (Node (NoOp)))
                  (Edge (Node (Assign 'x 5)) (Node (NoOp))))))
  )