#lang racket

;; Available Expressions Analysis
;; An expression is available at a program point if
;; its current value has already been computed earlier
;; in the execution, and not changed on all paths to
;; this program point; so there is no need to re-evaluate
;; it.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define avail-expr-analysis
  (Analysis
   ; direction
   'forward
   ; init
   (λ (cfg node) (list->set (filter not-constant? (get-exprs cfg))))
   ; entry fact
   (λ (fun cfg n) (set))
   ; exit fact
   null
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (if (not (expr-contains-var? e id))
            (set e) (set))]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (list->set (filter (λ (e1) (expr-contains-var? e1 id))
                           (set->list (get-exprs cfg))))]
       [else (set)]))
   ; meet
   set-intersect
   ))

(define availble-expr (chaotic-iteration avail-expr-analysis))

(module+ test
  (define test-fun
    (parse-function '{test {}
                           {var a b x y}
                           {{:= x {- a b}}
                            {:= y {* a b}}
                            {while {> y {- a b}}
                                   {{:= a {- a 1}}
                                    {:= x {- a b}}}}
                            {return 0}}}))

  (define result (availble-expr test-fun))
  (define result-IN (car result))
  (define result-OUT (cdr result))

  (check-equal? (make-immutable-hash (hash->list result-IN))
                (hash
                 (Node (Return 0) 6)
                 (set (Minus 'a 'b))
                 (Node (Assign 'a (Minus 'a 1)) 3)
                 (set (Minus 'a 'b))
                 (Node (Assign 'x (Minus 'a 'b)) 4)
                 (set)
                 (Node (Greater 'y (Minus 'a 'b)) 5)
                 (set (Minus 'a 'b))
                 (Node (Assign 'y (Mult 'a 'b)) 2)
                 (set (Minus 'a 'b))
                 (Node (Assign 'x (Minus 'a 'b)) 1)
                 (set)))

  (check-equal? (make-immutable-hash (hash->list result-OUT))
                (hash
                 (Node (Return 0) 6)
                 (set (Minus 'a 'b))
                 (Node (Assign 'a (Minus 'a 1)) 3)
                 (set)
                 (Node (Assign 'x (Minus 'a 'b)) 4)
                 (set (Minus 'a 'b))
                 (Node (Greater 'y (Minus 'a 'b)) 5)
                 (set (Minus 'a 'b))
                 (Node (Assign 'y (Mult 'a 'b)) 2)
                 (set (Minus 'a 'b) (Mult 'a 'b))
                 (Node (Assign 'x (Minus 'a 'b)) 1)
                 (set (Minus 'a 'b)))))