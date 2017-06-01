#lang racket

;; Very Busy Expression Analysis
;; An expression is very busy if, no matter what
;; path is taken, it will definitely be evaluated
;; again before its value changes.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

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

(define not-constant?
  (λ (x) (and (not (integer? x)) (not (boolean? x)))))

(define very-busy-analysis
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg node) (list->set (filter not-constant? (get-exprs cfg))))
   ; entry fact
   null
   ; exit fact
   (λ (fun cfg n) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _)
        (if (not-constant? e) (set e) (set))]
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

(define very-busy-exprs
  (chaotic-iteration very-busy-analysis))

(define test-fun
  (parse-function '{test {}
                         {var a b x y}
                         {{if {== a b}
                              {{:= x {- b a}}
                               {:= y {- a b}}}
                              {{:= y {- b a}}
                               {:= a 0}
                               {:= x {- a b}}}}
                          {return 0}}}))

(very-busy-exprs test-fun)

#|
(cons
 (hash
  (Node (Assign 'y (Minus 'a 'b)) 2)
  (set (Minus 'a 'b))
  (Node (Assign 'x (Minus 'b 'a)) 1)
  (set (Minus 'a 'b) (Minus 'b 'a))
  (Node (Assign 'a 0) 4)
  (set)
  (Node (Assign 'y (Minus 'b 'a)) 3)
  (set (Minus 'b 'a))
  (Node (Equal 'a 'b) 6)
  (set (Minus 'b 'a))
  (Node (Return 0) 8)
  (set)
  (Node (NoOp) 7)
  (set)
  (Node (Assign 'x (Minus 'a 'b)) 5)
  (set (Minus 'a 'b)))
 (hash
  (Node (Assign 'y (Minus 'a 'b)) 2)
  (set)
  (Node (Assign 'x (Minus 'b 'a)) 1)
  (set (Minus 'a 'b))
  (Node (Assign 'a 0) 4)
  (set (Minus 'a 'b))
  (Node (Assign 'y (Minus 'b 'a)) 3)
  (set)
  (Node (Equal 'a 'b) 6)
  (set (Minus 'b 'a))
  (Node (Return 0) 8)
  (set)
  (Node (NoOp) 7)
  (set)
  (Node (Assign 'x (Minus 'a 'b)) 5)
  (set)))
|#