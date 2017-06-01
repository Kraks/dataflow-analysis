#lang racket

;; Live Variables Analysis
;; A variable is live at a program point if its
;; current value may be read during the remaining
;; execution of program.

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define live-variables-analysis
  (Analysis
   ; direction
   'backward
   ; init
   (λ (cfg n) (set))
   ; entry fact
   (λ (fun cfg entry) (set))
   ; exit fact
   (λ (fun cfg exit) (set))
   ; gen
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (get-vars e)]
       [(Node (Equal l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (Greater l r) _)
        (set-union (get-vars l) (get-vars r))]
       [(Node (App f args) _)
        (apply set-union (map get-vars args))]
       [(Node (Output e) _)
        (get-vars e)]
       [(Node (Return e) _)
        (get-vars e)]
       [else (set)]))
   ; kill
   (λ (cfg n)
     (match n
       [(Node (Assign id e) _) (set id)]
       [else (set)]))
   ; meet
   set-union))

(define live-variables
  (chaotic-iteration live-variables-analysis))

(module+ test
  (define test-fun
    (parse-function
     '{test {}
            {var x y z}
            {{:= y 4}
             {:= x 2}
             {if {== x y}
                 {:= z y}
                 {:= z {* y y}}}
             {:= x z}}}))

  (define result (live-variables test-fun))
  (define result-IN (car result))
  (define result-OUT (cdr result))
  (check-equal? (make-immutable-hash (hash->list result-IN))
                (hash
                 (Node (Assign 'x 2) 2)
                 (set 'y)
                 (Node (Assign 'z 'y) 3)
                 (set 'y)
                 (Node (Assign 'z (Mult 'y 'y)) 4)
                 (set 'y)
                 (Node (Equal 'x 'y) 5)
                 (set 'x 'y)
                 (Node (Assign 'x 'z) 7)
                 (set 'z)
                 (Node (Assign 'y 4) 1)
                 (set)
                 (Node (NoOp) 6)
                 (set 'z)))
  (check-equal? (make-immutable-hash (hash->list result-OUT))
                (hash
                 (Node (Assign 'x 2) 2)
                 (set 'x 'y)
                 (Node (Assign 'z 'y) 3)
                 (set 'z)
                 (Node (Assign 'z (Mult 'y 'y)) 4)
                 (set 'z)
                 (Node (Equal 'x 'y) 5)
                 (set 'y)
                 (Node (Assign 'x 'z) 7)
                 (set)
                 (Node (Assign 'y 4) 1)
                 (set 'y)
                 (Node (NoOp) 6)
                 (set 'z))))