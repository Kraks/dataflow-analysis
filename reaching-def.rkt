#lang racket

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")
(require "dfa.rkt")

(require rackunit)

(define reaching-definitions-analysis
  (Analysis
   'forward
   (λ (cfg node) (set))
   (λ (fun cfg entry) (set))
;             (for/set ([v (Fun-locals fun)])
;               (cons v 'undefined)))
   (λ (fun cfg entry) (set))
   (λ (cfg n)
     (match n
       [(Node (Assign id val) label) (set (cons id label))]
       [else (set)]))
   (λ (cfg n)
     (match n
       [(Node (Assign id val) label)
        (list->set
         (map (λ (x) (cons id (Node-label x)))
              (filter (λ (x) (and (Assign? (Node-body x))
                                  (eq? (Assign-id (Node-body x)) id)
                                  (not (eq? (Node-label x) label))))
                      (CFG-nodes cfg))))]
       [else (set)]))
   set-union))

(define reaching-definitions
  (chaotic-iteration reaching-definitions-analysis))

;; Tests

(module+ test
  (define test-fun
    (parse-function '{test {}
                           {var x y}
                           {{:= x y}
                            {:= y 1}
                            {while {== x 1}
                                   {{:= y {* x y}}
                                    {:= x {- x 1}}}}
                            {return y}}}))

  (define rf-test-fun (reaching-definitions test-fun))
  (define IN-test-fun (car rf-test-fun))
  (define OUT-test-fun (cdr rf-test-fun))
  (check-equal? (make-immutable-hash (hash->list IN-test-fun))
                (hash
                 (Node (Equal 'x 1) 5)
                 (set '(y . 3) '(x . 4) '(y . 2) '(x . 1))
                 (Node (Assign 'y (Mult 'x 'y)) 3)
                 (set '(y . 3) '(x . 4) '(y . 2) '(x . 1))
                 (Node (Assign 'y 1) 2)
                 (set '(x . 1))
                 (Node (Assign 'x 'y) 1)
                 (set)
                 (Node (Assign 'x (Minus 'x 1)) 4)
                 (set '(y . 3) '(x . 4) '(x . 1))
                 (Node (Return 'y) 6)
                 (set '(y . 3) '(x . 4) '(y . 2) '(x . 1))))
  (check-equal? (make-immutable-hash (hash->list OUT-test-fun))
                (hash
                 (Node (Equal 'x 1) 5)
                 (set '(y . 3) '(x . 4) '(y . 2) '(x . 1))
                 (Node (Assign 'y (Mult 'x 'y)) 3)
                 (set '(y . 3) '(x . 4) '(x . 1))
                 (Node (Assign 'y 1) 2)
                 (set '(y . 2) '(x . 1))
                 (Node (Assign 'x 'y) 1)
                 (set '(x . 1))
                 (Node (Assign 'x (Minus 'x 1)) 4)
                 (set '(y . 3) '(x . 4))
                 (Node (Return 'y) 6)
                 (set '(y . 3) '(x . 4) '(y . 2) '(x . 1)))))