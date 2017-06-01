#lang racket

;; The framework of dataflow analysis
;; Chaotic Iteration Algorithm

(require "ast.rkt")
(require "parser.rkt")
(require "cfg.rkt")

(provide (all-defined-out))

(struct Analysis (direction
                  init
                  entry-fact
                  exit-fact
                  gen
                  kill
                  meet))

(define (chaotic-iteration analysis)
  (define init (Analysis-init analysis))
  (define direction (Analysis-direction analysis))
  (define entry-fact (Analysis-entry-fact analysis))
  (define exit-fact (Analysis-exit-fact analysis))
  (define gen (Analysis-gen analysis))
  (define kill (Analysis-kill analysis))
  (define meet (Analysis-meet analysis))

  (lambda (fun)
    (define cfg (fun->cfg fun))
    (define IN (make-hash))
    (define OUT (make-hash))
    
    (for ([n (CFG-nodes cfg)])
        (hash-set! IN n (init cfg n))
        (hash-set! OUT n (init cfg n)))

    (when (not (empty? entry-fact))
      (hash-set! OUT (CFG-entry cfg) (entry-fact fun cfg (CFG-entry cfg))))
    (when (not (empty? exit-fact))
      (hash-set! IN (CFG-exit cfg) (exit-fact fun cfg (CFG-exit cfg))))

    (hash-set! OUT (CFG-exit cfg) (set))
    (hash-set! IN (CFG-entry cfg) (set))
    
    (define (loop IN OUT old-IN old-OUT)
      (for ([n (CFG-nodes cfg)])
        (cond [(eq? 'forward direction)
               (let ([preds (map (curry hash-ref OUT) (get-preds n cfg))])
                 (when (not (empty? preds)) (hash-set! IN n (apply meet preds))))
               (hash-set! OUT n (set-union (set-subtract (hash-ref IN n) (kill cfg n)) (gen cfg n)))]
              [(eq? 'backward direction)
               (let ([succs (map (curry hash-ref IN) (get-succs n cfg))])
                 (when (not (empty? succs)) (hash-set! OUT n (apply meet succs))))
               (hash-set! IN n (set-union (set-subtract (hash-ref OUT n) (kill cfg n)) (gen cfg n)))]
              [else (error "not a direction")]))
      
      (if (and (equal? IN old-IN)
               (equal? OUT old-OUT))
          (cons IN OUT)
          (loop IN OUT (hash-copy IN) (hash-copy OUT))))
    (loop IN OUT (hash-copy IN) (hash-copy OUT))))
