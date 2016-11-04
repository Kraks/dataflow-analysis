#lang racket

(require "tip.rkt")

(define (parse-output s)
  (match s
    [`(output ,e)
     (Output (Null))]))