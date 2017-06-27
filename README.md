## Dataflow Analysis

A Racket implementation of traditional dataflow analyses for an imperative language TIP.

### Target: the TIP Language

These analyses are targeted for S-Expression based TIP language. The syntax is largely extracted from the great lecture notes _Static Program Analysis_[1].

**An Example**

```
{while {> 5 x}
  {{if {== x 3}
       {:= x 4}
       {:= x 5}}
   {:= x {- x 1}}}}
```

### File Descriptions

* `parser.rkt` functions that parse s-exp based TIP to abstract syntax tree (AST).
* `ast.rkt` the abstract syntax tree structure definitions.
* `cfg.rkt` contrlo flow graph (CFG) structure definitions; CFG is transformed from AST.
* `dfa.rkt` chaotic iteration framework and algorithm, which operates on CFG.
* `reaching-def.rkt` reaching definition analysis.
* `very-busy.rkt` very busy expressions analysis.
* `available-expr.rkt` available expressions analysis.
* `live-var.rkt` live variables analysis.

See test cases in each files.

### TODO

* SSA-based analysis
* Pointer analysis

### References

[1] [Static Program Analysis](https://cs.au.dk/~amoeller/spa/)
