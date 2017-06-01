## Dataflow Analysis

An implementation of traditional dataflow analyses for an imperative language TIP.

#### The TIP Language

The TIP language is largely inspired from the lecture notes _Static Program Analysis_[1], but mixed with S-Expression syntax.

**An Example**

```
{while {> 5 x}
  {{if {== x 3}
       {:= x 4}
       {:= x 5}}
   {:= x {- x 1}}}}
```

#### File Description

* `parser.rkt` function that parses s-exp based TIP to abstract syntax tree (AST).
* `ast.rkt` the abstract syntax tree structure definitions.
* `cfg.rkt` contrlo flow graph (CFG) structure definitions; CFG is transformed from AST.
* `dfa.rkt` chaotic iteration framework algorithm.
* `reaching-def.rkt` reaching definition analysis.
* `very-busy.rkt` very busy expressions analysis.

#### TODO

* SSA-based analysis
* Pointer analysis

#### References

[1] [Static Program Analysis](https://cs.au.dk/~amoeller/spa/)
