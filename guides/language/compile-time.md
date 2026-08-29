# Compile-Time Evaluation

Catena 0.1.34 decides what executes during compilation: checking and
generation, never silent execution.

## The decision

| Form | Decision |
| --- | --- |
| Constants | never execute — definitions compile, not run |
| Attributes | no attribute system exists |
| Macros | no macro system exists |
| Generated derivations | compiler-internal generation, no user code |

Any arrival — a const-eval fragment, a macro expander, an attribute
evaluator, a derive that runs user code — enters through its own
slice under C034's gate: total-or-bounded in the admitting change,
never unbounded, never a compatible addition.

## Derivations are generation, not execution

`Catena.Derive` emits folds and capability helpers by structural
recursion over declared datatypes: it evaluates no user expression,
marks every derived definition with `compiler_derived` provenance,
recompiles byte-identically, and its output flows through the same
checking, verification, and erasure as handwritten code.

## The restriction table

| Evaluator | Regime | Home |
| --- | --- | --- |
| The gate | total-or-bounded in the admitting slice | C034 |
| Condition normalization | acyclic, budgeted | C003 |
| Specification examples | 20,000 semantic steps | C006 |
| Law checking | bounded samples | C004 |

Compilation cannot hang on user-authored code: every machine that
runs is acyclic or budgeted, and every future one arrives gated.

## Current boundary

Spellings for any future const/macro/attribute surface remain
P109's; deriving extensions remain G040's, classified under these
rules on arrival; code-generation programs remain G005/G116's.

The normative contract is the research repository's
[Compile-Time Evaluation Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/compile-time-evaluation).
