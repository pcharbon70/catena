# Phase 3: First-Class `Resumption` Kinds, Types, And Effects

**Description:** This phase makes resumptions real static-language values. It
introduces the `Resumption k a b e` type constructor, derives its parameters
from handled operations and delimiters, types `resume`, and permits
resumptions to flow through ordinary Catena values without overstating
one-shot guarantees that Hindley-Milner inference cannot prove.

**Status:** In progress (Sections 3.1-3.3 complete).

**Dependencies:** Phase 2 complete.

## Section 3.1: Kinds And Internal Type Representation

**Description:** Add the kind and type representation needed to distinguish
resumption mode, operation result, delimiter result, and residual effects.

- [x] **Section 3.1 Complete**

### Task 3.1.1: Define Resumption Kinds And Constructors

**Description:** Add canonical internal identities for `ResumptionKind`,
`OneShot`, `MultiShot`, and `Resumption k a b e`.

- [x] **Task 3.1.1 Complete**

#### Subtask 3.1.1.1: Extend Kind Construction

**Description:** Define `ResumptionKind`, validate `OneShot` and `MultiShot`
at that kind, and assign `Resumption` the accepted constructor kind ending in
`Type`.

- [x] **Subtask 3.1.1.1 Complete**

#### Subtask 3.1.1.2: Extend Type Representation

**Description:** Add constructors, predicates, accessors, structural equality,
and validation for all four resumption parameters without encoding the value
as an untyped function.

- [x] **Subtask 3.1.1.2 Complete**

### Task 3.1.2: Integrate Resumptions With Type Infrastructure

**Description:** Make resumption types participate correctly in schemes,
substitution, free-variable analysis, unification, environments, and
diagnostic rendering.

- [x] **Task 3.1.2 Complete**

#### Subtask 3.1.2.1: Extend Substitution And Unification

**Description:** Traverse kind, operation-result, delimiter-result, and
effect-row parameters with occurs checks and kind-safe unification.

- [x] **Subtask 3.1.2.1 Complete**

#### Subtask 3.1.2.2: Extend Schemes And Pretty Printing

**Description:** Generalize and instantiate permitted resumption variables,
render readable source-oriented types, and preserve row-variable identity in
errors and typed-module output.

- [x] **Subtask 3.1.2.2 Complete**

**Implementation evidence:** `catena_kind` now carries the distinct
`ResumptionKind` and `EffectRow` kinds and assigns the accepted kind to
`OneShot`, `MultiShot`, and the four-parameter `Resumption` constructor.
`catena_types` represents the capability as
`{tresumption, Kind, OperationResult, DelimiterResult, EffectRow}` with
kinded mode variables, canonical residual rows, validation, predicates,
accessors, and structural equality. Substitution, occurs checks, row-aware
unification, generalization, instantiation, and source-oriented rendering
preserve every parameter and open-row identity. The focused Section 3.1
EUnit module passes 8 tests.

## Section 3.2: Handler Binder And Resume Inference

**Description:** Derive resumption types from operation signatures and the
enclosing handled computation, then enforce the accepted `resume` typing rule.

- [x] **Section 3.2 Complete**

### Task 3.2.1: Infer Operation-Case Resumption Binders

**Description:** Bind each explicit or synthetic resumption using the
operation result, delimiter result, selected kind, and residual effect row.

- [x] **Task 3.2.1 Complete**

#### Subtask 3.2.1.1: Derive Operation And Delimiter Types

**Description:** Resolve the declared operation signature, unify case
patterns, infer the handled computation result, and produce the `a` and `b`
parameters of `Resumption OneShot a b e`.

- [x] **Subtask 3.2.1.1 Complete**

#### Subtask 3.2.1.2: Derive Residual Effect Rows

**Description:** Remove the handled effect only where deep-handler semantics
justify it, preserve open row variables, and include effects exercised by the
resumed computation.

- [x] **Subtask 3.2.1.2 Complete**

### Task 3.2.2: Type And Effect-Check Resume Expressions

**Description:** Enforce that `resume(k, value)` receives typed resumption
authority and returns the matching delimiter result with its residual effects.

- [x] **Task 3.2.2 Complete**

#### Subtask 3.2.2.1: Infer Resume Operands And Result

**Description:** Require the target to unify with `Resumption k a b e`, unify
the supplied value with `a`, assign result type `b`, and accumulate effect row
`e`.

- [x] **Subtask 3.2.2.1 Complete**

#### Subtask 3.2.2.2: Report Dual-Origin Type Failures

**Description:** Relate invalid resume targets and supplied values to the
operation declaration, binder, delimiter, and resume expression rather than
reporting only an internal unification term.

- [x] **Subtask 3.2.2.2 Complete**

**Implementation evidence:** Normalized operation cases now resolve their
declared signatures, type their patterns, and bind explicit or synthetic
authority as `Resumption OneShot a b e`. Handler inference isolates the
handled computation, removes only handled labels, retains closed or open
residual rows, and relates every case result to the delimiter result.
`resume(target, value)` requires typed authority, checks the supplied
operation result, returns the delimiter result, and reintroduces known
residual effects. Typed transforms retain binder and resume evidence with
operation-declaration, binder, delimiter, target, and resume origins while
the backend remains fail-closed for explicit control. The focused Section
3.2 and surrounding Phase 2/type/effect/compiler suites pass 189 tests.

## Section 3.3: First-Class Flow And One-Shot Static Boundaries

**Description:** Permit resumption values to participate in normal typed
program structure while applying conservative diagnostics and deferrals where
the current type system lacks linearity or multi-shot safety.

- [x] **Section 3.3 Complete**

### Task 3.3.1: Support First-Class Resumption Values

**Description:** Preserve resumption types through variables, transform
parameters and results, closures, algebraic data, tuples, lists, records,
pattern bindings, and module-local call resolution.

- [x] **Task 3.3.1 Complete**

#### Subtask 3.3.1.1: Type Storage And Higher-Order Passing

**Description:** Infer programs that store, wrap, return, and pass resumptions
without erasing their kind, result types, residual row, or source origin.

- [x] **Subtask 3.3.1.1 Complete**

#### Subtask 3.3.1.2: Preserve Opaque Construction Authority

**Description:** Reject source attempts to construct, deconstruct, forge, or
pattern-match the runtime representation while allowing ordinary binding of
the opaque value.

- [x] **Subtask 3.3.1.2 Complete**

### Task 3.3.2: Enforce Conservative Consumption And Mode Rules

**Description:** Detect statically obvious one-shot misuse and keep unsupported
multi-shot behavior explicitly rejected until Phase 7.

- [x] **Task 3.3.2 Complete**

#### Subtask 3.3.2.1: Diagnose Obvious Duplicate Resume

**Description:** Reject simple duplicate or re-entrant uses in the same
normalized control region when provable, while documenting that runtime
consumption remains authoritative.

- [x] **Subtask 3.3.2.1 Complete**

#### Subtask 3.3.2.2: Gate Multi-Shot And Unsafe Escape

**Description:** Reject `MultiShot` construction and any statically known
unsupported residual effect or lifetime pattern with a dedicated deferred
diagnostic rather than approximating it as one-shot.

- [x] **Subtask 3.3.2.2 Complete**

**Implementation evidence:** Lexically bound values may now be checked as
first-class resume targets, so resumptions flow through transform parameters
and results, local and higher-order calls, closures, tuples, lists, records,
variants, and ordinary patterns without relying on binder names. Source
signatures accept the established `Resumption OneShot a b {}` spelling while
preserving all four internal roles. The compiler reserves `Resumption`,
`ResumptionKind`, `OneShot`, and `MultiShot` representation vocabulary and
rejects source construction, deconstruction, and shadowing attempts.
`catena_resumption_flow` rejects provable direct, aliased, and nested
one-shot reuse on a single path without conflating exclusive branches or
unknown higher-order invocation with a linearity proof. Concrete multi-shot
invocation fails with `unsupported_resumption_mode` and retains its residual
row in the diagnostic. The focused and surrounding suites pass 219 tests.

## Section 3.4: Phase 3 Integration Tests

**Description:** Prove that source handlers become fully typed first-class
resumption programs while every unsupported execution path remains
fail-closed before selective CPS exists.

- [ ] **Section 3.4 Complete**

### Task 3.4.1: Exercise Type And Effect Behavior

**Description:** Run parser-native source through normalization, kind
validation, inference, row solving, schemes, and typed-module assembly.

- [ ] **Task 3.4.1 Complete**

#### Subtask 3.4.1.1: Test Positive Resumption Typing

**Description:** Cover explicit and automatic handlers, polymorphic operation
results, nested delimiters, open residual rows, storage, return, higher-order
passing, and resume-result transformation.

- [ ] **Subtask 3.4.1.1 Complete**

#### Subtask 3.4.1.2: Test Negative Resumption Typing

**Description:** Cover wrong resume target, wrong supplied value, wrong
delimiter result, effect-row mismatch, forged representation, obvious
duplicate resume, and unimplemented multi-shot requests.

- [ ] **Subtask 3.4.1.2 Complete**

### Task 3.4.2: Run Phase Completion Gates

**Description:** Establish a stable typed frontend boundary ready for
control-mode analysis without advertising executable resumption semantics.

- [ ] **Task 3.4.2 Complete**

#### Subtask 3.4.2.1: Validate Typed Artifacts And Regressions

**Description:** Confirm typed modules retain all four resumption parameters,
source origins, and fail-closed dispositions and run existing kind, type,
effect, handler, stdlib, and backend-negative suites.

- [ ] **Subtask 3.4.2.1 Complete**

#### Subtask 3.4.2.2: Run Repository Gates

**Description:** Run Phase 3 integration tests, `make check-specs`, and the
complete active EUnit suite and publish the exact phase-ending evidence.

- [ ] **Subtask 3.4.2.2 Complete**
