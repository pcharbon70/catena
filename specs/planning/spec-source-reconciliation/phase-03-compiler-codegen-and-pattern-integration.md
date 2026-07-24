# Phase 3: Compiler, Codegen, and Pattern Integration

**Description:** This phase turns the compiler, Core Erlang generator, and
pattern subsystem from separately tested components into an executable
frontend-to-backend path. It reconciles parser-native AST shapes at the
code-generation boundary, enforces promoted pattern contracts during
compilation, and publishes a verified Phase 3 baseline without hiding failures
owned by later phases.

**Status:** Complete.

**Dependencies:** Phase 2 complete.

## Section 3.1: Canonical Frontend-to-Codegen Lowering

**Description:** Introduce an explicit lowering boundary from the canonical
parser and semantic AST into the smaller backend AST used by Core Erlang
generation.

- [x] **Section 3.1 Complete**

### Task 3.1.1: Normalize Canonical AST Shapes

**Description:** Lower parser-native literals, operators, bindings, records,
constructors, lambdas, and match expressions without placeholder output.

- [x] **Task 3.1.1 Complete**

### Task 3.1.2: Lower Transform Clauses

**Description:** Convert canonical `transform_decl` nodes, including guarded
and multi-clause definitions, into single Core Erlang function bodies with
explicit pattern matches.

- [x] **Task 3.1.2 Complete**

### Task 3.1.3: Preserve Module Exports

**Description:** Make Core Erlang module exports follow parser-native export
declarations while retaining the current export-all default for modules
without an explicit list.

- [x] **Task 3.1.3 Complete**

**Acceptance Criteria:**

- An analyzed parser-native module generates a valid Core Erlang module
- Multi-clause transforms retain their arity and clause patterns
- Explicit transform exports exclude non-exported transforms

## Section 3.2: Compiler-Enforced Pattern Contracts

**Description:** Move promoted pattern invariants into the semantic and backend
integration paths so parser success alone cannot admit invalid guards,
inconsistent clauses, or lossy pattern lowering.

- [x] **Section 3.2 Complete**

### Task 3.2.1: Enforce Guard Purity

**Description:** Recognize canonical effectful guard expressions and reject
them during semantic analysis before type inference or code generation.

- [x] **Task 3.2.1 Complete**

### Task 3.2.2: Validate Clause and Or-Pattern Bindings

**Description:** Require consistent transform arity and identical bound-name
sets across every or-pattern alternative.

- [x] **Task 3.2.2 Complete**

### Task 3.2.3: Validate Pattern Code Generation

**Description:** Exercise parser-native constructor, tuple, list, cons,
as-pattern, or-pattern, guard, and match-expression lowering through Core
Erlang validation.

- [x] **Task 3.2.3 Complete**

**Acceptance Criteria:**

- Effectful parser-native guards fail with a semantic error
- Inconsistent transform arity and or-pattern bindings fail deterministically
- Canonical advanced patterns reach valid Core Erlang structures

## Section 3.3: Public Core Pipeline and Verified Baseline

**Description:** Expose a stage-oriented source-to-Core entry point that
requires successful semantic, kind, and type validation, then publish the
focused and complete-suite Phase 3 evidence.

- [x] **Section 3.3 Complete**

### Task 3.3.1: Add Source-to-Core Entry Points

**Description:** Add string and file compiler APIs that reuse the canonical
frontend stages before invoking Core Erlang generation.

- [x] **Task 3.3.1 Complete**

### Task 3.3.2: Preserve Stage-Specific Failures

**Description:** Keep lexer, parser, semantic, kind, type, import, and codegen
errors distinguishable at the public Core compilation boundary.

- [x] **Task 3.3.2 Complete**

### Task 3.3.3: Publish Phase 3 Verification

**Description:** Run focused compiler/codegen/pattern suites and the complete
active suite, record deterministic totals, and update promoted compiler and
quality status.

- [x] **Task 3.3.3 Complete**

**Acceptance Criteria:**

- Valid source reaches a BEAM-compilable Core Erlang module
- Invalid source stops at its originating compiler stage
- Phase 3 focused tests pass and the later-phase failure inventory is retained

## Phase Completion Gate

**Description:** Phase 3 completes when canonical source reaches validated Core
Erlang through the public compiler boundary, pattern invariants are enforced
in that path, and the repository records a deterministic revised baseline.

- [x] Canonical frontend AST lowers without placeholder backend nodes
- [x] Pattern guard, binding, and advanced lowering contracts pass
- [x] Public source-to-Core compilation validates typed source first
- [x] Current-status and compiler specs match verified behavior
