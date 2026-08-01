# Phase 3: Compiler Frontend and Type Inference

**Description:** This phase clears the remaining non-effect compiler findings
from AST construction through parsing, semantic analysis, code generation,
module loading, type inference, unification, traits, and standard-library
instance metadata.

**Status:** Planned.

**Dependencies:** Phase 2 complete.

## Section 3.1: AST And Parser Contracts

**Description:** Make frontend constructors, parser wrappers, locations, and
pattern shapes agree with the values that the generated frontend and public
parse API actually produce.

- [ ] **Section 3.1 Complete**

### Task 3.1.1: Reconcile AST Constructors

**Description:** Resolve overspecified literals, invalid effect declaration
constructors, unreachable helpers, and location-shape mismatches.

- [ ] **Task 3.1.1 Complete**

#### Subtask 3.1.1.1: Tighten Literal And Pattern Values

**Description:** Align literal and pattern constructor specs with supported
numeric and binary values while preserving public AST records.

- [ ] **Subtask 3.1.1.1 Complete**

#### Subtask 3.1.1.2: Repair Declaration Construction

**Description:** Make type, effect, trait, and transform declaration defaults
produce valid records on every documented constructor path.

- [ ] **Subtask 3.1.1.2 Complete**

### Task 3.1.2: Reconcile Parser Wrappers

**Description:** Give parse and lex success/error results one truthful
maintained contract above the generated modules.

- [ ] **Task 3.1.2 Complete**

#### Subtask 3.1.2.1: Align Parse Result Types

**Description:** Cover tokens, forms, locations, recovery errors, and resource
limit failures without catch-all terms that hide impossible branches.

- [ ] **Subtask 3.1.2.1 Complete**

#### Subtask 3.1.2.2: Remove Unreachable Frontend Branches

**Description:** Delete genuinely dead clauses or correct upstream types when
a clause is intended to remain reachable.

- [ ] **Subtask 3.1.2.2 Complete**

## Section 3.2: Semantic And Codegen Contracts

**Description:** Reconcile semantic validation and lowering contracts across
the public typed compiler pipeline.

- [ ] **Section 3.2 Complete**

### Task 3.2.1: Repair Semantic Analysis Flow

**Description:** Resolve impossible pattern matches, missing local returns, and
call mismatches in clause validation, scope construction, and pattern checks.

- [ ] **Task 3.2.1 Complete**

#### Subtask 3.2.1.1: Align Validation Results

**Description:** Use one explicit success/error shape across semantic passes
and the public compiler orchestrator.

- [ ] **Subtask 3.2.1.1 Complete**

#### Subtask 3.2.1.2: Preserve Intended Diagnostics

**Description:** Add focused failures for guard purity, arity, bindings, and
source locations before removing branches Dialyzer marks unreachable.

- [ ] **Subtask 3.2.1.2 Complete**

### Task 3.2.2: Repair Lowering And Module Flow

**Description:** Align Core Erlang lowering, module construction, import
loading, and compiler utility contracts with their actual maps and tuples.

- [ ] **Task 3.2.2 Complete**

#### Subtask 3.2.2.1: Reconcile Lowering Inputs

**Description:** Narrow lowering specs only after tests prove the canonical AST
and typed forms reaching each code-generation stage.

- [ ] **Subtask 3.2.2.1 Complete**

#### Subtask 3.2.2.2: Reconcile Compiler Results

**Description:** Standardize compile, load, and code-generation result shapes
without swallowing typed frontend errors.

- [ ] **Subtask 3.2.2.2 Complete**

## Section 3.3: Type Inference And Trait Contracts

**Description:** Clear the compiler-types inventory after shared type exports
are stable, working from inference state and unification outward to traits and
stdlib instances.

- [ ] **Section 3.3 Complete**

### Task 3.3.1: Repair Inference And Unification

**Description:** Align substitution, state, constraint, expression, pattern,
effect, and unification contracts with successful and failing inference paths.

- [ ] **Task 3.3.1 Complete**

#### Subtask 3.3.1.1: Reconcile State Transitions

**Description:** Specify fresh-variable, environment, substitution, and
constraint updates without overly broad map contracts.

- [ ] **Subtask 3.3.1.1 Complete**

#### Subtask 3.3.1.2: Reconcile Error Returns

**Description:** Preserve occurs-check, mismatch, kind, and effect diagnostics
while removing impossible fallbacks.

- [ ] **Subtask 3.3.1.2 Complete**

### Task 3.3.2: Repair Traits And Instances

**Description:** Make trait lookup, resolution, kind validation, and stdlib
instance installation return explicit success/error values on all paths.

- [ ] **Task 3.3.2 Complete**

#### Subtask 3.3.2.1: Remove No-Return Instance Cascades

**Description:** Correct invalid database and environment contracts that make
instance helper functions appear unable to return.

- [ ] **Subtask 3.3.2.1 Complete**

#### Subtask 3.3.2.2: Verify Compiler-Type Coverage

**Description:** Run focused inference, unification, trait, HKT, and stdlib
compilation suites before requiring the owned directories to be warning-free.

- [ ] **Subtask 3.3.2.2 Complete**

**Acceptance Criteria:**

- Compiler AST, parser, semantic, codegen, root, error, and type directories
  have no remaining Dialyzer warnings
- Generated frontend files remain generated
- Public compile and parse error contracts remain explicit
- Focused compiler, pattern, type, trait, and stdlib suites pass
- `make verify` remains green
- The phase publishes its exact ending warning count
