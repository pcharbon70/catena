# Phase 2: Standard Library and Frontend Validation

**Description:** This phase reconciles the standard-library and frontend
contracts represented by the 29 Phase 2 failures in the verified Phase 1
baseline. It restores higher-kinded trait validation, makes parsed
import/export forms flow through compilation, and aligns conformance fixtures
with the canonical language syntax used by the active standard library.

**Status:** Complete.

**Dependencies:** Phase 1 complete.

## Section 2.1: Higher-Kinded Trait Validation

**Description:** Repair trait-method kind inference so signatures, default
methods, instances, and complete standard-library modules can be validated
without runtime crashes or lost kind evidence.

- [x] **Section 2.1 Complete**

### Task 2.1.1: Correct Trait Method Folding

**Description:** Make kind inference fold over every trait member with the
correct accumulator contract.

- [x] **Task 2.1.1 Complete**

**Subtasks:**

- [x] 2.1.1.1 Accept the current maximum arity in the fold callback
- [x] 2.1.1.2 Compare signature evidence with the accumulated maximum
- [x] 2.1.1.3 Ignore non-signature members without resetting prior evidence

### Task 2.1.2: Preserve Higher-Kinded Evidence

**Description:** Verify unary and binary type-constructor kinds remain stable
when a trait also contains default implementations.

- [x] **Task 2.1.2 Complete**

**Subtasks:**

- [x] 2.1.2.1 Cover a signature followed by a default method
- [x] 2.1.2.2 Validate unary constructors such as `Mapper f`
- [x] 2.1.2.3 Validate binary constructors such as `System arr`

### Task 2.1.3: Restore Prelude Kind Validation

**Description:** Run focused HKT and Prelude compilation suites through the
corrected validator.

- [x] **Task 2.1.3 Complete**

**Acceptance Criteria:**

- Trait validation does not crash on mixed signature/default member lists
- Focused HKT validation passes for unary and binary constructors
- Prelude kind checking and compilation pass

## Section 2.2: Canonical Import and Export Contracts

**Description:** Carry the parser's six-field import representation and
list-based exports through name resolution and compilation so imported
constructors are actually available to type inference.

- [x] **Section 2.2 Complete**

### Task 2.2.1: Normalize Parsed Module Metadata

**Description:** Make tests and semantic consumers use the canonical import and
export AST forms emitted by the parser.

- [x] **Task 2.2.1 Complete**

### Task 2.2.2: Compile With Imported Constructors

**Description:** Load exported type constructors from parsed imports and merge
them into the local type environment with local bindings taking precedence.

- [x] **Task 2.2.2 Complete**

### Task 2.2.3: Validate Import Boundaries

**Description:** Exercise simple, dotted, selective, qualified, and missing
module imports at parser and compilation boundaries.

- [x] **Task 2.2.3 Complete**

**Acceptance Criteria:**

- Parser tests assert the six-field import AST
- Name resolution accepts parser-native export lists
- Prelude constructors compile when brought into scope by an import

## Section 2.3: Canonical Frontend Syntax and Verified Baseline

**Description:** Align standard-library validation fixtures with module-scoped
exports and `extend` trait inheritance, then publish the reduced full-suite
baseline.

- [x] **Section 2.3 Complete**

### Task 2.3.1: Validate Module-Scoped Exports

**Description:** Test export declarations in the module-header context required
by the grammar and used by active standard-library files.

- [x] **Task 2.3.1 Complete**

### Task 2.3.2: Validate Trait Inheritance Syntax

**Description:** Use the canonical `extend` keyword for single and multiple
trait constraints and remove stale colon-based fixtures.

- [x] **Task 2.3.2 Complete**

### Task 2.3.3: Publish Phase 2 Verification

**Description:** Run the focused Phase 2 suite and complete active suite, record
the new totals, and update promoted quality status without hiding later-phase
failures.

- [x] **Task 2.3.3 Complete**

**Acceptance Criteria:**

- Canonical export and multiple-inheritance examples parse successfully
- All 29 failures assigned to Phase 2 are resolved
- The complete active suite has a deterministic revised baseline

## Phase Completion Gate

**Description:** Phase 2 completes when its six assigned modules are green,
standard-library Prelude compilation succeeds, and the remaining active-suite
failures belong only to later reconciliation phases.

- [x] All Phase 2 focused modules pass
- [x] Prelude parsing, kind validation, imports, and type checking pass
- [x] The complete active-suite baseline is updated
- [x] Current-status and standard-library specs match verified behavior
