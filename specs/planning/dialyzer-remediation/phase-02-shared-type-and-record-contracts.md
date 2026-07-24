# Phase 2: Shared Type and Record Contracts

**Description:** This phase repairs shared exported types, callbacks, records,
and constructor defaults that currently create repeated downstream warnings
across compiler, runtime, testing, and property modules.

**Status:** Planned.

**Dependencies:** Phase 1 complete.

## Section 2.1: Exported Type Ownership

**Description:** Give every cross-module type reference a real owning module
and exported definition so Dialyzer can analyze callers instead of treating
their contracts as unknown.

- [ ] **Section 2.1 Complete**

### Task 2.1.1: Repair Core Compiler Type Exports

**Description:** Reconcile high-fan-out types such as
`catena_types:type/0`, constraint locations, and inference state.

- [ ] **Task 2.1.1 Complete**

#### Subtask 2.1.1.1: Export Canonical Public Types

**Description:** Add or correct `-export_type` declarations for types that are
intentionally consumed across module boundaries.

- [ ] **Subtask 2.1.1.1 Complete**

#### Subtask 2.1.1.2: Remove Stale Remote Type References

**Description:** Replace references to private, renamed, or nonexistent types
with the current canonical owner.

- [ ] **Subtask 2.1.1.2 Complete**

### Task 2.1.2: Repair Testing And Runtime Type Exports

**Description:** Reconcile shared runner results, batch results, runtime
contexts, callback state, and law-execution metadata.

- [ ] **Task 2.1.2 Complete**

#### Subtask 2.1.2.1: Align Runner Result Types

**Description:** Ensure property, batch, law, and compatibility-layer result
types describe the same maintained execution path.

- [ ] **Subtask 2.1.2.1 Complete**

#### Subtask 2.1.2.2: Align Callback Metadata

**Description:** Export or localize callback and behavior types so
implementations can be checked without unknown-type fallbacks.

- [ ] **Subtask 2.1.2.2 Complete**

## Section 2.2: Record Defaults And Constructor Truth

**Description:** Make record declarations, defaults, constructors, and public
specifications agree on which fields are required, optional, or initialized.

- [ ] **Section 2.2 Complete**

### Task 2.2.1: Repair Invalid Record Construction

**Description:** Resolve the initial six record-field mismatches and the
constructor no-return cascades they cause.

- [ ] **Task 2.2.1 Complete**

#### Subtask 2.2.1.1: Reconcile Optional Defaults

**Description:** Replace invalid `undefined` defaults or overly narrow field
types with one truthful representation of optionality.

- [ ] **Subtask 2.2.1.1 Complete**

#### Subtask 2.2.1.2: Exercise Constructor Paths

**Description:** Add focused tests for default and fully populated
construction, including AST effect declarations and law-parameter records.

- [ ] **Subtask 2.2.1.2 Complete**

### Task 2.2.2: Rebaseline Shared Contract Warnings

**Description:** Rerun analysis after the shared fixes and distinguish removed
root causes from residual subsystem-owned specifications.

- [ ] **Task 2.2.2 Complete**

#### Subtask 2.2.2.1: Require Zero Unknown Types

**Description:** Verify that every maintained remote type resolves and that
the unknown-type inventory is empty.

- [ ] **Subtask 2.2.2.1 Complete**

#### Subtask 2.2.2.2: Publish The Phase Delta

**Description:** Record warnings removed by shared metadata, warnings exposed
after cascade removal, and the exact Phase 3 input count.

- [ ] **Subtask 2.2.2.2 Complete**

**Acceptance Criteria:**

- Maintained remote types resolve to exported canonical owners
- Unknown-type warnings are zero
- Initial record-field mismatches are resolved
- Constructors covered by this phase have valid local returns
- Focused constructor and callback tests pass
- `make verify` remains green
- The phase publishes its exact ending warning count
