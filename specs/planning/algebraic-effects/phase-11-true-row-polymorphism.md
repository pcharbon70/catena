# Phase 11: True Row Polymorphism

**Description:** This phase implements true row polymorphism for effect types, enabling extensible effect sets with proper row variables and effect set operations.

---

## Section 11.1: Row Type Definition

**Description:** Define row types and row variables for extensible effect sets.

### Task 11.1.1: Row Type Definition
**Description:** Define row types in `src/compiler/types/catena_row_types.erl`.

**Subtasks:**
- Create `catena_row_types.erl` module
- Define `-type effect_row() :: {effect_row, [atom() | row_var()]}.`
- Define `-type row_var() :: {row_var, var_id()}.`
- Define row type constructors
- Add row type validation

### Task 11.1.2: Row Variable Generation
**Description:** Implement fresh row variable generation.

**Subtasks:**
- Implement `fresh_row_var/1` (infer_state) function
- Add row variable uniqueness tracking
- Implement row variable naming
- Add row variable scoping
- Implement row variable garbage collection

### Task 11.1.3: Row Type Operations
**Description:** Implement basic row type operations.

**Subtasks:**
- Implement `row_union/2` function
- Implement `row_intersection/2` function
- Implement `row_difference/2` function
- Implement `row_contains/2` function
- Add row type normalization

---

## Section 11.2: Row Unification

**Description:** Implement unification for row types with occurs check.

### Task 11.2.1: Row Unification Algorithm
**Description:** Implement row type unification in `src/compiler/types/catena_row_unify.erl`.

**Subtasks:**
- Create `catena_row_unify.erl` module
- Implement row-variable unification
- Implement concrete-row unification
- Add row occurs check
- Implement row substitution

### Task 11.2.2: Row Substitution
**Description:** Implement substitution for row variables.

**Subtasks:**
- Implement `apply_row_subst/2` (row, substitution) function
- Add row substitution composition
- Implement row substitution optimization
- Add row substitution validation
- Implement row substitution debugging

### Task 11.2.3: Row Constraint Solving
**Description:** Implement constraint solving for row types.

**Subtasks:**
- Implement row constraint generation
- Add row constraint solver
- Implement row constraint propagation
- Add row constraint validation
- Implement row constraint error reporting

---

## Section 11.3: Effect Set Operations

**Description:** Implement effect set operations using row polymorphism.

### Task 11.3.1: Effect Union with Rows
**Description:** Implement effect union preserving row variables.

**Subtasks:**
- Implement `effect_union_rows/2` function
- Add row variable preservation
- Implement effect union normalization
- Add effect union optimization
- Implement effect union validation

### Task 11.3.2: Effect Difference with Rows
**Description:** Implement effect difference (subtraction) with rows.

**Subtasks:**
- Implement `effect_difference/2` function
- Add row variable handling in difference
- Implement effect difference normalization
- Add effect difference validation
- Implement effect difference error cases

### Task 11.3.3: Effect Subsumption
**Description:** Implement effect subsumption checking.

**Subtasks:**
- Implement `effect_subsumes/2` function
- Add row variable subsumption rules
- Implement subsumption optimization
- Add subsumption validation
- Implement subsumption debugging

---

## Section 11.4: Row Polymorphic Type Inference

**Description:** Extend type inference to handle row polymorphic effects.

### Task 11.4.1: Row Polymorphic Function Types
**Description:** Infer row polymorphic effect types for functions.

**Subtasks:**
- Extend `catena_infer` for row polymorphism
- Implement row variable generalization
- Add row variable instantiation
- Implement row polymorphic let-polymorphism
- Add row polymorphic constraint generation

### Task 11.4.2: Row Polymorphic Operations
**Description:** Infer row types for effect operations.

**Subtasks:**
- Implement operation effect row inference
- Add row variable propagation
- Implement operation row constraints
- Add operation row generalization
- Implement operation row validation

### Task 11.4.3: Row Polymorphic Handlers
**Description:** Infer row types for handlers.

**Subtasks:**
- Implement handler row type inference
- Add handler row variable handling
- Implement handler row subsumption
- Add handler row validation
- Implement handler row error reporting

---

## Section 11.5: Integration Tests

**Description:** Integration tests for row polymorphism.

### Task 11.5.1: Row Type Tests
**Description:** Test row type operations.

**Subtasks:**
- Test row variable creation
- Test row union/intersection/difference
- Test row unification
- Test row substitution
- Test row constraint solving

### Task 11.5.2: Effect Set Operation Tests
**Description:** Test effect operations with rows.

**Subtasks:**
- Test effect union with rows
- Test effect difference with rows
- Test effect subsumption
- Test effect normalization
- Test effect error cases

### Task 11.5.3: Row Polymorphic Inference Tests
**Description:** Test type inference with row polymorphism.

**Subtasks:**
- Test row polymorphic function inference
- Test row polymorphic operation inference
- Test row polymorphic handler inference
- Test row variable generalization/instantiation
- Test row polymorphic error cases

---

## Deliverables

### New Modules
- `src/compiler/types/catena_row_types.erl` - Row type definitions
- `src/compiler/types/catena_row_unify.erl` - Row unification
- `src/compiler/types/catena_row_subst.erl` - Row substitution
- `src/compiler/types/catena_row_constraints.erl` - Row constraints

### Modified Modules
- `src/compiler/types/catena_types.erl` - Row type integration
- `src/compiler/types/catena_infer.erl` - Row polymorphic inference
- `src/compiler/types/catena_effect_poly.erl` - True row polymorphism
- `src/compiler/types/catena_infer_unify.erl` - Row unification integration

### Test Modules
- `test/compiler/types/catena_row_types_tests.erl`
- `test/compiler/types/catena_row_unify_tests.erl`
- `test/compiler/types/catena_row_constraints_tests.erl`
- `test/compiler/types/catena_row_polymorphism_integration_tests.erl`
