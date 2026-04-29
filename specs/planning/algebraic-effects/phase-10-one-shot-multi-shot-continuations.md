# Phase 10: One-Shot vs Multi-Shot Continuations

**Description:** This phase implements explicit one-shot and multi-shot continuation semantics, enabling precise control over resource management and enabling patterns like backtracking and nondeterminism.

---

## Section 10.1: Continuation Kind Type

**Description:** Define the continuation kind type and associated semantics.

### Task 10.1.1: Continuation Kind Definition
**Description:** Define continuation kind types in `src/compiler/effects/catena_continuation_kind.erl`.

**Subtasks:**
- Create `catena_continuation_kind.erl` module
- Define `-type continuation_kind() :: one_shot | multi_shot.`
- Define continuation kind semantics documentation
- Export kind constructor functions
- Add kind validation and conversion

### Task 10.1.2: One-Shot Semantics
**Description:** Specify one-shot continuation semantics.

**Subtasks:**
- Define one-shot operational semantics
- Document one-shot resource guarantees
- Specify one-shot error handling
- Add one-shot use case documentation
- Implement one-shot examples

### Task 10.1.3: Multi-Shot Semantics
**Description:** Specify multi-shot continuation semantics.

**Subtasks:**
- Define multi-shot operational semantics
- Document multi-shot state duplication rules
- Specify multi-shot resource implications
- Add multi-shot use case documentation
- Implement multi-shot examples

---

## Section 10.2: One-Shot Continuations

**Description:** Implement one-shot continuations that can only be resumed once.

### Task 10.2.1: One-Shot Resumption Type
**Description:** Define and implement one-shot resumption type.

**Subtasks:**
- Define one-shot resumption type
- Implement one-shot state tracking
- Add one-shot validation
- Implement one-shot consumption
- Add one-shot error detection

### Task 10.2.2: One-Shot Capture
**Description:** Implement one-shot continuation capture.

**Subtasks:**
- Implement `capture_one_shot/0` function
- Add one-shot continuation state capture
- Implement one-shot resource tracking
- Add one-shot capture validation
- Implement one-shot capture debugging

### Task 10.2.3: One-Shot Resume
**Description:** Implement one-shot resume with validation.

**Subtasks:**
- Implement `resume_one_shot/2` function
- Add one-shot resume validation
- Implement one-shot consumption tracking
- Add one-shot double-resume detection
- Implement one-shot error handling

---

## Section 10.3: Multi-Shot Continuations

**Description:** Implement multi-shot continuations that can be resumed multiple times.

### Task 10.3.1: Multi-Shot Resumption Type
**Description:** Define and implement multi-shot resumption type.

**Subtasks:**
- Define multi-shot resumption type
- Implement multi-shot state copying
- Add multi-shot state sharing options
- Implement multi-shot reference counting
- Add multi-shot validation

### Task 10.3.2: Multi-Shot Capture
**Description:** Implement multi-shot continuation capture with state copying.

**Subtasks:**
- Implement `capture_multi_shot/0` function
- Add multi-shot continuation state capture
- Implement multi-shot state deep copy
- Add multi-shot shallow copy option
- Implement multi-shot capture optimization

### Task 10.3.3: Multi-Shot Resume
**Description:** Implement multi-shot resume with state management.

**Subtasks:**
- Implement `resume_multi_shot/2` function
- Add multi-shot state restoration
- Implement multi-shot state tracking
- Add multi-shot resume counting
- Implement multi-shot resource management

---

## Section 10.4: State Duplication and Sharing

**Description:** Implement state duplication strategies for multi-shot continuations.

### Task 10.4.1: Deep Copy Strategy
**Description:** Implement deep copy for complete state duplication.

**Subtasks:**
- Implement `deep_copy/1` function
- Add deep copy for common types
- Implement deep copy for custom types
- Add deep copy optimization
- Implement deep copy validation

### Task 10.4.2: Shallow Copy Strategy
**Description:** Implement shallow copy for shared state.

**Subtasks:**
- Implement `shallow_copy/1` function
- Add shallow copy semantics
- Implement shallow copy reference counting
- Add shallow copy garbage collection
- Implement shallow copy validation

### Task 10.4.3: Selective Copy Strategy
**Description:** Implement selective copy for fine-grained control.

**Subtasks:**
- Implement selective copy specification
- Add selective copy annotations
- Implement selective copy optimization
- Add selective copy validation
- Implement selective copy debugging

---

## Section 10.5: Integration Tests

**Description:** Integration tests for one-shot vs multi-shot continuations.

### Task 10.5.1: One-Shot Continuation Tests
**Description:** Test one-shot continuation behavior.

**Subtasks:**
- Test one-shot resumption succeeds once
- Test one-shot double-resume fails
- Test one-shot resource cleanup
- Test one-shot error handling
- Test one-shot nested continuations

### Task 10.5.2: Multi-Shot Continuation Tests
**Description:** Test multi-shot continuation behavior.

**Subtasks:**
- Test multi-shot resumption multiple times
- Test multi-shot state duplication
- Test multi-shot state sharing
- Test multi-shot resource management
- Test multi-shot nested continuations

### Task 10.5.3: Backtracking and Nondeterminism
**Description:** Test patterns enabled by multi-shot continuations.

**Subtasks:**
- Test backtracking with multi-shot
- Test ambivalent choice operator
- Test nondeterministic search
- Test solution enumeration
- Test constraint solving

---

## Deliverables

### New Modules
- `src/compiler/effects/catena_continuation_kind.erl` - Continuation kind types
- `src/compiler/effects/catena_one_shot.erl` - One-shot continuation implementation
- `src/compiler/effects/catena_multi_shot.erl` - Multi-shot continuation implementation
- `src/compiler/effects/catena_state_copy.erl` - State duplication strategies

### Modified Modules
- `src/compiler/effects/catena_resumption.erl` - Continuation kind support
- `src/compiler/effects/catena_handler.erl` - Kind-aware handlers
- `src/compiler/effects/catena_effect_advanced.erl` - Kind selection API

### Test Modules
- `test/compiler/effects/catena_continuation_kind_tests.erl`
- `test/compiler/effects/catena_one_shot_tests.erl`
- `test/compiler/effects/catena_multi_shot_tests.erl`
- `test/compiler/effects/catena_continuation_kind_integration_tests.erl`
