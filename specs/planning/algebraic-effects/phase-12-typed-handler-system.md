# Phase 12: Typed Handler System

**Description:** This phase implements a fully typed handler system where handlers have explicit type signatures that specify their operations, input/output types, and effect rows.

---

## Section 12.1: Handler Type Definition

**Description:** Define handler types with full type signatures.

### Task 12.1.1: Handler Type Signature
**Description:** Define handler type signature in `src/compiler/effects/catena_handler_types.erl`.

**Subtasks:**
- Create `catena_handler_types.erl` module
- Define handler type with operations, input, output, effects
- Define operation signature type
- Define handler type constraints
- Add handler type validation

### Task 12.1.2: Operation Signature Type
**Description:** Define operation signature types.

**Subtasks:**
- Define operation signature type
- Add operation parameter types
- Define operation result type
- Add operation effect row type
- Implement operation signature validation

### Task 12.1.3: Handler Type Constructors
**Description:** Implement handler type constructor functions.

**Subtasks:**
- Implement handler type builder
- Add operation signature builder
- Implement handler type composition
- Add handler type validation
- Implement handler type pretty printing

---

## Section 12.2: Handler Type Checking

**Description:** Implement type checking for handlers.

### Task 12.2.1: Handler Type Checker
**Description:** Implement handler type checking in `src/compiler/effects/catena_handler_check.erl`.

**Subtasks:**
- Create `catena_handler_check.erl` module
- Implement handler type checking
- Add operation type checking
- Implement handler signature checking
- Add handler error reporting

### Task 12.2.2: Operation Coverage Checking
**Description:** Check that handlers cover all declared operations.

**Subtasks:**
- Implement operation coverage checking
- Add missing operation detection
- Implement redundant operation detection
- Add operation coverage reporting
- Implement operation coverage suggestions

### Task 12.2.3: Handler Return Type Checking
**Description:** Check that handler return values match expected types.

**Subtasks:**
- Implement handler return type checking
- Add resumption return type checking
- Implement handler effect type checking
- Add handler type inference
- Implement handler type error reporting

---

## Section 12.3: Handler Type Inference

**Description:** Implement type inference for handlers.

### Task 12.3.1: Handler Type Inference
**Description:** Infer handler types from handler implementations.

**Subtasks:**
- Implement handler type inference
- Add operation signature inference
- Implement handler effect inference
- Add handler type generalization
- Implement handler type inference debugging

### Task 12.3.2: Resumption Type Inference
**Description:** Infer types for resumptions within handlers.

**Subtasks:**
- Implement resumption type inference
- Add resumption parameter type inference
- Implement resumption return type inference
- Add resumption effect row inference
- Implement resumption type validation

### Task 12.3.3: Handler Context Type Inference
**Description:** Infer types for handler contexts (scoped computations).

**Subtasks:**
- Implement handler context type inference
- Add handler effect row subtraction
- Implement handler context validation
- Add handler context error reporting
- Implement handler context optimization

---

## Section 12.4: Typed Handler Execution

**Description:** Implement typed handler execution with runtime type checks.

### Task 12.4.1: Typed Handler Invocation
**Description:** Implement handler invocation with type checking.

**Subtasks:**
- Implement typed handler invocation
- Add operation type validation at runtime
- Implement handler return type validation
- Add handler effect validation
- Implement handler type error handling

### Task 12.4.2: Resumption Type Enforcement
**Description:** Enforce types when invoking resumptions.

**Subtasks:**
- Implement resumption type checking
- Add resumption parameter type validation
- Implement resumption return type validation
- Add resumption effect validation
- Implement resumption type error handling

### Task 12.4.3: Handler Type Coercion
**Description:** Implement safe type coercion for handlers.

**Subtasks:**
- Implement handler type coercion
- Add operation type coercion
- Implement resumption type coercion
- Add handler type subsumption
- Implement handler type coercion validation

---

## Section 12.5: Integration Tests

**Description:** Integration tests for typed handlers.

### Task 12.5.1: Handler Type Tests
**Description:** Test handler type definitions and checking.

**Subtasks:**
- Test handler type definition
- Test operation signature definition
- Test handler type validation
- Test handler type error cases
- Test handler type pretty printing

### Task 12.5.2: Handler Type Inference Tests
**Description:** Test handler type inference.

**Subtasks:**
- Test handler type inference
- Test operation signature inference
- Test resumption type inference
- Test handler context inference
- Test handler type inference errors

### Task 12.5.3: Typed Handler Execution Tests
**Description:** Test typed handler execution.

**Subtasks:**
- Test typed handler invocation
- Test resumption type enforcement
- Test handler type coercion
- Test handler type error handling
- Test complex typed handler scenarios

---

## Deliverables

### New Modules
- `src/compiler/effects/catena_handler_types.erl` - Handler type definitions
- `src/compiler/effects/catena_handler_check.erl` - Handler type checking
- `src/compiler/effects/catena_handler_infer.erl` - Handler type inference
- `src/compiler/effects/catena_handler_exec.erl` - Typed handler execution

### Modified Modules
- `src/compiler/effects/catena_handler.erl` - Type integration
- `src/compiler/effects/catena_resumption.erl` - Typed resumptions
- `src/compiler/types/catena_infer.erl` - Handler type inference integration
- `src/compiler/parser/catena_parser.yrl` - Handler type syntax

### Test Modules
- `test/compiler/effects/catena_handler_types_tests.erl`
- `test/compiler/effects/catena_handler_check_tests.erl`
- `test/compiler/effects/catena_handler_infer_tests.erl`
- `test/compiler/effects/catena_typed_handler_integration_tests.erl`
