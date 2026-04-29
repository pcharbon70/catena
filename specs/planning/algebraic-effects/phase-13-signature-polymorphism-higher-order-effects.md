# Phase 13: Signature-Based Polymorphism and Higher-Order Effects

**Description:** This phase implements signature-based restrictions on effect polymorphism and support for higher-order effects (operations that take effectful functions as arguments), completing the transformation to a full algebraic effects system.

---

## Section 13.1: Operation Signatures

**Description:** Define operation signatures with row variables for polymorphism restrictions.

### Task 13.1.1: Operation Signature Definition
**Description:** Define operation signature types with row variables in `src/compiler/effects/catena_op_signatures.erl`.

**Subtasks:**
- Create `catena_op_signatures.erl` module
- Define operation signature with row_var field
- Define signature scoping rules
- Add signature validation
- Implement signature constructors

### Task 13.1.2: Signature Row Variable Scoping
**Description:** Implement row variable scoping in operation signatures.

**Subtasks:**
- Implement row variable scope tracking
- Add row variable scope validation
- Implement row variable scope exit
- Add row variable scope error detection
- Implement row variable scope debugging

### Task 13.1.3: Signature Polymorphism Restrictions
**Description:** Implement restrictions on effect polymorphism based on signatures.

**Subtasks:**
- Implement signature-based polymorphism checking
- Add row variable escape detection
- Implement signature validity checking
- Add signature error reporting
- Implement signature optimization

---

## Section 13.2: Higher-Order Effect Types

**Description:** Implement types for higher-order effects (operations with effectful parameters).

### Task 13.2.1: Higher-Order Operation Type
**Description:** Define types for higher-order operations in `src/compiler/effects/catena_ho_effects.erl`.

**Subtasks:**
- Create `catena_ho_effects.erl` module
- Define higher-order operation type
- Define effectful function parameter type
- Add higher-order operation validation
- Implement higher-order operation constructors

### Task 13.2.2: Effectful Parameter Type Inference
**Description:** Infer types for effectful function parameters.

**Subtasks:**
- Implement effectful parameter type inference
- Add effectful parameter effect row inference
- Implement effectful parameter validation
- Add effectful parameter error reporting
- Implement effectful parameter optimization

### Task 13.2.3: Higher-Order Effect Substitution
**Description:** Implement substitution for higher-order effect variables.

**Subtasks:**
- Implement higher-order effect substitution
- Add effect variable substitution in parameters
- Implement higher-order effect unification
- Add higher-order effect validation
- Implement higher-order effect debugging

---

## Section 13.3: Hefty Algebras

**Description:** Implement hefty algebras (trees of effect handlers) for higher-order effects.

### Task 13.3.1: Hefty Algebra Type
**Description:** Define hefty algebra types in `src/compiler/effects/catena_hefty.erl`.

**Subtasks:**
- Create `catena_hefty.erl` module
- Define hefty tree type
- Define hefty handler type
- Add hefty algebra validation
- Implement hefty algebra constructors

### Task 13.3.2: Hefty Tree Construction
**Description:** Implement construction of hefty trees from effectful code.

**Subtasks:**
- Implement hefty tree construction
- Add hefty tree optimization
- Implement hefty tree validation
- Add hefty tree pretty printing
- Implement hefty tree debugging

### Task 13.3.3: Hefty Handler Interpretation
**Description:** Implement interpretation of hefty trees by handlers.

**Subtasks:**
- Implement hefty tree interpretation
- Add hefty handler composition
- Implement hefty optimization
- Add hefty error handling
- Implement hefty debugging

---

## Section 13.4: Higher-Order Effect Execution

**Description:** Implement execution semantics for higher-order effects.

### Task 13.4.1: Higher-Order Operation Execution
**Description:** Implement execution of higher-order operations.

**Subtasks:**
- Implement higher-order operation execution
- Add effectful parameter invocation
- Implement higher-order effect propagation
- Add higher-order error handling
- Implement higher-order debugging

### Task 13.4.2: Effectual Handler Invocation
**Description:** Implement handlers that can invoke effectful operations.

**Subtasks:**
- Implement effectual handler invocation
- Add handler effect row preservation
- Implement effectual handler composition
- Add effectual handler optimization
- Implement effectual handler debugging

### Task 13.4.3: Higher-Order Effect Optimization
**Description:** Implement optimization for higher-order effects.

**Subtasks:**
- Implement higher-order effect fusion
- Add higher-order effect inlining
- Implement higher-order effect specialization
- Add higher-order effect dead code elimination
- Implement higher-order effect validation

---

## Section 13.5: Integration Tests

**Description:** Integration tests for signature-based polymorphism and higher-order effects.

### Task 13.5.1: Signature-Based Polymorphism Tests
**Description:** Test signature-based effect polymorphism restrictions.

**Subtasks:**
- Test operation signature definition
- Test row variable scoping
- Test polymorphism restriction validation
- Test row variable escape detection
- Test signature error cases

### Task 13.5.2: Higher-Order Effect Tests
**Description:** Test higher-order effect operations.

**Subtasks:**
- Test higher-order operation type inference
- Test effectful parameter invocation
- Test higher-order effect propagation
- Test higher-order effect composition
- Test higher-order error handling

### Task 13.5.3: Hefty Algebra Tests
**Description:** Test hefty algebra construction and interpretation.

**Subtasks:**
- Test hefty tree construction
- Test hefty handler interpretation
- Test hefty optimization
- Test hefty composition
- Test hefty error cases

---

## Deliverables

### New Modules
- `src/compiler/effects/catena_op_signatures.erl` - Operation signatures
- `src/compiler/effects/catena_ho_effects.erl` - Higher-order effect types
- `src/compiler/effects/catena_hefty.erl` - Hefty algebras
- `src/compiler/effects/catena_hefty_interpreter.erl` - Hefty interpretation

### Modified Modules
- `src/compiler/effects/catena_handler.erl` - Higher-order handler support
- `src/compiler/types/catena_infer.erl` - Higher-order effect inference
- `src/compiler/types/catena_row_types.erl` - Higher-order row variables
- `src/compiler/parser/catena_parser.yrl` - Higher-order effect syntax

### Test Modules
- `test/compiler/effects/catena_op_signatures_tests.erl`
- `test/compiler/effects/catena_ho_effects_tests.erl`
- `test/compiler/effects/catena_hefty_tests.erl`
- `test/compiler/effects/catena_higher_order_integration_tests.erl`
