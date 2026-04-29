# Phase 7: Handler/Resumption Core Model

**Description:** This phase implements the fundamental abstraction that distinguishes algebraic effects from simple effect tracking: the handler/resumption model. Handlers receive both operation values and resumptions (continuations representing the rest of the computation), enabling them to decide whether to resume, abort, or handle operations multiple times.

---

## Section 7.1: Resumption Type and Capture

**Description:** Implement the core resumption type representing "the rest of the computation after an operation" and the mechanisms to capture continuations as resumptions.

### Task 7.1.1: Resumption Type Definition
**Description:** Define the resumption type and associated data structures in `src/compiler/effects/catena_resumption.erl`.

**Subtasks:**
- Create `catena_resumption.erl` module
- Define `-type resumption() :: fun((term()) -> term()).`
- Define resumption metadata type (captured at, stack depth, etc.)
- Export resumption constructor and accessor functions
- Add resumption validation (arity checks, closure inspection)

### Task 7.1.2: Continuation Capture Mechanism
**Description:** Implement the low-level mechanism to capture the current continuation as a resumption.

**Subtasks:**
- Implement `capture_continuation/0` function
- Use Erlang/OTP process dictionary or explicit state for continuation storage
- Create continuation stack tracking module
- Implement stack unwinding to capture point
- Add continuation reconstruction from stack frames

### Task 7.1.3: Resumption Execution
**Description:** Implement the mechanism to execute a resumption with a given value.

**Subtasks:**
- Implement `resume/2` function (resumption, value)
- Handle resumption execution errors
- Add resumption stack trace capture
- Implement resumption timeout mechanisms
- Add resumption execution telemetry

---

## Section 7.2: Handler Type and Interface

**Description:** Define the handler type and interface for handlers that receive both operation values and resumptions.

### Task 7.2.1: Handler Type Definition
**Description:** Define the handler type and operation signature types in `src/compiler/effects/catena_handler.erl`.

**Subtasks:**
- Create `catena_handler.erl` module
- Define `-type handler() :: #{operation => atom(), handler => fun((term(), resumption()) -> term())}.`
- Define handler metadata type
- Export handler constructor and accessor functions
- Add handler validation (operation matching, arity)

### Task 7.2.2: Handler Registration and Lookup
**Description:** Implement the handler registry for dynamic handler installation and lookup.

**Subtasks:**
- Implement `register_handler/2` (operation, handler)
- Implement `unregister_handler/1` (operation)
- Implement `lookup_handler/1` (operation)
- Create handler stack for nested scopes
- Add handler conflict detection and resolution

### Task 7.2.3: Handler Execution Protocol
**Description:** Define and implement the protocol for executing handlers with resumptions.

**Subtasks:**
- Define handler execution callback signature
- Implement `execute_handler/3` (handler, operation_value, resumption)
- Add handler error handling and recovery
- Implement handler timeout and cancellation
- Add handler execution logging

---

## Section 7.3: Perform Operation with Resumption

**Description:** Redefine the `perform` operation to suspend computation, capture resumption, and invoke handlers.

### Task 7.3.1: Perform Operation Redesign
**Description:** Redesign `perform/2` to work with the handler/resumption model.

**Subtasks:**
- Modify `catena_effects:perform/2` to suspend computation
- Implement continuation capture at perform point
- Add handler lookup and invocation
- Implement resumption passing to handler
- Add perform operation context tracking

### Task 7.3.2: Handler Stack Management
**Description:** Implement proper handler stack for nested effect handling.

**Subtasks:**
- Create handler stack module `catena_handler_stack.erl`
- Implement `push_handlers/1` for entering new scopes
- Implement `pop_handlers/0` for exiting scopes
- Add handler stack serialization
- Implement handler stack traversal for operation lookup

### Task 7.3.3: Computation Suspension and Resumption
**Description:** Implement the low-level suspension mechanism that enables handlers to receive resumptions.

**Subtasks:**
- Implement `suspend/2` (operation, value)
- Create suspension state management
- Add suspension point tracking
- Implement resumption from suspension
- Add suspension/resumption cycle validation

---

## Section 7.4: Handler Control Flow

**Description:** Implement the three fundamental handler control flow patterns: resume, abort, and multi-shot handle.

### Task 7.4.1: Resume Operation
**Description:** Implement the resume operation that passes control back to the suspended computation.

**Subtasks:**
- Implement `resume/2` (resumption, value)
- Define resume semantics (value passing, context restoration)
- Add resume error propagation
- Implement nested resume handling
- Add resume validation (one-shot vs multi-shot)

### Task 7.4.2: Abort Operation
**Description:** Implement the abort operation that discards the resumption entirely.

**Subtasks:**
- Implement `abort/1` (value) operation
- Define abort semantics (cleanup, resource release)
- Add abort error handling
- Implement nested abort handling
- Add abort telemetry and debugging

### Task 7.4.3: Multi-Shot Handling
**Description:** Enable handlers to call resumptions multiple times.

**Subtasks:**
- Implement multi-shot resumption support
- Add resumption copy mechanism for multi-shot
- Define multi-shot semantics (state duplication, sharing)
- Add multi-shot resource management
- Implement multi-shot validation and warnings

---

## Section 7.5: Integration Tests

**Description:** Comprehensive integration tests for the handler/resumption core model.

### Task 7.5.1: Basic Handler/Resumption Tests
**Description:** Test basic handler invocation with resumptions.

**Subtasks:**
- Test handler receives operation value and resumption
- Test resume returns to computation
- Test abort terminates computation
- Test handler return values propagate correctly
- Test multiple operations in sequence

### Task 7.5.2: Nested Handler Tests
**Description:** Test nested handler scopes and handler stack behavior.

**Subtasks:**
- Test inner handler handles operation
- Test outer handler handles when inner doesn't
- Test handler stack push/pop behavior
- Test handler shadowing and precedence
- Test handler restoration after scope exit

### Task 7.5.3: Multi-Shot Handler Tests
**Description:** Test handlers that invoke resumptions multiple times.

**Subtasks:**
- Test handler calls resumption twice
- Test resumption state across multiple invocations
- Test multi-shot with state mutations
- Test multi-shot resource handling
- Test multi-shot error handling

---

## Deliverables

### New Modules
- `src/compiler/effects/catena_resumption.erl` - Resumption type and capture
- `src/compiler/effects/catena_handler.erl` - Handler type and registry
- `src/compiler/effects/catena_handler_stack.erl` - Handler stack management

### Modified Modules
- `src/compiler/effects/catena_effects.erl` - Updated perform with resumption
- `src/compiler/effects/catena_effect_advanced.erl` - Integration with new model

### Test Modules
- `test/compiler/effects/catena_resumption_tests.erl`
- `test/compiler/effects/catena_handler_tests.erl`
- `test/compiler/effects/catena_handler_stack_tests.erl`
- `test/compiler/effects/catena_handler_resumption_integration_tests.erl`
