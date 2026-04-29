# Phase 9: Deep vs Shallow Handlers

**Description:** This phase implements both deep and shallow handler semantics, enabling precise control over which operations are handled at which scope levels.

---

## Section 9.1: Handler Depth Semantics

**Description:** Define the semantics and types for deep vs shallow handlers.

### Task 9.1.1: Handler Depth Type Definition
**Description:** Define handler depth types in `src/compiler/effects/catena_handler_depth.erl`.

**Subtasks:**
- Create `catena_handler_depth.erl` module
- Define `-type handler_depth() :: deep | shallow.`
- Define depth semantics documentation
- Export depth constructor functions
- Add depth validation and conversion

### Task 9.1.2: Deep Handler Semantics
**Description:** Specify and document deep handler behavior.

**Subtasks:**
- Define deep handler operational semantics
- Document deep handler scope rules
- Specify deep handler performance implications
- Add deep handler use case documentation
- Implement deep handler examples

### Task 9.1.3: Shallow Handler Semantics
**Description:** Specify and document shallow handler behavior.

**Subtasks:**
- Define shallow handler operational semantics
- Document shallow handler scope rules
- Specify shallow handler performance implications
- Add shallow handler use case documentation
- Implement shallow handler examples

---

## Section 9.2: Shallow Handler Implementation

**Description:** Implement shallow handlers that only handle operations at their direct scope level.

### Task 9.2.1: Shallow Handler Execution
**Description:** Implement shallow handler execution logic.

**Subtasks:**
- Implement `scope_effects_shallow/2` function
- Add shallow handler lookup (current scope only)
- Implement shallow handler invocation
- Add shallow handler error handling
- Implement shallow handler telemetry

### Task 9.2.2: Shallow Handler Scoping
**Description:** Implement proper scoping for shallow handlers.

**Subtasks:**
- Implement shallow handler scope boundaries
- Add shallow handler nesting behavior
- Implement shallow handler shadowing
- Add shallow handler scope exit cleanup
- Implement shallow handler context preservation

### Task 9.2.3: Shallow Handler Composition
**Description:** Implement composition of multiple shallow handlers.

**Subtasks:**
- Implement shallow handler combination
- Add shallow handler precedence rules
- Implement shallow handler conflict resolution
- Add shallow handler composition optimization
- Implement shallow handler composition debugging

---

## Section 9.3: Deep Handler Implementation

**Description:** Implement deep handlers that handle operations at all nested levels.

### Task 9.3.1: Deep Handler Execution
**Description:** Implement deep handler execution logic.

**Subtasks:**
- Implement `scope_effects_deep/2` function
- Add deep handler lookup (all nested scopes)
- Implement deep handler invocation
- Add deep handler error handling
- Implement deep handler telemetry

### Task 9.3.2: Deep Handler Scoping
**Description:** Implement proper scoping for deep handlers.

**Subtasks:**
- Implement deep handler scope boundaries
- Add deep handler nesting behavior
- Implement deep handler shadowing
- Add deep handler scope exit cleanup
- Implement deep handler context preservation

### Task 9.3.3: Deep Handler Traversal
**Description:** Implement efficient traversal for deep handler lookup.

**Subtasks:**
- Implement deep handler stack traversal
- Add deep handler caching
- Implement deep handler traversal optimization
- Add deep handler traversal debugging
- Implement deep handler traversal instrumentation

---

## Section 9.4: Handler Depth Selection

**Description:** Provide mechanisms for selecting handler depth and converting between depths.

### Task 9.4.1: Depth Selection API
**Description:** Provide API for selecting handler depth.

**Subtasks:**
- Implement `with_deep_handler/3` function
- Implement `with_shallow_handler/3` function
- Add depth configuration in effect scopes
- Implement depth parameter passing
- Add depth selection helpers

### Task 9.4.2: Depth Conversion
**Description:** Implement conversion between deep and shallow handlers.

**Subtasks:**
- Implement `to_deep/1` (shallow -> deep)
- Implement `to_shallow/1` (deep -> shallow)
- Add conversion validation
- Implement conversion warnings
- Add conversion optimization

### Task 9.4.3: Mixed Depth Handlers
**Description:** Support mixing deep and shallow handlers in the same scope.

**Subtasks:**
- Implement mixed depth handler scoping
- Add mixed depth precedence rules
- Implement mixed depth conflict resolution
- Add mixed depth composition optimization
- Implement mixed depth debugging

---

## Section 9.5: Integration Tests

**Description:** Integration tests for deep vs shallow handlers.

### Task 9.5.1: Shallow Handler Tests
**Description:** Test shallow handler behavior.

**Subtasks:**
- Test shallow handler handles only direct operations
- Test shallow handler doesn't handle nested operations
- Test shallow handler nesting
- Test shallow handler scoping
- Test shallow handler composition

### Task 9.5.2: Deep Handler Tests
**Description:** Test deep handler behavior.

**Subtasks:**
- Test deep handler handles nested operations
- Test deep handler scope boundaries
- Test deep handler nesting
- Test deep handler scoping
- Test deep handler composition

### Task 9.5.3: Mixed Depth Tests
**Description:** Test mixing deep and shallow handlers.

**Subtasks:**
- Test shallow inside deep
- Test deep inside shallow
- Test mixed depth precedence
- Test mixed depth conversion
- Test mixed depth optimization

---

## Deliverables

### New Modules
- `src/compiler/effects/catena_handler_depth.erl` - Handler depth types
- `src/compiler/effects/catena_shallow_handler.erl` - Shallow handler implementation
- `src/compiler/effects/catena_deep_handler.erl` - Deep handler implementation

### Modified Modules
- `src/compiler/effects/catena_effect_advanced.erl` - Depth selection API
- `src/compiler/effects/catena_handler.erl` - Depth-aware handlers

### Test Modules
- `test/compiler/effects/catena_handler_depth_tests.erl`
- `test/compiler/effects/catena_shallow_handler_tests.erl`
- `test/compiler/effects/catena_deep_handler_tests.erl`
- `test/compiler/effects/catena_handler_depth_integration_tests.erl`
