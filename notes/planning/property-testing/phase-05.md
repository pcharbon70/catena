# Phase 5: Stateful Testing

## Overview

This phase implements stateful property testing for systems with mutable state. Unlike stateless properties that test pure functions, stateful testing generates sequences of commands that modify state and verifies invariants hold throughout. We build a state machine DSL for specifying systems, command generation with preconditions, symbolic and concrete execution phases, parallel execution for concurrency testing, and linearizability checking.

By the end of this phase, developers can test stateful systems like GenServers, databases, and caches using a declarative specification. The framework generates random command sequences, executes them against the real system, and verifies that the system's behavior matches a reference model.

**Design Philosophy**: Stateful testing should feel like specifying an OTP behavior. Commands map to API calls, preconditions specify when commands are valid, postconditions verify correct behavior, and the state model tracks expected state. The two-phase approach (symbolic then concrete) enables shrinking of command sequences.

This phase runs for **4 weeks** and focuses on enabling testing of real-world OTP applications.

---

## 5.1 State Machine DSL
- [ ] **Section 5.1 Complete**

The state machine DSL provides a declarative way to specify stateful systems. A specification includes initial state, commands with generators and conditions, state transitions, and invariants. The DSL is designed to be natural for developers familiar with OTP patterns.

### 5.1.1 State Machine Type Definition
- [ ] **Task 5.1.1 Complete**

Define the StateMachine type that represents a complete system specification.

- [ ] 5.1.1.1 Define `StateMachine` record with `name`, `initial_state`, `commands`, and `invariants`
- [ ] 5.1.1.2 Define `Command` record with `name`, `args_gen`, `precondition`, `execute`, `postcondition`, `next_state`
- [ ] 5.1.1.3 Implement `state_machine/2` macro for defining state machines
- [ ] 5.1.1.4 Implement validation of state machine specifications

### 5.1.2 Command Specification
- [ ] **Task 5.1.2 Complete**

Implement the command specification syntax for defining individual operations.

- [ ] 5.1.2.1 Implement `command/1` macro for defining a command
- [ ] 5.1.2.2 Implement `args/1` for specifying argument generators
- [ ] 5.1.2.3 Implement `requires/2` for preconditions (symbolic state -> bool)
- [ ] 5.1.2.4 Implement `execute/2` for running command against real system
- [ ] 5.1.2.5 Implement `ensures/3` for postconditions (state, args, result -> bool)
- [ ] 5.1.2.6 Implement `updates/3` for state transitions (state, args, result -> new_state)

### 5.1.3 State Model
- [ ] **Task 5.1.3 Complete**

Implement the state model that tracks expected system state during testing.

- [ ] 5.1.3.1 Define state model as arbitrary Erlang term
- [ ] 5.1.3.2 Implement `initial_state/0` callback for defining starting state
- [ ] 5.1.3.3 Support both record and map state representations
- [ ] 5.1.3.4 Implement state accessors and updaters

### 5.1.4 Invariants
- [ ] **Task 5.1.4 Complete**

Implement invariants that must hold at all times during execution.

- [ ] 5.1.4.1 Implement `invariant/2` for defining named invariants
- [ ] 5.1.4.2 Check invariants after each command execution
- [ ] 5.1.4.3 Report which invariant failed with state context
- [ ] 5.1.4.4 Support conditional invariants (only checked in certain states)

### Unit Tests - Section 5.1
- [ ] **Unit Tests 5.1 Complete**
- [ ] Test state machine definition with various command types
- [ ] Test command preconditions filter invalid commands
- [ ] Test state transitions update model correctly
- [ ] Test invariants are checked after each command
- [ ] Test invariant violation is detected and reported

---

## 5.2 Command Generation
- [ ] **Section 5.2 Complete**

Command generation creates random sequences of valid commands. Commands are filtered by preconditions based on current state. Shrinking of command sequences removes commands while maintaining validity.

### 5.2.1 Command Sequence Generation
- [ ] **Task 5.2.1 Complete**

Generate sequences of commands respecting preconditions.

- [ ] 5.2.1.1 Implement `gen_commands/2` generating command sequences from state machine
- [ ] 5.2.1.2 Filter commands by precondition at each step
- [ ] 5.2.1.3 Control sequence length with size parameter
- [ ] 5.2.1.4 Handle cases where no commands are valid (empty sequence)

### 5.2.2 Weighted Command Selection
- [ ] **Task 5.2.2 Complete**

Support weighted selection of commands to control command distribution.

- [ ] 5.2.2.1 Implement `weight/1` option for commands
- [ ] 5.2.2.2 Implement `frequency/1` for dynamic weighting based on state
- [ ] 5.2.2.3 Document weight tuning for effective testing
- [ ] 5.2.2.4 Report command distribution in test output

### 5.2.3 Command Sequence Shrinking
- [ ] **Task 5.2.3 Complete**

Shrink command sequences while maintaining validity (all preconditions still satisfied).

- [ ] 5.2.3.1 Implement sequence shrinking by removing commands
- [ ] 5.2.3.2 Re-validate preconditions after removal
- [ ] 5.2.3.3 Shrink individual command arguments
- [ ] 5.2.3.4 Prioritize removing later commands (preserve setup)

### 5.2.4 Symbolic Values
- [ ] **Task 5.2.4 Complete**

Support symbolic values that represent command results before execution.

- [ ] 5.2.4.1 Implement `Var` type for symbolic command results
- [ ] 5.2.4.2 Track variable bindings during symbolic execution
- [ ] 5.2.4.3 Resolve variables during concrete execution
- [ ] 5.2.4.4 Enable commands to depend on results of earlier commands

### Unit Tests - Section 5.2
- [ ] **Unit Tests 5.2 Complete**
- [ ] Test command sequence generation produces valid sequences
- [ ] Test preconditions filter invalid commands
- [ ] Test weighted selection respects weights
- [ ] Test shrinking preserves precondition validity
- [ ] Test symbolic values resolve correctly

---

## 5.3 Symbolic Execution
- [ ] **Section 5.3 Complete**

Symbolic execution runs commands against the state model without touching the real system. This phase validates preconditions, generates symbolic results, and updates model state. Shrinking operates on symbolic commands before concrete execution.

### 5.3.1 Symbolic Command Execution
- [ ] **Task 5.3.1 Complete**

Execute commands symbolically against the state model.

- [ ] 5.3.1.1 Implement `run_symbolic/2` executing command sequence symbolically
- [ ] 5.3.1.2 Generate fresh `Var` for each command result
- [ ] 5.3.1.3 Update model state using `next_state` callback
- [ ] 5.3.1.4 Collect symbolic trace for later concrete execution

### 5.3.2 Precondition Checking
- [ ] **Task 5.3.2 Complete**

Validate preconditions during symbolic execution.

- [ ] 5.3.2.1 Check precondition before each symbolic command
- [ ] 5.3.2.2 Fail generation if precondition fails (should not happen with proper generation)
- [ ] 5.3.2.3 Report precondition failure with state context
- [ ] 5.3.2.4 Support symbolic preconditions referencing variables

### 5.3.3 Symbolic Result Tracking
- [ ] **Task 5.3.3 Complete**

Track symbolic results and variable bindings.

- [ ] 5.3.3.1 Maintain variable environment mapping `Var` to position in trace
- [ ] 5.3.3.2 Support variable references in subsequent command arguments
- [ ] 5.3.3.3 Detect undefined variable references
- [ ] 5.3.3.4 Pretty-print symbolic traces for debugging

### Unit Tests - Section 5.3
- [ ] **Unit Tests 5.3 Complete**
- [ ] Test symbolic execution produces correct trace
- [ ] Test state model updates correctly
- [ ] Test variable environment tracks bindings
- [ ] Test symbolic trace pretty-printing
- [ ] Test precondition failures are detected

---

## 5.4 Concrete Execution
- [ ] **Section 5.4 Complete**

Concrete execution runs the command sequence against the real system, resolving symbolic values to actual results. Postconditions verify the real system matches expected behavior.

### 5.4.1 System Under Test Setup
- [ ] **Task 5.4.1 Complete**

Set up the real system for testing.

- [ ] 5.4.1.1 Implement `setup/0` callback for system initialization
- [ ] 5.4.1.2 Implement `cleanup/1` callback for system teardown
- [ ] 5.4.1.3 Support process-based systems (spawn, stop)
- [ ] 5.4.1.4 Handle setup failures gracefully

### 5.4.2 Concrete Command Execution
- [ ] **Task 5.4.2 Complete**

Execute commands against the real system.

- [ ] 5.4.2.1 Implement `run_concrete/2` executing command sequence
- [ ] 5.4.2.2 Resolve `Var` references to actual values
- [ ] 5.4.2.3 Call `execute` callback with resolved arguments
- [ ] 5.4.2.4 Capture command results for postcondition checking

### 5.4.3 Postcondition Verification
- [ ] **Task 5.4.3 Complete**

Verify postconditions after each concrete command.

- [ ] 5.4.3.1 Call `postcondition` callback with state, args, and result
- [ ] 5.4.3.2 Fail test immediately on postcondition violation
- [ ] 5.4.3.3 Report which command and postcondition failed
- [ ] 5.4.3.4 Include state and result in failure message

### 5.4.4 Result Comparison
- [ ] **Task 5.4.4 Complete**

Compare concrete results with expected values.

- [ ] 5.4.4.1 Implement `expect/2` for asserting expected results
- [ ] 5.4.4.2 Support pattern matching in expectations
- [ ] 5.4.4.3 Support approximate matching for non-deterministic results
- [ ] 5.4.4.4 Report mismatches with detailed diff

### Unit Tests - Section 5.4
- [ ] **Unit Tests 5.4 Complete**
- [ ] Test concrete execution runs commands correctly
- [ ] Test variable resolution works
- [ ] Test postcondition violations are detected
- [ ] Test setup and cleanup run correctly
- [ ] Test result comparison with various types

---

## 5.5 Parallel Execution Support
- [ ] **Section 5.5 Complete**

Parallel execution tests concurrent behavior by running multiple command sequences simultaneously. This reveals race conditions and other concurrency bugs. Linearizability checking verifies that concurrent execution could have occurred sequentially.

### 5.5.1 Parallel Command Generation
- [ ] **Task 5.5.1 Complete**

Generate parallel command groups for concurrent execution.

- [ ] 5.5.1.1 Implement `gen_parallel_commands/2` generating parallel groups
- [ ] 5.5.1.2 Generate initial sequential prefix followed by parallel groups
- [ ] 5.5.1.3 Configure number of parallel tracks (default 2-4)
- [ ] 5.5.1.4 Ensure preconditions valid for all parallel orderings

### 5.5.2 Parallel Execution
- [ ] **Task 5.5.2 Complete**

Execute command groups in parallel.

- [ ] 5.5.2.1 Spawn processes for each parallel track
- [ ] 5.5.2.2 Synchronize start of parallel execution
- [ ] 5.5.2.3 Collect results from all tracks
- [ ] 5.5.2.4 Handle timeouts and crashes

### 5.5.3 Linearizability Checking
- [ ] **Task 5.5.3 Complete**

Verify that parallel results could have occurred in some sequential order.

- [ ] 5.5.3.1 Implement linearization search algorithm
- [ ] 5.5.3.2 Generate all possible interleavings
- [ ] 5.5.3.3 Check if any interleaving matches observed results
- [ ] 5.5.3.4 Report non-linearizable execution with witness

### 5.5.4 Race Condition Detection
- [ ] **Task 5.5.4 Complete**

Detect and report potential race conditions.

- [ ] 5.5.4.1 Track concurrent access to shared state
- [ ] 5.5.4.2 Report commands that may race
- [ ] 5.5.4.3 Suggest potential fixes for races
- [ ] 5.5.4.4 Support optional race condition annotations

### Unit Tests - Section 5.5
- [ ] **Unit Tests 5.5 Complete**
- [ ] Test parallel command generation produces valid parallel groups
- [ ] Test parallel execution runs commands concurrently
- [ ] Test linearizability checker finds valid orderings
- [ ] Test non-linearizable executions are detected
- [ ] Test race condition detection works

---

## 5.6 Integration Tests - Phase 5
- [ ] **Integration Tests 5.6 Complete**

Integration tests verify complete stateful testing workflows with realistic systems.

- [ ] Test complete workflow with simple counter GenServer
- [ ] Test queue data structure with enqueue/dequeue commands
- [ ] Test key-value store with put/get/delete commands
- [ ] Test state machine with complex state transitions
- [ ] Test parallel execution finds known race condition
- [ ] Test linearizability checking with concurrent counter
- [ ] Test shrinking produces minimal failing command sequence
- [ ] Test symbolic values work across commands
- [ ] Test invariant violations are detected
- [ ] Test performance: 1000 command sequences in reasonable time

---

## Success Criteria

1. **State Machine DSL**: Declarative specification of stateful systems
2. **Command Generation**: Valid command sequences respecting preconditions
3. **Symbolic Execution**: Two-phase approach enabling shrinking
4. **Concrete Execution**: Correct execution with postcondition verification
5. **Parallel Testing**: Concurrent execution with linearizability checking
6. **Shrinking**: Minimal failing command sequences
7. **Integration**: Works with OTP behaviors and common patterns
8. **Documentation**: Clear examples for GenServer testing

---

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 6**: BEAM integration with process and actor testing
- **Phase 7**: Advanced state machine patterns and optimizations
