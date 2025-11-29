# Phase 6: BEAM Integration

## Overview

This phase implements BEAM-specific testing features that leverage the unique capabilities of the Erlang VM. We build process testing support for testing actors in isolation, message passing properties for verifying communication protocols, concurrency testing utilities for detecting race conditions, distribution testing for multi-node scenarios, and OTP behavior testing patterns for testing GenServers, Supervisors, and other OTP components.

By the end of this phase, developers can comprehensively test BEAM applications including concurrent processes, message protocols, distributed systems, and OTP behaviors. The testing infrastructure understands BEAM semantics and provides appropriate generators, assertions, and debugging tools.

**Design Philosophy**: BEAM testing should feel native to BEAM developers. Process spawning, message passing, and OTP patterns are first-class concepts. The framework handles the complexity of concurrent testing (ordering, timeouts, cleanup) so developers can focus on specifying expected behavior.

This phase runs for **4 weeks** and focuses on making property testing practical for real BEAM applications.

---

## 6.1 Process Testing Support
- [ ] **Section 6.1 Complete**

Process testing provides tools for spawning test processes, controlling their execution, and verifying their behavior. Processes can be tested in isolation with controlled environments and deterministic scheduling options.

### 6.1.1 Test Process Management
- [ ] **Task 6.1.1 Complete**

Implement process spawning and lifecycle management for testing.

- [ ] 6.1.1.1 Implement `spawn_test_process/1` spawning a process linked to test runner
- [ ] 6.1.1.2 Implement `with_process/2` for scoped process testing with automatic cleanup
- [ ] 6.1.1.3 Implement `stop_process/1` for graceful process termination
- [ ] 6.1.1.4 Implement `kill_process/1` for forced termination
- [ ] 6.1.1.5 Track all spawned processes for cleanup after test

### 6.1.2 Process State Inspection
- [ ] **Task 6.1.2 Complete**

Implement tools for inspecting process state during testing.

- [ ] 6.1.2.1 Implement `get_state/1` using sys:get_state for GenServer-like processes
- [ ] 6.1.2.2 Implement `process_info_safe/2` wrapper with timeout handling
- [ ] 6.1.2.3 Implement `message_queue/1` returning current message queue
- [ ] 6.1.2.4 Implement `process_memory/1` for memory usage tracking

### 6.1.3 Process Generators
- [ ] **Task 6.1.3 Complete**

Implement generators for process-related values.

- [ ] 6.1.3.1 Implement `gen_pid/0` generating pids of existing processes
- [ ] 6.1.3.2 Implement `gen_ref/0` generating unique references
- [ ] 6.1.3.3 Implement `gen_node/0` generating node names
- [ ] 6.1.3.4 Implement `gen_registered_name/0` generating registered process names

### 6.1.4 Process Assertions
- [ ] **Task 6.1.4 Complete**

Implement assertions for verifying process behavior.

- [ ] 6.1.4.1 Implement `assert_alive/1` verifying process is alive
- [ ] 6.1.4.2 Implement `assert_dead/1` verifying process has terminated
- [ ] 6.1.4.3 Implement `assert_exit_reason/2` verifying termination reason
- [ ] 6.1.4.4 Implement `assert_no_messages/1` verifying empty message queue

### Unit Tests - Section 6.1
- [ ] **Unit Tests 6.1 Complete**
- [ ] Test process spawning and cleanup
- [ ] Test state inspection works for GenServer
- [ ] Test process generators produce valid values
- [ ] Test process assertions detect violations
- [ ] Test cleanup handles crashed processes

---

## 6.2 Message Passing Properties
- [ ] **Section 6.2 Complete**

Message passing properties test communication between processes. We verify that messages are sent correctly, received in expected order, and that protocols are followed. This includes support for selective receive patterns and timeout handling.

### 6.2.1 Message Generators
- [ ] **Task 6.2.1 Complete**

Implement generators for messages and message patterns.

- [ ] 6.2.1.1 Implement `gen_message/1` generating messages of a specific type
- [ ] 6.2.1.2 Implement `gen_message_sequence/2` generating ordered message lists
- [ ] 6.2.1.3 Implement `gen_tagged_message/2` generating {tag, payload} messages
- [ ] 6.2.1.4 Implement `gen_call_message/1` generating gen_server call messages

### 6.2.2 Message Sending Properties
- [ ] **Task 6.2.2 Complete**

Test properties about message sending behavior.

- [ ] 6.2.2.1 Implement `sends_message/3` property verifying message is sent
- [ ] 6.2.2.2 Implement `sends_messages_in_order/3` for ordered sends
- [ ] 6.2.2.3 Implement `no_message_loss/3` for reliable delivery
- [ ] 6.2.2.4 Track sent messages with tracing for verification

### 6.2.3 Message Receiving Properties
- [ ] **Task 6.2.3 Complete**

Test properties about message receiving behavior.

- [ ] 6.2.3.1 Implement `receives_message/2` with timeout
- [ ] 6.2.3.2 Implement `receives_message_matching/2` with pattern matching
- [ ] 6.2.3.3 Implement `receives_all_messages/2` for bulk receive
- [ ] 6.2.3.4 Implement `flush_messages/0` for test cleanup

### 6.2.4 Protocol Verification
- [ ] **Task 6.2.4 Complete**

Verify that message protocols are followed correctly.

- [ ] 6.2.4.1 Define protocol as sequence of expected message patterns
- [ ] 6.2.4.2 Implement `follows_protocol/3` verifying protocol adherence
- [ ] 6.2.4.3 Report protocol violations with message trace
- [ ] 6.2.4.4 Support optional and repeated messages in protocols

### Unit Tests - Section 6.2
- [ ] **Unit Tests 6.2 Complete**
- [ ] Test message generators produce valid messages
- [ ] Test message sending properties detect failures
- [ ] Test message receiving with timeouts
- [ ] Test protocol verification catches violations
- [ ] Test message ordering verification

---

## 6.3 Concurrency Testing Utilities
- [ ] **Section 6.3 Complete**

Concurrency testing utilities help detect race conditions, deadlocks, and other concurrent bugs. We provide deterministic scheduling, interleaving exploration, and concurrent invariant checking.

### 6.3.1 Deterministic Scheduling
- [ ] **Task 6.3.1 Complete**

Implement controlled scheduling for reproducible concurrent tests.

- [ ] 6.3.1.1 Implement scheduler wrapper for controlled process scheduling
- [ ] 6.3.1.2 Implement schedule specification language
- [ ] 6.3.1.3 Implement replay of specific schedules
- [ ] 6.3.1.4 Record schedules for debugging failures

### 6.3.2 Interleaving Exploration
- [ ] **Task 6.3.2 Complete**

Explore different execution interleavings to find bugs.

- [ ] 6.3.2.1 Implement random interleaving selection
- [ ] 6.3.2.2 Implement systematic interleaving enumeration (bounded)
- [ ] 6.3.2.3 Implement priority-based interleaving (likely to find bugs)
- [ ] 6.3.2.4 Report interleaving that caused failure

### 6.3.3 Race Condition Detection
- [ ] **Task 6.3.3 Complete**

Detect potential race conditions in concurrent code.

- [ ] 6.3.3.1 Implement happens-before tracking for operations
- [ ] 6.3.3.2 Detect concurrent access to shared state
- [ ] 6.3.3.3 Report potential races with access trace
- [ ] 6.3.3.4 Distinguish benign races from harmful ones

### 6.3.4 Deadlock Detection
- [ ] **Task 6.3.4 Complete**

Detect potential deadlocks in concurrent systems.

- [ ] 6.3.4.1 Track lock acquisition and release
- [ ] 6.3.4.2 Detect wait cycles indicating deadlock
- [ ] 6.3.4.3 Report deadlock with lock acquisition trace
- [ ] 6.3.4.4 Support timeout-based deadlock detection

### Unit Tests - Section 6.3
- [ ] **Unit Tests 6.3 Complete**
- [ ] Test deterministic scheduling produces same results
- [ ] Test interleaving exploration finds known bug
- [ ] Test race detection finds concurrent access
- [ ] Test deadlock detection finds lock cycles
- [ ] Test replay of recorded schedules

---

## 6.4 Distribution Testing
- [ ] **Section 6.4 Complete**

Distribution testing verifies behavior across multiple Erlang nodes. We support spawning test nodes, testing remote communication, and verifying consistency across nodes.

### 6.4.1 Test Node Management
- [ ] **Task 6.4.1 Complete**

Manage test nodes for distributed testing.

- [ ] 6.4.1.1 Implement `spawn_test_node/1` starting a new node
- [ ] 6.4.1.2 Implement `connect_nodes/2` connecting test nodes
- [ ] 6.4.1.3 Implement `with_nodes/2` for scoped multi-node testing
- [ ] 6.4.1.4 Implement automatic node cleanup after tests

### 6.4.2 Remote Process Testing
- [ ] **Task 6.4.2 Complete**

Test processes running on remote nodes.

- [ ] 6.4.2.1 Implement `spawn_on_node/2` spawning process on specific node
- [ ] 6.4.2.2 Implement `rpc_test/3` for testing remote procedure calls
- [ ] 6.4.2.3 Implement `global_process/1` for testing globally registered processes
- [ ] 6.4.2.4 Handle node failures gracefully

### 6.4.3 Network Partition Simulation
- [ ] **Task 6.4.3 Complete**

Simulate network partitions for resilience testing.

- [ ] 6.4.3.1 Implement `partition_nodes/2` simulating network split
- [ ] 6.4.3.2 Implement `heal_partition/1` restoring connectivity
- [ ] 6.4.3.3 Implement `delay_messages/3` for network latency simulation
- [ ] 6.4.3.4 Test behavior during and after partition healing

### 6.4.4 Consistency Verification
- [ ] **Task 6.4.4 Complete**

Verify data consistency across distributed nodes.

- [ ] 6.4.4.1 Implement `eventually_consistent/3` with timeout
- [ ] 6.4.4.2 Implement `strongly_consistent/2` for immediate consistency
- [ ] 6.4.4.3 Implement consistency checkers for common patterns (CRDTs, etc.)
- [ ] 6.4.4.4 Report consistency violations with node states

### Unit Tests - Section 6.4
- [ ] **Unit Tests 6.4 Complete**
- [ ] Test node spawning and cleanup
- [ ] Test remote process spawning
- [ ] Test partition simulation
- [ ] Test consistency verification
- [ ] Test partition healing behavior

---

## 6.5 OTP Behavior Patterns
- [ ] **Section 6.5 Complete**

OTP behavior patterns provide specialized testing support for common OTP behaviors: GenServer, Supervisor, gen_statem, and others. These patterns understand OTP semantics and provide appropriate generators and assertions.

### 6.5.1 GenServer Testing
- [ ] **Task 6.5.1 Complete**

Specialized testing for GenServer behaviors.

- [ ] 6.5.1.1 Implement `genserver_property/2` for testing GenServer properties
- [ ] 6.5.1.2 Implement `gen_call/1` generating call requests
- [ ] 6.5.1.3 Implement `gen_cast/1` generating cast requests
- [ ] 6.5.1.4 Implement assertions for GenServer state transitions
- [ ] 6.5.1.5 Test timeout handling and info messages

### 6.5.2 Supervisor Testing
- [ ] **Task 6.5.2 Complete**

Specialized testing for Supervisor behaviors.

- [ ] 6.5.2.1 Implement `supervisor_property/2` for testing Supervisor properties
- [ ] 6.5.2.2 Implement child crash simulation
- [ ] 6.5.2.3 Verify restart strategies (one_for_one, one_for_all, etc.)
- [ ] 6.5.2.4 Test supervisor shutdown ordering
- [ ] 6.5.2.5 Verify intensity and period limits

### 6.5.3 gen_statem Testing
- [ ] **Task 6.5.3 Complete**

Specialized testing for gen_statem behaviors.

- [ ] 6.5.3.1 Implement `statem_property/2` for testing state machines
- [ ] 6.5.3.2 Generate events for state transitions
- [ ] 6.5.3.3 Verify state transition validity
- [ ] 6.5.3.4 Test state enter callbacks
- [ ] 6.5.3.5 Test timeout and postpone behaviors

### 6.5.4 Custom Behavior Testing
- [ ] **Task 6.5.4 Complete**

Support for testing custom OTP behaviors.

- [ ] 6.5.4.1 Implement behavior callback testing framework
- [ ] 6.5.4.2 Generate callbacks with appropriate arguments
- [ ] 6.5.4.3 Verify callback return values
- [ ] 6.5.4.4 Document patterns for custom behavior testing

### Unit Tests - Section 6.5
- [ ] **Unit Tests 6.5 Complete**
- [ ] Test GenServer call/cast/info handling
- [ ] Test Supervisor restart strategies
- [ ] Test gen_statem state transitions
- [ ] Test custom behavior framework
- [ ] Test OTP-specific assertions

---

## 6.6 Integration Tests - Phase 6
- [ ] **Integration Tests 6.6 Complete**

Integration tests verify BEAM integration features work correctly in realistic scenarios.

- [ ] Test complete GenServer testing workflow
- [ ] Test Supervisor with crashing children
- [ ] Test message protocol between multiple processes
- [ ] Test concurrent access to shared GenServer
- [ ] Test distributed counter across nodes
- [ ] Test network partition handling
- [ ] Test race condition detection with known bug
- [ ] Test deterministic replay of concurrent failure
- [ ] Test cleanup handles all scenarios (normal, crash, timeout)
- [ ] Test performance: concurrent tests complete in reasonable time

---

## Success Criteria

1. **Process Testing**: Comprehensive tools for testing individual processes
2. **Message Passing**: Property verification for message protocols
3. **Concurrency Utilities**: Race and deadlock detection
4. **Distribution Testing**: Multi-node testing with partition simulation
5. **OTP Patterns**: Specialized support for common OTP behaviors
6. **Reliability**: Proper cleanup and error handling
7. **Performance**: Tests complete in practical time
8. **Documentation**: Clear patterns for BEAM testing

---

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 7**: Advanced features building on BEAM testing foundation
- **Future Phases**: Actor model and effect system testing
