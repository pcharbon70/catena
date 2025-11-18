# Phase 5: Actor Model Integration

## Overview

This phase integrates Catena with BEAM's actor model, bringing together functional programming and concurrent processes. **This phase unifies actors with the algebraic effect system**—actors are revealed as handlers for the Process effect introduced in Phase 2, demonstrating how effects elegantly capture stateful concurrent computation. Actors provide isolated state, message-based communication, and fault tolerance—the foundation of Erlang's legendary reliability. We design actor syntax that makes category theory concepts explicit while compiling to standard OTP behaviors. Actors maintain immutable state between messages, update state functionally, and handle failures gracefully through supervision trees.

The actor system treats processes as objects in a category where morphisms are message handlers. **Actor message handlers ARE effect handlers** for the Process effect, with state transitions becoming effectful computations. State transitions are pure functions from (Message, State) to (NewState, Reply), but spawn/send/receive operations are effects handled by the BEAM runtime. This functional approach to actors preserves referential transparency while leveraging BEAM's process isolation and fault recovery through the effect system. We implement both low-level process primitives (as effect operations) and high-level supervision abstractions, enabling everything from simple stateful servers to complex distributed systems.

This phase runs for **3.5 weeks** and completes the proof-of-concept by demonstrating Catena's unique value proposition: marrying category theory's mathematical rigor with BEAM's practical distributed computing through algebraic effects. By the end, developers can write concurrent, fault-tolerant systems using familiar functional patterns backed by battle-tested BEAM infrastructure, with effects making the boundaries between pure and concurrent computation explicit.

---

## 5.1 Actor Definition Syntax
- [ ] **Section 5.1 Complete**

Actor definitions describe stateful processes using functional syntax. An actor specifies its state type, message protocol, initialization function, and message handler. The handler is a pure function from (Message, State) to (NewState, MaybeReply)—making state transitions explicit and testable. We compile actors to OTP gen_server behaviors, mapping Catena's functional handler to gen_server callbacks. This design makes actors predictable while maintaining BEAM compatibility.

### 5.1.1 State Type System
- [ ] **Task 5.1.1 Complete**

Actor state must be explicitly typed, defining what data the actor maintains across messages. State types are typically records or ADTs containing all persistent information. State is immutable—handlers return new state rather than mutating. We enforce this through the type system, preventing shared mutable state that causes concurrency bugs. State can include other actor references (pids), enabling actor hierarchies and supervision.

- [ ] 5.1.1.1 Implement actor state type declaration with immutability constraints
- [ ] 5.1.1.2 Implement state initialization function specifying starting state
- [ ] 5.1.1.3 Implement state validation ensuring state types are serializable for process migration
- [ ] 5.1.1.4 Implement state introspection allowing debugging and monitoring of actor state

### 5.1.2 Message Protocol
- [ ] **Task 5.1.2 Complete**

Message protocols define what messages an actor accepts, using sum types (variants). Each message constructor represents a different request type. Example: `shape Message = Increment | Decrement | Get | Reset Natural`. This makes the actor's API explicit in types. Pattern matching on messages routes to appropriate handling logic. We generate message type definitions that serialize correctly over BEAM channels.

- [ ] 5.1.2.1 Implement message type declaration as algebraic data types
- [ ] 5.1.2.2 Implement message pattern matching in handlers routing messages to logic
- [ ] 5.1.2.3 Implement message serialization for sending between processes
- [ ] 5.1.2.4 Implement message type validation catching protocol violations at compile time

### 5.1.3 Handler Implementation
- [ ] **Task 5.1.3 Complete**

Message handlers have signature: `handle : Message -> State -> (State, Maybe Reply) / {Process}`. **Actor handlers ARE effect handlers for the Process effect**—they can use process operations (spawn, send, receive) within handler logic. The Process effect tracks stateful concurrent operations while keeping the core state transition logic explicit. Handlers pattern match on messages, compute new state using pure logic and process effects, and optionally produce replies. This approach makes the pure/effectful boundary clear while enabling testability. We compile handlers to gen_server handle_call/handle_cast callbacks that execute with the Process effect runtime.

- [ ] 5.1.3.1 Implement handler function type-and-effect checking ensuring `Message -> State -> (State, Maybe Reply) / {Process}` signature
- [ ] 5.1.3.2 Implement handler pattern matching compiling to efficient message dispatch
- [ ] 5.1.3.3 Implement reply generation handling synchronous and asynchronous messages
- [ ] 5.1.3.4 Implement Process effect integration allowing handlers to spawn, send, and receive during message handling

### 5.1.4 Process Primitives
- [ ] **Task 5.1.4 Complete**

**Process primitives are operations of the Process effect** from Phase 2. `spawn` creates new processes from actor definitions (operation signature: `spawn : Flow -> ProcessId / {Process}`). `send` (written `!`) sends asynchronous messages (`send : ProcessId -> Message -> Unit / {Process}`). `call` (written `?`) sends synchronous messages and waits for replies. `link` and `monitor` establish failure detection relationships. These effect operations map directly to BEAM operations, leveraging the process-based effect runtime while providing type safety through the effect system.

- [ ] 5.1.4.1 Implement `spawn` as Process effect operation creating new actor processes from definitions
- [ ] 5.1.4.2 Implement `send` (!) as Process effect operation for asynchronous message sending with type checking
- [ ] 5.1.4.3 Implement `call` (?) as Process effect operation for synchronous request-reply with timeout handling
- [ ] 5.1.4.4 Implement `link` and `monitor` as Process effect operations for failure detection and supervision

### 5.1.5 Actor-Workflow Integration
- [ ] **Task 5.1.5 Complete**

Demonstrate that actors form a Workflow (Monad) for concurrent computation, proving that Catena's category theory foundation elegantly captures actor-based concurrency. The Process monad enables monadic composition of concurrent operations, making distributed programming compositional and type-safe.

- [ ] 5.1.5.1 Define Process Workflow instance showing processes form a monad: `instance Workflow Process where pure : a -> Process a; bind : Process a -> (a -> Process b) -> Process b`
- [ ] 5.1.5.2 Implement `pure` for Process spawning trivial actor that immediately returns value (wrapping pure computation in process)
- [ ] 5.1.5.3 Implement `bind` for Process enabling sequential actor communication where second actor depends on first's result
- [ ] 5.1.5.4 Show actor message handlers are EffectfulFlows (Kleisli arrows) in Process category: `handle : Message -> State -> (State, Reply) / {Process}`
- [ ] 5.1.5.5 Enable do-notation for actor choreography allowing sequential process composition: `do { x <- spawn Worker1; y <- x ? Process; spawn (Worker2 y) }`
- [ ] 5.1.5.6 Verify Process monad laws (left/right identity, associativity) hold for actor composition via property testing

### Unit Tests - Section 5.1
- [ ] **Unit Tests 5.1 Complete**
- [ ] Test actor state type checking enforcing immutability and serializability
- [ ] Test message protocol definition and pattern matching in handlers
- [ ] Test handler function type-and-effect checking including Process effect tracking
- [ ] Test process primitives (spawn, send, call) as Process effect operations with effect tracking
- [ ] Test actor-effect unification demonstrating actors as Process effect handlers
- [ ] Test Process Workflow instance with monad laws verification
- [ ] Test do-notation for actor composition producing correct concurrent behavior
- [ ] Test actor handlers as EffectfulFlows composing via Kleisli composition

---

## 5.2 Supervision Trees
- [ ] **Section 5.2 Complete**

Supervision trees provide fault tolerance through hierarchical process monitoring. Supervisors watch child processes and restart them on failure according to configurable strategies. We implement OTP supervision patterns using declarative syntax. Supervisors themselves are actors, creating recursive supervision hierarchies. The "let it crash" philosophy separates normal logic from error handling—processes fail fast, supervisors handle recovery.

### 5.2.1 Supervisor Syntax
- [ ] **Task 5.2.1 Complete**

Supervisor definitions specify restart strategy and child specifications. Syntax: `supervisor Name = { strategy: one_for_one, children: [...] }`. Strategies include one_for_one (restart only failed child), one_for_all (restart all children), and rest_for_one (restart failed child and those after it). Max_restarts and max_seconds limit restart frequency preventing crash loops.

- [ ] 5.2.1.1 Implement supervisor declaration syntax with strategy and child list
- [ ] 5.2.1.2 Implement restart strategy types (one_for_one, one_for_all, rest_for_one)
- [ ] 5.2.1.3 Implement restart intensity limits (max_restarts, max_seconds) preventing loops
- [ ] 5.2.1.4 Implement supervisor compilation to OTP supervisor behavior

### 5.2.2 Child Specifications
- [ ] **Task 5.2.2 Complete**

Child specifications describe supervised processes. Each child has an ID, start function, restart policy (permanent, transient, temporary), shutdown timeout, and type (worker or supervisor). We generate child specs from actor definitions automatically, with sensible defaults. Developers can customize child specs for specific requirements like graceful shutdown times.

- [ ] 5.2.2.1 Implement child specification syntax with ID, start function, and restart policy
- [ ] 5.2.2.2 Implement automatic child spec generation from actor definitions
- [ ] 5.2.2.3 Implement restart policy types (permanent, transient, temporary) with correct semantics
- [ ] 5.2.2.4 Implement shutdown timeout configuration for graceful termination

### 5.2.3 Restart Strategies
- [ ] **Task 5.2.3 Complete**

Different restart strategies suit different use cases. One-for-one restarts failed children independently—best for independent workers. One-for-all restarts all children when any fails—useful when children depend on each other. Rest-for-one restarts failed child and subsequent children—for when later children depend on earlier ones. We implement all strategies following OTP semantics exactly.

- [ ] 5.2.3.1 Implement one-for-one strategy restarting only failed children
- [ ] 5.2.3.2 Implement one-for-all strategy restarting all children on any failure
- [ ] 5.2.3.3 Implement rest-for-one strategy restarting failed child and all after it
- [ ] 5.2.3.4 Implement simple-one-for-one strategy for dynamic child pools

### 5.2.4 Supervision Tree Compilation
- [ ] **Task 5.2.4 Complete**

Supervision trees compile to OTP supervisor behaviors with child specs. We generate init callbacks returning supervision strategy and child list. Child start functions spawn actors with correct arguments. The compilation ensures that supervision trees integrate seamlessly with existing OTP applications, allowing Catena supervisors to manage Erlang processes and vice versa.

- [ ] 5.2.4.1 Implement supervisor behavior generation with init callback
- [ ] 5.2.4.2 Implement child start function generation spawning actors correctly
- [ ] 5.2.4.3 Implement supervision tree initialization handling application startup
- [ ] 5.2.4.4 Implement hot code upgrade support for supervised processes

### Unit Tests - Section 5.2
- [ ] **Unit Tests 5.2 Complete**
- [ ] Test supervisor declaration and compilation to OTP supervisors
- [ ] Test restart strategies (one-for-one, one-for-all, rest-for-one) with simulated failures
- [ ] Test child specifications with different restart policies and shutdown timeouts
- [ ] Test supervision tree compilation and integration with OTP application structure

---

## 5.3 Process Communication Patterns
- [ ] **Section 5.3 Complete**

Beyond basic message passing, we implement higher-level communication patterns common in actor systems. Request-reply with timeouts prevents indefinite blocking. Broadcast sends messages to multiple actors. Process registries enable name-based lookup. Selective receive allows mailbox filtering. These patterns build on primitives to provide convenient, type-safe communication abstractions.

### 5.3.1 Synchronous Request-Reply
- [ ] **Task 5.3.1 Complete**

Synchronous calls block until receiving a reply or timeout. Syntax: `result <- actor ? Message`. The `?` operator sends a message and waits for response. Timeouts prevent indefinite blocking—unresponsive actors cause timeout errors rather than deadlocks. We implement call operation using gen_server:call, handling timeouts and failures gracefully.

- [ ] 5.3.1.1 Implement synchronous call operator (?) with type-checked request and reply
- [ ] 5.3.1.2 Implement timeout handling returning timeout errors after specified duration
- [ ] 5.3.1.3 Implement call failure handling converting process exits to Result types
- [ ] 5.3.1.4 Implement call performance optimization reducing overhead for simple calls

### 5.3.2 Asynchronous Fire-and-Forget
- [ ] **Task 5.3.2 Complete**

Asynchronous sends don't wait for replies. Syntax: `actor ! Message`. The `!` operator sends and returns immediately. This enables high throughput—senders don't block waiting for processing. Message order is preserved per sender-receiver pair. We implement send using gen_server:cast or direct message passing depending on whether replies are possible.

- [ ] 5.3.2.1 Implement asynchronous send operator (!) with message type checking
- [ ] 5.3.2.2 Implement send performance optimization avoiding unnecessary overhead
- [ ] 5.3.2.3 Implement message ordering guarantees preserving order per sender-receiver pair
- [ ] 5.3.2.4 Implement send failure handling detecting dead processes and full mailboxes

### 5.3.3 Process Registration
- [ ] **Task 5.3.3 Complete**

Process registration maps names to process IDs, enabling location-independent messaging. Global registration makes processes findable system-wide. Local registration scopes names per node. We integrate with BEAM's process registry (erlang:register) while adding type safety. Named processes receive messages at registered names rather than PIDs.

- [ ] 5.3.3.1 Implement process registration with unique names checking for conflicts
- [ ] 5.3.3.2 Implement name lookup resolving registered names to process IDs
- [ ] 5.3.3.3 Implement global registration for distributed process lookup across nodes
- [ ] 5.3.3.4 Implement unregistration and automatic cleanup on process termination

### 5.3.4 Selective Receive
- [ ] **Task 5.3.4 Complete**

Selective receive filters mailbox contents, receiving only messages matching patterns. This allows prioritizing urgent messages or waiting for specific replies while deferring others. We compile selective receive to BEAM's optimized receive expressions with patterns. Timeout support prevents indefinite waiting. This pattern enables complex protocols without manual mailbox management.

- [ ] 5.3.4.1 Implement selective receive syntax with pattern matching and timeout
- [ ] 5.3.4.2 Implement selective receive compilation to BEAM receive expressions
- [ ] 5.3.4.3 Implement timeout handling returning timeout results or default values
- [ ] 5.3.4.4 Implement selective receive optimization leveraging BEAM's receive optimization

### Unit Tests - Section 5.3
- [ ] **Unit Tests 5.3 Complete**
- [ ] Test synchronous request-reply with various timeout scenarios
- [ ] Test asynchronous send performance and message ordering
- [ ] Test process registration and name-based messaging
- [ ] Test selective receive filtering messages correctly

---

## 5.4 Integration Tests
- [ ] **Section 5.4 Complete**

Integration tests validate the complete actor system with realistic concurrent programs. We build applications using actors and supervisors, test fault tolerance through simulated failures, measure performance under load, and verify correct behavior in distributed scenarios. These tests ensure that Catena actors integrate seamlessly with BEAM's process model and provide the expected reliability.

### 5.4.1 Actor Communication Testing
- [ ] **Task 5.4.1 Complete**

We test various communication patterns between actors. Test synchronous calls, asynchronous sends, broadcast to multiple actors, and request-reply chains. Verify message ordering, timeout handling, and failure detection. Test both happy paths and error scenarios (dead processes, slow actors, network partitions in distributed tests).

- [ ] 5.4.1.1 Test point-to-point messaging between actor pairs with various message types
- [ ] 5.4.1.2 Test broadcast messaging sending to multiple actors simultaneously
- [ ] 5.4.1.3 Test message chains where actors call other actors forming pipelines
- [ ] 5.4.1.4 Test error handling when messages are sent to dead or non-existent processes

### 5.4.2 Fault Tolerance Testing
- [ ] **Task 5.4.2 Complete**

We verify that supervision trees provide fault tolerance. Crash child processes deliberately and verify supervisors restart them correctly according to strategy. Test restart intensity limits preventing crash loops. Verify that state is reset on restart (or recovered if persistence added). Test cascading failures and verify that supervision strategies contain failures appropriately.

- [ ] 5.4.2.1 Test supervisor restart of crashed children with different strategies
- [ ] 5.4.2.2 Test restart intensity limits stopping supervisor after too many crashes
- [ ] 5.4.2.3 Test cascading failure handling ensuring failures don't propagate unexpectedly
- [ ] 5.4.2.4 Test state recovery after restart with various initialization strategies

### 5.4.3 Performance Benchmarking
- [ ] **Task 5.4.3 Complete**

We measure actor system performance under load. Benchmark message throughput (messages/second), latency (message round-trip time), and scalability (performance with many actors). Compare Catena actors to native Erlang gen_servers—aim for overhead under 10%. Test with various workloads: many small messages, few large messages, request-reply vs fire-and-forget.

- [ ] 5.4.3.1 Benchmark message throughput with varying numbers of actors and message rates
- [ ] 5.4.3.2 Benchmark message latency measuring round-trip time for synchronous calls
- [ ] 5.4.3.3 Benchmark actor spawn time and memory overhead per actor
- [ ] 5.4.3.4 Compare Catena actor performance to equivalent Erlang gen_server implementation

### 5.4.4 Distributed Actor Testing
- [ ] **Task 5.4.4 Complete**

We test actors across multiple BEAM nodes (distributed Erlang). Spawn actors on remote nodes, send messages between nodes, verify that process linking and monitoring work across nodes. Test network partitions and node failures. This validates that Catena actors leverage BEAM's distribution transparency, enabling truly distributed systems.

- [ ] 5.4.4.1 Test remote actor spawning creating processes on different nodes
- [ ] 5.4.4.2 Test distributed messaging sending messages between nodes with location transparency
- [ ] 5.4.4.3 Test distributed failure detection using links and monitors across nodes
- [ ] 5.4.4.4 Test network partition handling and recovery when nodes disconnect

### 5.4.5 Actor-Effect System Integration
- [ ] **Task 5.4.5 Complete**

We validate the unification of actors with the Process effect system. Test that actor handlers correctly use Process effect operations, that effect tracking works in actor contexts, and that the actor model and effect system compose seamlessly. This demonstrates the key innovation: actors as effect handlers.

- [ ] 5.4.5.1 Test actor handlers using Process effect operations (spawn, send, receive) with correct effect tracking
- [ ] 5.4.5.2 Test effect composition with actors using both Process and IO effects in handlers
- [ ] 5.4.5.3 Test actor lifecycle integration with effect runtime (handler processes, effect propagation)
- [ ] 5.4.5.4 Test supervision as effect handling demonstrating fault tolerance through effect boundaries

---

## Success Criteria

1. **Actor Syntax**: Clean functional syntax for actors compiling to OTP behaviors
2. **Actor-Effect Unification**: Actors demonstrated as handlers for the Process effect
3. **State Immutability**: Type-enforced immutable state with functional updates
4. **Supervision Trees**: Declarative supervision with all OTP restart strategies
5. **Communication Patterns**: Synchronous and asynchronous messaging with type-and-effect safety
6. **Fault Tolerance**: Working supervision with automatic restart and recovery
7. **Performance**: Actor overhead under 10% compared to native Erlang gen_servers
8. **BEAM Integration**: Seamless interoperability with existing OTP applications

## Provides Foundation

This phase completes the proof-of-concept by demonstrating:
- **Category Theory + BEAM**: Functional abstractions running on battle-tested infrastructure
- **Algebraic Effects + Actors**: Unifying actors as effect handlers for stateful concurrent computation
- **Type-and-Effect Safety + Concurrency**: Static type-and-effect checking preventing common concurrency bugs
- **Fault Tolerance**: "Let it crash" philosophy with automatic recovery through effect boundaries
- **Distributed Computing**: Foundation for building distributed Catena applications with effect safety
- **Production Readiness**: Real systems can be built using Catena actors and effects
- **Phase 6**: Advanced effect features building on Process effect and actor integration

## Key Outputs

- Actor definition syntax with state types, message protocols, and effect signatures
- Effectful message handlers with `(Message, State) -> (NewState, Reply) / {Process}` signature
- Process primitives (spawn, send, call, link, monitor) as Process effect operations with type-and-effect safety
- Actor-effect unification demonstrating actors as handlers for Process effect
- Supervision tree syntax compiling to OTP supervisors
- All OTP restart strategies (one-for-one, one-for-all, rest-for-one)
- Communication patterns (request-reply, fire-and-forget, selective receive) with effect tracking
- Process registration and name-based messaging
- Complete actor system test suite including effect integration tests
- Performance benchmarks validating efficiency of effect-based actors
- Distributed actor examples demonstrating scalability
- Integration with OTP application structure
- Documentation of actor model usage patterns and actor-effect unification
