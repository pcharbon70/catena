# Phase 6: Effect System Completion

## Overview

This phase completes the algebraic effect system by implementing advanced features deferred from the proof-of-concept. While Phases 1-5 demonstrated minimal viable effects with monomorphic effect tracking, IO/Process effects only, and basic process-based runtime, Phase 6 elevates the effect system to production-ready status. We implement **effect polymorphism** enabling generic effectful code, **expanded effect library** providing standard effects for common patterns, **effect optimizations** reducing runtime overhead, and **advanced effect features** like delimited continuations and scoped effects.

Effect polymorphism allows functions to be generic over effect sets, similar to type polymorphism. A function can work with any effects, specific effect subsets, or no effects at all. This enables building reusable libraries that don't hard-code effect requirements. The expanded effect library provides State, Reader, Writer, Async, and Error effects following algebraic effect principles. Effect optimizations include fusion (combining multiple effect operations), handler inlining (eliminating effect overhead for pure code), and static resolution (compile-time effect dispatch).

This phase runs for **5 weeks** as a post-PoC completion phase, transforming the effect system from demonstration to production capability. By the end, Catena will have a complete, efficient, and ergonomic algebraic effect system rivaling research languages like Koka and Eff while maintaining BEAM compatibility and performance.

---

## 6.1 Effect Polymorphism
- [ ] **Section 6.1 Complete**

Effect polymorphism extends the type system to treat effects as first-class abstraction parameters. Functions can be polymorphic over effect sets using effect variables (ε, δ, φ). Type signatures like `(a -> b / ε) -> List a -> List b / ε` describe functions generic over any effect set. Effect constraints restrict polymorphism: `f : a -> b / {IO | ε}` requires at least IO effect. We implement effect unification, effect inference for polymorphic functions, and effect instantiation at call sites.

### 6.1.1 Effect Variables
- [ ] **Task 6.1.1 Complete**

Effect variables represent unknown or polymorphic effect sets. Syntax: lowercase Greek letters (ε, δ, φ) in type signatures. Effect variables unify like type variables—two effect variables unify if their effect sets are compatible. We implement effect variable representation, substitution, and occurs checking (preventing infinite effect sets).

- [ ] 6.1.1.1 Implement effect variable representation in type-and-effect terms
- [ ] 6.1.1.2 Implement effect variable substitution for unification and instantiation
- [ ] 6.1.1.3 Implement occurs check preventing infinite effect sets during unification
- [ ] 6.1.1.4 Implement effect variable pretty-printing in type signatures

### 6.1.2 Effect Constraints
- [ ] **Task 6.1.2 Complete**

Effect constraints restrict what effects can instantiate effect variables. Row constraints specify required effects: `{FileIO | ε}` means "at least FileIO". Absence constraints specify forbidden effects: `ε \ {Process}` means "any effects except Process". We implement constraint representation, entailment checking, and constraint simplification.

- [ ] 6.1.2.1 Implement row constraint syntax and representation for required effects
- [ ] 6.1.2.2 Implement absence constraint syntax and representation for forbidden effects
- [ ] 6.1.2.3 Implement constraint entailment checking determining if constraints imply others
- [ ] 6.1.2.4 Implement constraint simplification reducing constraints to canonical form

### 6.1.3 Effect Inference for Polymorphism
- [ ] **Task 6.1.3 Complete**

Extending Algorithm W to infer effect variables requires tracking effect constraints alongside type constraints. When inferring a function's effects, we generate effect variables for unknown effect sets, collect constraints from perform operations and handlers, and solve constraints to find most general effect signatures. Let-polymorphism generalizes effect variables at let bindings.

- [ ] 6.1.3.1 Extend Algorithm W to generate effect variables for polymorphic functions
- [ ] 6.1.3.2 Implement effect constraint generation from perform operations and handlers
- [ ] 6.1.3.3 Implement effect unification unifying effect variables with concrete effect sets
- [ ] 6.1.3.4 Implement effect generalization introducing effect quantifiers at let bindings

### 6.1.4 Effect Instantiation
- [ ] **Task 6.1.4 Complete**

When calling polymorphic effectful functions, we instantiate effect variables with concrete effect sets from the calling context. Effect instantiation substitutes effect variables with actual effects, checks that effect constraints are satisfied, and propagates instantiated effects to caller's effect set. This enables generic effectful code while maintaining effect safety.

- [ ] 6.1.4.1 Implement effect variable instantiation substituting variables with concrete effects
- [ ] 6.1.4.2 Implement constraint satisfaction checking ensuring instantiation respects constraints
- [ ] 6.1.4.3 Implement effect propagation merging instantiated effects into caller's effect set
- [ ] 6.1.4.4 Implement effect inference for higher-order functions preserving polymorphism

### Unit Tests - Section 6.1
- [ ] **Unit Tests 6.1 Complete**
- [ ] Test effect variable unification with various effect sets
- [ ] Test effect constraints (row constraints, absence constraints) with entailment checking
- [ ] Test effect polymorphism inference for generic functions
- [ ] Test effect instantiation at call sites with constraint checking
- [ ] Test interaction between type polymorphism and effect polymorphism

---

## 6.2 Expanded Effect Library
- [ ] **Section 6.2 Complete**

Beyond IO and Process effects from the PoC, we implement a comprehensive standard effect library. State effect provides mutable state without actual mutation. Reader effect supplies read-only environment/configuration. Writer effect accumulates output (logs, traces). Async effect handles asynchronous computation with futures/promises. Error effect provides typed exceptions. Each effect has handlers demonstrating different implementation strategies.

### 6.2.1 State Effect
- [ ] **Task 6.2.1 Complete**

State effect provides get/put operations for mutable state without side effects. Operations: `get : Unit -> s / {State s}`, `put : s -> Unit / {State s}`. State handlers thread state through computation as accumulator. Multiple state types can coexist using type parameters. We implement State effect, pure handler (returns final state), and stateful handler (uses actual mutable reference).

- [ ] 6.2.1.1 Define State effect with get and put operations parameterized by state type
- [ ] 6.2.1.2 Implement pure State handler threading state as accumulator through computation
- [ ] 6.2.1.3 Implement stateful State handler using Erlang process dictionary for performance
- [ ] 6.2.1.4 Implement State effect examples (counter, accumulator, mutable algorithms)

### 6.2.2 Reader Effect
- [ ] **Task 6.2.2 Complete**

Reader effect provides read-only access to environment/configuration. Operation: `ask : Unit -> r / {Reader r}`. Reader handlers supply environment value to all ask operations. Useful for dependency injection, configuration, and implicit parameters. We implement Reader effect, handler providing environment, and local modification (temporary environment changes).

- [ ] 6.2.2.1 Define Reader effect with ask operation parameterized by environment type
- [ ] 6.2.2.2 Implement Reader handler providing environment value to computation
- [ ] 6.2.2.3 Implement local operation temporarily modifying environment for subcomputation
- [ ] 6.2.2.4 Implement Reader effect examples (configuration, dependency injection, context)

### 6.2.3 Writer Effect
- [ ] **Task 6.2.3 Complete**

Writer effect accumulates output values (logs, traces, audits). Operation: `tell : w -> Unit / {Writer w}` where w is a monoid. Writer handlers collect all tell outputs into accumulator. Final result is (value, accumulated_output) pair. We implement Writer effect, handler accumulating with monoid append, and filtering (selective output).

- [ ] 6.2.3.1 Define Writer effect with tell operation parameterized by output monoid type
- [ ] 6.2.3.2 Implement Writer handler accumulating output using monoid append operation
- [ ] 6.2.3.3 Implement listen operation capturing output of subcomputation
- [ ] 6.2.3.4 Implement Writer effect examples (logging, tracing, audit trails)

### 6.2.4 Async Effect
- [ ] **Task 6.2.4 Complete**

Async effect handles asynchronous computation with futures/promises. Operations: `async : (Unit -> a / ε) -> Future a / {Async}`, `await : Future a -> a / {Async}`. Async handlers spawn BEAM processes for concurrent computation, leveraging process-based runtime. Integrates with actor model for structured concurrency.

- [ ] 6.2.4.1 Define Async effect with async and await operations using Future type
- [ ] 6.2.4.2 Implement Async handler spawning BEAM processes for concurrent execution
- [ ] 6.2.4.3 Implement await operation blocking until future completes with timeout support
- [ ] 6.2.4.4 Implement Async effect examples (parallel map, concurrent requests, pipelines)

### 6.2.5 Error Effect
- [ ] **Task 6.2.5 Complete**

Error effect provides typed exceptions. Operation: `throw : e -> a / {Error e}`. Error handlers catch exceptions and handle them. Unlike Result types, errors propagate automatically until handled. We implement Error effect, catch handler converting errors to Result, and error transformation (mapping error types).

- [ ] 6.2.5.1 Define Error effect with throw operation parameterized by error type
- [ ] 6.2.5.2 Implement Error handler catching errors and converting to Result type
- [ ] 6.2.5.3 Implement error propagation automatically unwinding until handler found
- [ ] 6.2.5.4 Implement Error effect examples (validation, parsing, error handling)

### Unit Tests - Section 6.2
- [ ] **Unit Tests 6.2 Complete**
- [ ] Test State effect with get/put operations and pure handler
- [ ] Test Reader effect with ask operation and local modification
- [ ] Test Writer effect with tell operation and output accumulation
- [ ] Test Async effect with async/await and concurrent execution
- [ ] Test Error effect with throw/catch and error propagation
- [ ] Test effect composition combining multiple effects in same function

---

## 6.3 Effect Optimizations
- [ ] **Section 6.3 Complete**

Effect system overhead must be minimal for production use. We implement optimizations reducing runtime cost: effect fusion combines multiple effect operations into single operation, handler inlining eliminates overhead for simple handlers, static resolution dispatches effects at compile time when possible, and effect specialization generates optimized code for monomorphic effect use. Goal: effect overhead under 5% compared to hand-written BEAM code.

### 6.3.1 Effect Fusion
- [ ] **Task 6.3.1 Complete**

Effect fusion combines consecutive effect operations into single operation, reducing message passing overhead. Example: `get >>= \s -> put (s+1)` fuses to single modify operation. We implement fusion rules for common patterns, fusion analysis detecting fusible sequences, and fused code generation combining operations.

- [ ] 6.3.1.1 Implement fusion rules for State effect (get-put fusion, consecutive puts)
- [ ] 6.3.1.2 Implement fusion analysis detecting fusible effect operation sequences
- [ ] 6.3.1.3 Implement fused operation generation combining multiple operations
- [ ] 6.3.1.4 Implement fusion correctness preservation ensuring semantics unchanged

### 6.3.2 Handler Inlining
- [ ] **Task 6.3.2 Complete**

Simple handlers can be inlined at effect sites, eliminating process spawning and message passing. Pure handlers (no actual effects) inline completely, removing all effect overhead. We implement inlining analysis determining inlinable handlers, inline transformation replacing effect operations with handler bodies, and specialization generating optimal code for inlined handlers.

- [ ] 6.3.2.1 Implement inlining analysis identifying handlers safe to inline
- [ ] 6.3.2.2 Implement handler body inlining substituting effect operations with handler code
- [ ] 6.3.2.3 Implement specialization generating optimized code for inlined handlers
- [ ] 6.3.2.4 Implement inlining heuristics balancing code size vs performance

### 6.3.3 Static Effect Resolution
- [ ] **Task 6.3.3 Complete**

When effect handlers are statically known (not passed as parameters), we can resolve effect operations at compile time. Static resolution generates direct function calls instead of dynamic dispatch, eliminating effect runtime overhead. We implement static handler analysis, compile-time effect dispatch, and devirtualization transforming dynamic effects to static calls.

- [ ] 6.3.3.1 Implement static handler analysis determining when handlers are compile-time constants
- [ ] 6.3.3.2 Implement static effect dispatch generating direct calls instead of message passing
- [ ] 6.3.3.3 Implement devirtualization transforming effect operations to direct function calls
- [ ] 6.3.3.4 Implement escape analysis detecting when effects don't escape handler scope

### 6.3.4 Effect Specialization
- [ ] **Task 6.3.4 Complete**

Polymorphic effectful functions can be specialized for monomorphic use cases, generating optimized code without effect polymorphism overhead. Specialization duplicates function bodies for each concrete effect set, performs effect-specific optimizations, and generates optimized BEAM code. Balances code size against performance.

- [ ] 6.3.4.1 Implement specialization analysis identifying profitable specialization opportunities
- [ ] 6.3.4.2 Implement function body duplication for each monomorphic effect instantiation
- [ ] 6.3.4.3 Implement effect-specific optimization passes for specialized functions
- [ ] 6.3.4.4 Implement specialization heuristics avoiding code bloat from over-specialization

### Unit Tests - Section 6.3
- [ ] **Unit Tests 6.3 Complete**
- [ ] Test effect fusion combining operations with preserved semantics
- [ ] Test handler inlining eliminating effect overhead for simple cases
- [ ] Test static effect resolution generating direct calls
- [ ] Test effect specialization for monomorphic uses
- [ ] Benchmark optimization impact comparing optimized vs unoptimized effect code

---

## 6.4 Advanced Effect Features
- [ ] **Section 6.4 Complete**

Advanced features enable sophisticated effect patterns: delimited continuations provide resumption control, scoped effects restrict effect visibility, effectful guards allow effects in pattern guards, and effect debugging provides runtime introspection. These features demonstrate the full power of algebraic effects while maintaining implementation feasibility on BEAM.

### 6.4.1 Delimited Continuations
- [ ] **Task 6.4.1 Complete**

Effect handlers have access to continuation—the rest of computation after perform. Handlers can resume continuation zero, one, or multiple times. This enables control flow effects: early return, backtracking, exceptions. We implement continuation capture, continuation invocation, and multi-resume handlers (resuming continuation multiple times).

- [ ] 6.4.1.1 Implement continuation capture at perform sites using BEAM exceptions or CPS
- [ ] 6.4.1.2 Implement continuation resume allowing handlers to continue computation
- [ ] 6.4.1.3 Implement multi-resume handlers supporting multiple continuation invocations
- [ ] 6.4.1.4 Implement continuation examples (generators, backtracking, coroutines)

### 6.4.2 Scoped Effects
- [ ] **Task 6.4.2 Complete**

Effect scoping restricts where effects can be performed. Scoped effects require handler in same syntactic scope, preventing effects from escaping. Useful for resource management: files opened within scope must be closed before scope exits. We implement scope checking, scope-based effect resolution, and scope violation detection.

- [ ] 6.4.2.1 Implement scoped effect declaration syntax marking effects as scoped
- [ ] 6.4.2.2 Implement scope checking preventing scoped effects from escaping handlers
- [ ] 6.4.2.3 Implement resource management patterns using scoped effects (files, locks)
- [ ] 6.4.2.4 Implement scope violation errors with clear messages and suggestions

### 6.4.3 Effectful Guards
- [ ] **Task 6.4.3 Complete**

Extending pattern guards to allow effects enables rich matching conditions. Effectful guards can perform IO, access state, or throw errors during pattern matching. Deferred from Phase 3, this feature requires careful semantics: guard evaluation order, effect handling during matching, and short-circuit evaluation.

- [ ] 6.4.3.1 Extend guard syntax to allow perform operations in guard expressions
- [ ] 6.4.3.2 Implement guard effect checking tracking effects in guard expressions
- [ ] 6.4.3.3 Implement guard evaluation with effect handling during pattern matching
- [ ] 6.4.3.4 Implement guard examples (database lookups, validation, complex conditions)

### 6.4.4 Effect Debugging Tools
- [ ] **Task 6.4.4 Complete**

Effect debugging provides runtime introspection of effect execution. Effect traces show all performed operations and handler invocations. Effect profiling measures effect overhead per effect type. Effect visualization displays effect flow through program. We implement trace collection, profiling instrumentation, and debugging output formatting.

- [ ] 6.4.4.1 Implement effect tracing recording all perform operations and handler calls
- [ ] 6.4.4.2 Implement effect profiling measuring time and message counts per effect
- [ ] 6.4.4.3 Implement effect visualization generating flow diagrams of effect execution
- [ ] 6.4.4.4 Implement debugging commands in REPL for effect introspection

### Unit Tests - Section 6.4
- [ ] **Unit Tests 6.4 Complete**
- [ ] Test delimited continuations with single and multiple resumes
- [ ] Test scoped effects preventing escape and enabling resource safety
- [ ] Test effectful guards in pattern matching with various effects
- [ ] Test effect debugging tools producing correct traces and profiles

---

## 6.5 Law Verification System
- [ ] **Section 6.5 Complete**

Implement automated law verification for category theory properties, demonstrating that Catena trait instances satisfy algebraic laws. This validates the category-theory-first approach by ensuring mathematical correctness through property-based testing. Laws are defined within trait declarations and verified automatically for all instances.

### 6.5.1 Laws Syntax
- [ ] **Task 6.9.1 Complete**

Add `laws` keyword for grouping trait laws within trait declarations. Laws use property testing syntax with `forall` quantification and trait method constraints. Each law has a descriptive name and expresses a universal property that all instances must satisfy.

- [ ] 6.5.1.1 Add `laws` keyword to lexer and parser for trait law grouping
- [ ] 6.5.1.2 Implement law definition syntax within trait declarations producing LawDecl AST nodes
- [ ] 6.5.1.3 Implement law storage associating laws with their traits in symbol table
- [ ] 6.5.1.4 Implement law syntax validation checking laws use only trait methods and type variables

### 6.5.2 Law Verification
- [ ] **Task 6.9.2 Complete**

Implement `verify laws Trait for Type` command that automatically tests all laws for a given trait instance. Verification uses property testing framework from Phase 2, generating random test cases and checking law properties hold. Failed verifications report counterexamples.

- [ ] 6.5.2.1 Implement `verify laws` syntax and command for automatic law checking
- [ ] 6.5.2.2 Implement law checking via property testing generating test cases from law forall clauses
- [ ] 6.5.2.3 Implement law verification reporting showing pass/fail for each law with counterexamples on failure
- [ ] 6.5.2.4 Verify Functor, Applicative, and Monad laws for all standard library instances (Maybe, List, Result)

### Unit Tests - Section 6.5
- [ ] **Unit Tests 6.5 Complete**
- [ ] Test laws syntax parsing for trait declarations with multiple laws
- [ ] Test law storage and retrieval from symbol table
- [ ] Test verify laws command for Functor laws on List and Maybe
- [ ] Test law verification reporting showing counterexamples for failing laws
- [ ] Test law verification integration with property testing framework

---

## 6.6 Complete Standard Library
- [ ] **Section 6.6 Complete**

Implement remaining category theory abstractions beyond the minimal set from Phase 2. This completes the standard library hierarchy with all 17 core abstractions, demonstrating Catena's comprehensive category theory foundations. Each trait includes laws and instances for standard types.

### 6.6.1 Ord and Comparison
- [ ] **Task 6.6.1 Complete**

Implement Ord trait extending Setoid with total ordering. Provides comparison operations returning Ordering (LT, EQ, GT) and derived operations (<, <=, >, >=). Instances for Natural, Text, and other comparable types.

- [ ] 6.6.1.1 Define Ord trait extending Setoid with `compare : a -> a -> Ordering` method
- [ ] 6.6.1.2 Define derived comparison operators: `<`, `<=`, `>`, `>=` with correct precedence
- [ ] 6.6.1.3 Implement Ord instances for Natural, Text, Bool with antisymmetry and transitivity laws
- [ ] 6.6.1.4 Verify Ord laws for all instances using law verification system

### 6.6.2 Semigroupoid and Category
- [ ] **Task 6.6.2 Complete**

Implement Semigroupoid (composition without identity) and Category (composition with identity) traits. These formalize composition patterns and enable categorical abstractions like Arrows.

- [ ] 6.6.2.1 Define Semigroupoid trait with `compose : (b ~> c) -> (a ~> b) -> (a ~> c)` and `>>>`, `<<<` operators
- [ ] 6.6.2.2 Define Category trait extending Semigroupoid with `identity : a ~> a`
- [ ] 6.6.2.3 Implement Semigroupoid and Category instances for function type (->)
- [ ] 6.6.2.4 Verify associativity and identity laws for Category instances

### 6.6.3 Foldable and Traversable
- [ ] **Task 6.6.3 Complete**

Implement Foldable (reduction to single value) and Traversable (effectful traversal) traits. These enable generic operations over container types.

- [ ] 6.6.3.1 Define Foldable trait with `fold : Monoid m => (a -> m) -> f a -> m` method
- [ ] 6.6.3.2 Define Traversable trait extending (Functor, Foldable) with `traverse : Applicative f => (a -> f b) -> t a -> f (t b)`
- [ ] 6.6.3.3 Implement Foldable and Traversable instances for List and Maybe
- [ ] 6.6.3.4 Verify traversal laws (identity, composition, naturality)

### 6.6.4 Bifunctor, Extend, Comonad, Arrow
- [ ] **Task 6.6.4 Complete**

Implement remaining advanced traits: Bifunctor (functors of two arguments), Extend (comonadic extend), Comonad (categorical dual of Monad), and Arrow (generalized functions).

- [ ] 6.6.4.1 Define Bifunctor trait with `bimap : (a -> c) -> (b -> d) -> p a b -> p c d`
- [ ] 6.6.4.2 Define Extend trait with `extend : (w a -> b) -> w a -> w b` and `=>>`, `<<=` operators
- [ ] 6.6.4.3 Define Comonad trait extending Extend with `extract : w a -> a`
- [ ] 6.6.4.4 Define Arrow trait with `arr`, `first`, `second`, and `***`, `&&&` operators

### Unit Tests - Section 6.6
- [ ] **Unit Tests 6.6 Complete**
- [ ] Test Ord instances with comparison operations and transitivity
- [ ] Test Category instances with composition and identity laws
- [ ] Test Foldable instances folding lists and maybes correctly
- [ ] Test Traversable instances with effectful traversals
- [ ] Test Bifunctor, Extend, Comonad, and Arrow instances with their respective laws

---

## 6.7 Advanced Testing
- [ ] **Section 6.7 Complete**

Complete the testing framework with advanced features deferred from Phase 2: test suites for organization, benchmarks for performance testing, custom generators for complex types, and test coverage reporting.

### 6.7.1 Test Suites
- [ ] **Task 6.7.1 Complete**

Implement `suite` keyword for organizing related tests into groups. Suites provide hierarchical test organization and enable running specific test groups.

- [ ] 6.7.1.1 Add `suite` keyword and nesting syntax to parser producing SuiteDecl AST nodes
- [ ] 6.7.1.2 Implement suite execution running all tests within suite recursively
- [ ] 6.7.1.3 Implement suite filtering running specific suites by name or pattern
- [ ] 6.7.1.4 Implement hierarchical suite reporting showing pass/fail counts per suite

### 6.7.2 Benchmarks
- [ ] **Task 6.7.2 Complete**

Implement `benchmark` keyword for performance testing with baselines and requirements. Benchmarks measure execution time and memory usage, comparing implementations and enforcing performance requirements.

- [ ] 6.7.2.1 Add `benchmark` keyword with baseline and requirements syntax to parser
- [ ] 6.7.2.2 Implement `measure` keyword marking code for performance measurement
- [ ] 6.7.2.3 Implement benchmark execution measuring time and memory for each baseline
- [ ] 6.7.2.4 Implement benchmark reporting comparing baselines and checking requirements

### 6.7.3 Custom Generators and Shrinking
- [ ] **Task 6.7.3 Complete**

Extend property testing with custom generators for complex types and shrinking for minimal counterexamples. Generators compose to build complex test data, shrinking reduces failing cases to simplest form.

- [ ] 6.7.3.1 Implement generator combinators: constant, choose, one_of, list_of, tuple_of
- [ ] 6.7.3.2 Implement shrinking for basic types reducing counterexamples to minimal form
- [ ] 6.7.3.3 Implement user-defined generators for custom types with derive syntax
- [ ] 6.7.3.4 Integrate shrinking into property test failure reporting

### Unit Tests - Section 6.7
- [ ] **Unit Tests 6.7 Complete**
- [ ] Test suite organization and execution with nested suites
- [ ] Test benchmark measurement and reporting with time/memory metrics
- [ ] Test custom generators producing valid values for complex types
- [ ] Test shrinking reducing counterexamples to minimal failing cases

---

## 6.8 Operator Definition Syntax
- [ ] **Section 6.8 Complete**

Implement user-defined operators enabling custom infix, prefix, and postfix operators with precedence and associativity control. This completes the dual notation system by allowing developers to define domain-specific operators.

### 6.8.1 Operator Declarations
- [ ] **Task 6.8.1 Complete**

Add `operator` keyword for binding operators to functions. Operators can be infix (binary), prefix (unary left), or postfix (unary right) with specified precedence and associativity.

- [ ] 6.8.1.1 Add `operator` keyword and operator pattern syntax to lexer and parser
- [ ] 6.8.1.2 Implement operator binding syntax: `operator (op) = function [fixity precedence]`
- [ ] 6.8.1.3 Implement precedence and associativity specifications: infixl, infixr, infix, prefix, postfix
- [ ] 6.8.1.4 Implement operator registration in symbol table with precedence/associativity metadata

### 6.8.2 Operator Resolution
- [ ] **Task 6.8.2 Complete**

Implement operator parsing and resolution using precedence climbing or Pratt parsing. Custom operators integrate into expression parser, respecting precedence and associativity rules.

- [ ] 6.8.2.1 Implement precedence climbing parser for expression parsing with custom operators
- [ ] 6.8.2.2 Implement operator precedence resolution using symbol table metadata
- [ ] 6.8.2.3 Implement operator associativity handling for left/right/non-associative operators
- [ ] 6.8.2.4 Implement operator desugaring expanding operators to function applications in AST

### Unit Tests - Section 6.8
- [ ] **Unit Tests 6.8 Complete**
- [ ] Test operator declarations with various fixities and precedences
- [ ] Test operator parsing in expressions with correct precedence
- [ ] Test operator associativity (left, right, non-associative)
- [ ] Test custom operator integration with existing operators

---

## 6.9 Integration Tests
- [ ] **Section 6.9 Complete**

Integration tests validate the complete advanced effect system with realistic programs using all features together. We test effect polymorphism in libraries, effect composition combining multiple effects, effect optimizations improving performance measurably, and advanced features in production-like scenarios.

### 6.9.1 Polymorphic Effect Libraries
- [ ] **Task 6.9.1 Complete**

We build libraries using effect polymorphism, demonstrating generic effectful code. Examples: polymorphic map working with any effect, transactional abstractions generic over state effects, logging libraries generic over writer effects. Tests verify libraries work with concrete effect instantiations.

- [ ] 6.9.1.1 Implement polymorphic map function generic over effect sets
- [ ] 6.9.1.2 Implement transaction library using polymorphic State effect
- [ ] 6.9.1.3 Implement logging library using polymorphic Writer effect
- [ ] 6.9.1.4 Test libraries with various concrete effect instantiations

### 6.9.2 Multi-Effect Programs
- [ ] **Task 6.9.2 Complete**

Programs combining multiple effects test effect composition. Example: web server using IO (HTTP), State (session), Reader (config), Error (validation), and Async (concurrency). Tests verify effects compose correctly, handlers nest properly, and effect sets track accurately through complex programs.

- [ ] 6.9.2.1 Implement web server using IO, State, Reader, Error, and Async effects
- [ ] 6.9.2.2 Implement data pipeline using multiple effects for ETL operations
- [ ] 6.9.2.3 Implement game server using effects for game state, networking, and actors
- [ ] 6.9.2.4 Test effect composition correctness and handler nesting

### 6.9.3 Optimization Benchmarks
- [ ] **Task 6.9.3 Complete**

Performance benchmarks validate that optimizations achieve target <5% overhead. We compare optimized effect code against unoptimized effect code and hand-written BEAM code. Measure throughput, latency, and memory usage across effect types. Profile to identify remaining optimization opportunities.

- [ ] 6.9.3.1 Benchmark State effect performance with and without optimizations
- [ ] 6.9.3.2 Benchmark effect polymorphism overhead comparing monomorphic vs polymorphic code
- [ ] 6.9.3.3 Benchmark handler inlining measuring overhead elimination
- [ ] 6.9.3.4 Compare optimized effect code to equivalent hand-written BEAM code

### 6.9.4 Advanced Feature Integration
- [ ] **Task 6.9.4 Complete**

Tests using advanced features demonstrate full effect system capability. Implement generators using delimited continuations, resource management using scoped effects, complex pattern matching using effectful guards. Validate debugging tools work correctly on complex programs.

- [ ] 6.9.4.1 Implement generators using delimited continuations and multi-resume
- [ ] 6.9.4.2 Implement resource-safe file handling using scoped effects
- [ ] 6.9.4.3 Implement complex pattern matching using effectful guards
- [ ] 6.9.4.4 Test debugging tools on multi-effect programs producing useful output

---

## 6.5 Category Theory Completion
- [ ] **Section 6.5 Complete**

Complete the category theory library with advanced abstractions that were deferred from the proof-of-concept phases. This section finalizes Catena's mathematical foundation by implementing StructuredMapper (Applicative), Alternative, Foldable, Traversable, and optimizations that prove categorical abstractions have zero overhead on BEAM. We also establish CI-integrated law verification ensuring all library code maintains mathematical correctness.

### 6.5.1 Advanced Categorical Abstractions
- [ ] **Task 6.5.1 Complete**

Implement the remaining category theory abstractions that complete the functorial hierarchy. StructuredMapper (Applicative) sits between Mapper and Workflow providing parallel composition. Alternative provides choice and failure. Foldable enables collapsing structures. Traversable combines mapping with effects.

- [ ] 6.5.1.1 Implement StructuredMapper (Applicative) trait with `pure : a -> f a` and `apply : f (a -> b) -> f a -> f b` with `<*>` operator
- [ ] 6.5.1.2 Implement Alternative trait for choice/failure with `empty : f a` and `alt : f a -> f a -> f a` with `<|>` operator
- [ ] 6.5.1.3 Implement Foldable trait with `foldMap : Accumulator m => (a -> m) -> t a -> m` for collapsing structures
- [ ] 6.5.1.4 Implement Traversable trait with `traverse : StructuredMapper f => (a -> f b) -> t a -> f (t b)` for effectful traversal
- [ ] 6.5.1.5 Verify all trait laws via property testing: applicative laws, alternative laws, foldable laws, traversable laws
- [ ] 6.5.1.6 Implement instances for all standard types (List, Maybe, Result, Tree) with law verification

### 6.5.2 Category Theory Optimizations
- [ ] **Task 6.5.2 Complete**

Prove categorical abstractions have zero overhead on BEAM through compile-time optimizations. These optimizations demonstrate that mathematical abstractions don't sacrifice performance, making category theory practical for production use.

- [ ] 6.5.2.1 Implement functor fusion: detect `map f . map g` patterns and rewrite to `map (f . g)` eliminating intermediate structures
- [ ] 6.5.2.2 Implement bind inlining for known monads: inline Maybe/List/Result bind definitions at compile time
- [ ] 6.5.2.3 Implement build/foldr fusion (deforestation) for list operations preventing intermediate list creation
- [ ] 6.5.2.4 Implement rewrite rules system allowing library authors to specify optimization patterns
- [ ] 6.5.2.5 Benchmark optimizations: verify <5% overhead vs hand-written Erlang for common patterns
- [ ] 6.5.2.6 Generate optimization reports showing which rules fired during compilation for transparency

### 6.5.3 Law Verification in CI
- [ ] **Task 6.5.3 Complete**

Ensure all library code maintains categorical laws through continuous integration. This makes law verification a standard part of the development process, catching violations before they reach production.

- [ ] 6.5.3.1 Integrate law verification into test suite running property tests for all trait instances
- [ ] 6.5.3.2 Generate QuickCheck-style random data for all types using derive mechanisms for automatic generators
- [ ] 6.5.3.3 Run exhaustive law verification on all instances with configurable test counts (100, 1000, 10000 tests)
- [ ] 6.5.3.4 Create law violation dashboard showing which instances fail with counterexamples and statistics
- [ ] 6.5.3.5 Implement law coverage metrics tracking which laws are tested for each trait instance
- [ ] 6.5.3.6 Add CI pipeline stage that blocks merge if any law violations detected

### 6.5.4 Advanced Type Classes
- [ ] **Task 6.5.4 Complete**

Implement advanced type classes that demonstrate Catena's full categorical power. These abstractions show that Catena can express sophisticated mathematical concepts while remaining practical.

- [ ] 6.5.4.1 Implement Contravariant trait for contravariant functors with `contramap : (b -> a) -> f a -> f b`
- [ ] 6.5.4.2 Implement Profunctor trait for profunctors with `dimap : (a -> b) -> (c -> d) -> p b c -> p a d`
- [ ] 6.5.4.3 Implement Arrow trait extending Category with `arr : (a -> b) -> arr a b` and `first : arr a b -> arr (a,c) (b,c)`
- [ ] 6.5.4.4 Implement Comonad trait (dual of Monad) with `extract : w a -> a` and `extend : (w a -> b) -> w a -> w b`
- [ ] 6.5.4.5 Verify all advanced trait laws through property testing
- [ ] 6.5.4.6 Provide practical examples showing real-world uses (not just academic exercises)

### Unit Tests - Section 6.5
- [ ] **Unit Tests 6.5 Complete**
- [ ] Test StructuredMapper instances for List, Maybe, Result with applicative laws
- [ ] Test Alternative instances with choice and failure semantics
- [ ] Test Foldable operations producing correct reductions
- [ ] Test Traversable operations maintaining structure while sequencing effects
- [ ] Test functor fusion optimization detecting and rewriting map compositions
- [ ] Test bind inlining reducing Maybe/List/Result overhead
- [ ] Test law verification catching intentional violations with counterexamples
- [ ] Test advanced type classes (Contravariant, Profunctor, Arrow, Comonad) with instances

### Integration Tests - Section 6.5
- [ ] **Integration Tests 6.5 Complete**
- [ ] Compile parser combinator library using StructuredMapper and Alternative abstractions
- [ ] Compile web framework using Foldable/Traversable for request processing
- [ ] Benchmark optimized category theory code vs hand-written Erlang showing <5% overhead
- [ ] Run full law verification suite on standard library completing in <60 seconds
- [ ] Demonstrate Arrow-based stream processing maintaining performance
- [ ] Build real application using all category theory abstractions proving practical viability

---

## Success Criteria

1. **Effect Polymorphism**: Generic effectful functions with effect variables and constraints
2. **Effect Library**: Complete standard effect library (State, Reader, Writer, Async, Error)
3. **Optimizations**: Effect overhead under 5% compared to hand-written BEAM code
4. **Advanced Features**: Working delimited continuations, scoped effects, and effectful guards
5. **Debugging Tools**: Effect tracing, profiling, and visualization for development
6. **Production Ready**: Effect system suitable for building real applications
7. **Test Coverage**: 85% coverage with comprehensive tests for all advanced features

## Provides Foundation

This phase completes the effect system, enabling:
- **Production Applications**: Full-featured effect system for real-world development
- **Effect Libraries**: Third-party effect libraries leveraging polymorphism
- **Research Extensions**: Foundation for effect system research (effect inference, effect types)
- **BEAM Innovation**: Demonstrating algebraic effects on production VM
- **Future Work**: Effect system evolution and optimization continued development

## Key Outputs

- Effect polymorphism with effect variables and constraints
- Effect unification and inference for polymorphic functions
- Complete standard effect library (State, Reader, Writer, Async, Error)
- Effect optimizations (fusion, inlining, static resolution, specialization)
- Delimited continuations for control flow effects
- Scoped effects for resource safety
- Effectful pattern guards for rich matching
- Effect debugging and profiling tools
- Comprehensive test suite covering all advanced features
- Performance benchmarks demonstrating optimization effectiveness
- Documentation of advanced effect patterns and best practices
- Demonstration applications showcasing full effect system capabilities
