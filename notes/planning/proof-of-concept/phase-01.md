# Phase 1: Core Language Infrastructure

## Overview

This phase establishes the foundational compiler infrastructure for Catena, transforming source code into executable BEAM bytecode. We build the lexer for tokenization, the parser for AST generation, the type inference engine based on Hindley-Milner with categorical extensions, and the **minimal viable algebraic effect system**. The goal is to create a robust compilation pipeline that can parse basic Catena syntax (`type` for types, `transform` for morphisms, `effect` for side-effect declarations), perform type-and-effect checking with support for parametric polymorphism, and generate Core Erlang code that executes on the BEAM VM.

By the end of this phase, we will have a working compiler that can take simple Catena programs with effects, infer their types and track their effects automatically, and produce valid `.beam` files. This establishes the technical foundation for all subsequent phases, including the effect system that makes Catena unique: category-theoretic purity meeting BEAM pragmatism through algebraic effects.

**Effect System Scope for PoC**: This phase implements minimal viable effects with monomorphic effect tracking (no effect polymorphism), basic effect inference, process-based effect runtime, and IO/Process effects only. Full effect features (polymorphism, standard library, optimizations) are deferred to Phase 6.

This phase runs for **6.5 weeks** and focuses on correctness over optimization, establishing clean abstractions and comprehensive testing to support future feature additions without architectural rework.

---

## 1.1 Lexer and Parser
- [x] **Section 1.1 Complete**

The lexer and parser form the front-end of the compiler, transforming raw Catena source code into structured Abstract Syntax Trees (ASTs). The lexer breaks input text into tokens (keywords, operators, identifiers, literals), while the parser organizes these tokens according to Catena grammar rules. We implement this using Erlang's leex (lexical analyzer generator) and yecc (LALR parser generator) tools, which provide mature, battle-tested parsing infrastructure. The parser must handle Catena's unique syntax including categorical terminology (`type`, `transform`), composition operators (`|>`, `>>=`), and pattern matching constructs. Error recovery and helpful error messages are priorities from the start.

### 1.1.1 Token Recognition
- [x] **Task 1.1.1 Complete**

Token recognition involves defining lexical rules that classify input characters into meaningful tokens. We must identify keywords (`type`, `transform`, `match`, `let`, `in`, `end`, `trait`, `instance`, `effect`, `perform`, `handle`), operators (`|>`, `->`, `:`, `=`, `<>`, `>>=`), delimiters (`{`, `}`, `[`, `]`, `(`, `)`, `|`), literals (numbers, strings, atoms), and comments (single-line `--` and multi-line `{- -}`). The lexer must handle whitespace correctly, track line/column positions for error reporting, and distinguish between similar tokens (e.g., `->` vs `-`).

- [x] 1.1.1.1 Define token types and lexical rules for all Catena keywords, operators, and delimiters
- [x] 1.1.1.2 Implement number literal recognition supporting integers, floats, and scientific notation
- [x] 1.1.1.3 Implement string literal recognition with escape sequences and multi-line support
- [x] 1.1.1.4 Implement comment recognition for single-line and multi-line comments with proper nesting

### 1.1.2 Grammar Implementation
- [x] **Task 1.1.2 Complete**

The parser grammar defines the syntactic structure of Catena programs using production rules. We implement a context-free grammar that handles type declarations (`type`), function definitions (`transform`), pattern matching expressions, let bindings, and composition operators. The grammar must be unambiguous and support operator precedence correctly (e.g., function application binds tighter than `|>`). We use LALR parsing with shift/reduce conflict resolution or PEG parsing with ordered choice.

- [x] 1.1.2.1 Define grammar production rules for type declarations with ADT constructors and record syntax
- [x] 1.1.2.2 Define grammar production rules for transform definitions with type signatures and pattern clauses
- [x] 1.1.2.3 Define grammar production rules for expressions including composition, application, and let bindings
- [x] 1.1.2.4 Define operator precedence and associativity tables ensuring correct parsing of complex expressions

### 1.1.3 AST Construction
- [x] **Task 1.1.3 Complete**

The Abstract Syntax Tree (AST) is the internal representation of parsed Catena programs. We define appropriate data structures using Erlang records for each syntactic category: module definitions, type declarations, function definitions, patterns, expressions, and literals. The AST preserves source location information for error reporting and should be easy to traverse for subsequent compiler passes. We design the AST to be immutable and use pattern matching for processing, leveraging Erlang's native pattern matching capabilities.

- [x] 1.1.3.1 Define AST node structures for all expression types with source location metadata
- [x] 1.1.3.2 Define AST node structures for pattern forms including guards, or-patterns, and nested patterns
- [x] 1.1.3.3 Define AST node structures for declarations (types, transforms, modules) with visibility annotations
- [x] 1.1.3.4 Implement AST construction functions that build structured trees from parse results

### 1.1.4 Error Recovery and Reporting
- [x] **Task 1.1.4 Complete**

High-quality error messages are essential for developer experience. When syntax errors occur, we provide clear messages indicating what went wrong, where in the source file, and what was expected. Error recovery allows the parser to continue after errors to report multiple issues in one pass. We implement error messages with code snippets, color highlighting, and suggestions for fixes.

- [x] 1.1.4.1 Implement error reporting with source location, line/column numbers, and code context
- [x] 1.1.4.2 Implement panic-mode error recovery for common syntax errors to continue parsing
- [x] 1.1.4.3 Create helpful error messages with suggestions for common mistakes (e.g., missing `end`, unmatched delimiters)
- [x] 1.1.4.4 Add colored terminal output and code snippet formatting for error display

### 1.1.5 Effect Syntax Support
- [x] **Task 1.1.5 Complete**

Support for algebraic effects syntax including effect declarations, effect operations, and effect handlers. This extends the lexer and parser to recognize effect-specific keywords and constructs, enabling Catena's category-theoretic approach to side effects.

- [x] 1.1.5.1 Add `effect`, `operation`, `perform`, `with` keywords to lexer and extend parser with effect declaration grammar producing EffectDecl and EffectOperation AST nodes (success: parse `effect FileIO { operation read(path: String): String }`)
- [x] 1.1.5.2 Add perform expression grammar and AST node PerformExpr enabling effectful operation invocations (success: parse `perform FileIO.read(path)`)
- [x] 1.1.5.3 Add try/with handler syntax and AST nodes TryWithExpr and HandlerCase for effect handlers with pattern matching on operation names (success: parse complete handler blocks with multiple operation cases)
- [x] 1.1.5.4 Add effect annotation syntax using `/` operator for effect sets in type signatures and EffectAnnotation AST node (success: parse `String / {FileIO, Process}` in function signatures)

### 1.1.6 Trait System Syntax
- [x] **Task 1.1.6 Complete**

Add trait system keywords and syntax to demonstrate Catena's category-theory-first approach through general abstraction mechanisms. Traits replace ad-hoc polymorphism with principled type classes that form the foundation of the standard library's category theory abstractions (Setoid, Functor, Monad, etc.).

- [x] 1.1.6.1 Add `trait`, `instance` keywords to lexer for trait system declarations (inheritance uses `:` syntax)
- [x] 1.1.6.2 Add trait declaration grammar with method signatures and default implementations producing TraitDecl AST nodes (success: parse `trait Functor f where fmap : (a -> b) -> f a -> f b`)
- [x] 1.1.6.3 Add instance declaration grammar with method implementations producing InstanceDecl AST nodes (success: parse `instance Functor Maybe where fmap f = match | None -> None | Some x -> Some (f x) end`)
- [x] 1.1.6.4 Add trait hierarchy syntax with `:` for trait inheritance (success: parse `trait Monad m : Applicative m where bind : m a -> (a -> m b) -> m b end`)

### 1.1.5 Core Operators
- [x] **Task 1.1.5 Complete**

Add dual notation operators for category theory abstractions, providing both readable keyword versions and concise symbolic operators. This makes Catena accessible to beginners (using keywords like `equals`, `append`, `bind`) while enabling experts to write terse, mathematical code (using `===`, `<>`, `>>=`).

- [x] 1.1.5.1 Add equality operators `===` and `!==` to lexer for Setoid trait (type class equality)
- [x] 1.1.5.2 Add composition operators: `<>` (Semigroup append), `<$>` (Functor map), `<*>` (Applicative apply)
- [x] 1.1.5.3 Add monadic operators: `>>=` (bind), `>>` (sequence), `=<<` (flipped bind), `>=>` (Kleisli left-to-right), `<=<` (Kleisli right-to-left)
- [x] 1.1.5.4 Define operator precedence and associativity tables ensuring correct parsing (infixl 1 for >>=, infixl 4 for <$> and <*>, infixl 6 for <>)

### Unit Tests - Section 1.1
- [x] **Unit Tests 1.1 Complete**
- [x] Test lexer tokenization of all keywords, operators, delimiters, and literals with edge cases
- [x] Test parser handling of valid Catena programs generating correct ASTs
- [x] Test parser error recovery with intentionally malformed input producing multiple error reports
- [x] Test source location tracking ensuring accurate line/column information in AST nodes
- [x] Test effect syntax parsing for effect declarations, perform expressions, handlers, and effect annotations
- [x] Test nested handler syntax and operation pattern matching with complete coverage
- [x] Test trait system syntax parsing for trait declarations, instance declarations, and trait hierarchies
- [x] Test core operator parsing with correct precedence and associativity (===, <>, <$>, <*>, >>=, >=>)
- [x] Test operator parsing in complex expressions ensuring proper nesting and binding

---

## 1.2 Core Type System
- [x] **Section 1.2 Complete**

The type system is the heart of Catena, providing static guarantees while inferring types automatically. We implement Hindley-Milner type inference using Algorithm W, extended with support for type classes (traits), row polymorphism for extensible records, higher-kinded types for functors, and **algebraic effects tracking**. This becomes a **type-and-effect system** where function signatures include both their result type and the set of effects they may perform. The type system ensures that well-typed programs cannot go wrong while minimizing type annotations required from programmers. Type errors must be clear and localized, pointing to the exact source of type mismatches or unhandled effects.

**Effect System for PoC**: Effect tracking is monomorphic (no effect variables or polymorphism). Effects are tracked alongside types during inference, perform operations introduce effects, and handlers resolve effects. Full effect polymorphism and advanced inference are deferred to Phase 6.

### 1.2.1 Type Representation
- [x] **Task 1.2.1 Complete**

We define the internal representation of types, including type variables (α, β, γ), type constructors (`List`, `Maybe`, `Process`), function types (τ₁ -> τ₂), record types, and variant types. Type schemes represent polymorphic types with forall quantification. **We extend this with effect sets** to create a type-and-effect system where function types include effect annotations. We implement type equality checking, substitution operations, and pretty-printing for type expressions used in error messages.

- [x] 1.2.1.1 Define type term representation with type variables, constructors, functions, records, and variants
- [x] 1.2.1.2 Implement type substitution operations for unification and instantiation
- [x] 1.2.1.3 Implement type scheme representation for polymorphic types with quantified variables
- [x] 1.2.1.4 Implement type pretty-printing for human-readable error messages and REPL output
- [x] 1.2.1.5 Define EffectSet type as `{effect_set, [EffectName]}` and extend function types to `{fun_type, Param, Return, EffectSet}` where empty set `{}` represents pure functions (success: can represent and manipulate type-and-effect signatures)

### 1.2.2 Algorithm W Implementation
- [x] **Task 1.2.2 Complete**

Algorithm W is the standard approach to Hindley-Milner type inference, combining constraint generation with unification. We traverse the AST, generating type constraints, then solve them using Robinson's unification algorithm with occurs check. The algorithm infers the most general (principal) type for each expression. We handle let-polymorphism correctly, allowing generalization only at let bindings. **We extend Algorithm W to track effects alongside types**, propagating effect sets through the AST.

- [x] 1.2.2.1 Implement constraint generation traversing AST and collecting type equations
- [x] 1.2.2.2 Implement unification algorithm with occurs check preventing infinite types
- [x] 1.2.2.3 Implement type generalization for let bindings introducing forall quantifiers
- [x] 1.2.2.4 Implement type instantiation for polymorphic function applications
- [x] 1.2.2.5 Track effect annotations during type inference where perform operations introduce effects into function signatures, function application propagates effect sets via union, and PoC limitation allows only monomorphic effects without effect variables (defer polymorphism to Phase 6)

### 1.2.3 Constraint Solving
- [x] **Task 1.2.3 Complete**

Beyond simple unification, we need constraint solving for type classes (traits like `Functor`, `Monad`, `Ord`). When a function uses operations from a trait, we generate trait constraints that must be satisfied. Constraint solving searches for trait instances and resolves ambiguous type variables. We implement instance resolution with backtracking and check for coherence (no overlapping instances). **We also add effect handler checking** to verify that try/with blocks correctly handle declared effects.

- [x] 1.2.3.1 Implement trait constraint representation and generation from trait-polymorphic functions
- [x] 1.2.3.2 Implement instance resolution searching trait instances and unifying with constraints
- [x] 1.2.3.3 Implement constraint simplification reducing complex constraints to canonical form
- [x] 1.2.3.4 Implement coherence checking ensuring unique instance resolution without ambiguity
- [x] 1.2.3.5 Verify effect handlers match declared effect operations, check handler exhaustiveness ensuring all operations covered, and resolve effects when handled by removing them from effect set (success: type-check simple handler blocks correctly)

### 1.2.4 Error Messages
- [x] **Task 1.2.4 Complete**

Type errors are among the most common errors developers encounter. We provide clear, actionable error messages that explain type mismatches, suggest fixes, and show the chain of reasoning that led to the error. For unification failures, we show both types and highlight the incompatible parts. For missing instances, we suggest which trait implementations are needed.

- [x] 1.2.4.1 Implement type error formatting showing expected vs actual types with highlighting
- [x] 1.2.4.2 Implement type error localization tracking error sources through AST locations (partial - formatter ready)
- [x] 1.2.4.3 Implement error explanation providing context for common type errors with suggestions
- [x] 1.2.4.4 Implement error recovery attempting to continue type checking after errors to report multiple issues (partial - design complete)

### 1.2.5 Effect-Specific Error Messages
- [x] **Task 1.2.5 Complete**

Effect system errors require specialized messages to help developers understand which effects are unhandled, which handler operations are missing or mismatched, and where effects were introduced. We provide clear explanations of effect requirements and handler mismatches.

- [x] 1.2.5.1 Implement unhandled effect errors showing "Unhandled effect E in function f" with perform site location where effect was introduced
- [x] 1.2.5.2 Implement handler mismatch errors for incorrect arities ("Handler for E.operation expects N arguments, got M") and missing operations ("Missing handler for operation E.op")
- [x] 1.2.5.3 Implement effect annotation errors for mismatched effect sets between declared and inferred effects
- [x] 1.2.5.4 Implement effect context explanation showing effect propagation chain from perform through callers

### 1.2.6 Trait Constraint System
- [x] **Task 1.2.6 Complete**

Extend the type system to handle trait hierarchies, instance resolution, and trait method type inference. This completes the foundation for Catena's category-theory-first approach by making traits the general abstraction mechanism for all polymorphism (replacing ad-hoc overloading with principled type classes).

- [x] 1.2.6.1 Implement trait hierarchy checking ensuring extends relationships form valid DAG without cycles
- [x] 1.2.6.2 Implement instance search and resolution finding matching instances for trait constraints during type checking
- [x] 1.2.6.3 Implement trait method type inference checking method signatures match trait declarations and inferring implementation types
- [x] 1.2.6.4 Implement coherence checking detecting overlapping instances and ensuring unique resolution (no ambiguous instances)

### Unit Tests - Section 1.2
- [x] **Unit Tests 1.2 Complete**
- [x] Test type inference for simple expressions inferring correct types without annotations
- [x] Test type inference for polymorphic functions with proper generalization and instantiation
- [x] Test type checking catching type errors with clear error messages
- [x] Test trait constraint solving resolving instances correctly and detecting missing instances
- [x] Test trait hierarchy checking detecting cycles and validating extends relationships
- [x] Test instance resolution finding correct instances for trait constraints
- [x] Test coherence checking detecting overlapping instances
- [x] Test effect tracking in type inference correctly propagating effect sets through expressions
- [x] Test effect handler checking detecting unhandled effects and missing handler operations
- [x] Test type error formatting with highlighting and explanations

---

## 1.3 Core Erlang Code Generation
- [x] **Section 1.3 Complete**

After parsing and type checking, we generate Core Erlang code that executes on the BEAM VM. Core Erlang is a simplified functional language that serves as an intermediate representation for all BEAM languages. We translate Catena's typed AST into Core Erlang expressions, compiling shapes to tagged tuples, flows to functions, and pattern matching to case expressions. The generated code must preserve Catena semantics while leveraging BEAM optimizations.

### 1.3.1 Expression Translation
- [x] **Task 1.3.1 Complete**

We translate Catena expressions to equivalent Core Erlang expressions. Function applications become Core Erlang calls, let bindings become Core Erlang let expressions, and composition operators become function call chains. **Effect operations (perform and try/with) translate to process-based message passing.** We must handle tail-call optimization correctly, ensuring recursive functions don't overflow the stack. Literals translate directly, and variables map to Core Erlang variables.

- [x] 1.3.1.1 Implement translation of function applications to Core Erlang call expressions
- [x] 1.3.1.2 Implement translation of let bindings to Core Erlang let expressions with proper scoping
- [x] 1.3.1.3 Implement translation of composition operators (|>) to nested function calls
- [x] 1.3.1.4 Implement translation of literals (numbers, strings, atoms) to Core Erlang constants
- [x] 1.3.1.5 Implement translation of perform operations to process send/receive messages and try/with handlers to process spawning and message handling (success: generate Core Erlang that executes effects via BEAM processes)

### 1.3.2 Pattern Compilation
- [x] **Task 1.3.2 Complete**

Pattern matching is central to functional programming. We compile Catena patterns to Core Erlang case expressions using decision tree algorithms. This involves converting high-level patterns (guards, or-patterns, as-patterns) into primitive match operations. We optimize pattern matching order to minimize runtime checks and detect unreachable branches.

- [x] 1.3.2.1 Implement basic pattern compilation for constructors, variables, and wildcards
- [x] 1.3.2.2 Implement guard compilation translating guard expressions to Core Erlang conditions
- [x] 1.3.2.3 Implement decision tree generation optimizing pattern match ordering for efficiency
- [x] 1.3.2.4 Implement exhaustiveness checking warning about non-exhaustive patterns at compile time

### 1.3.3 Type Erasure
- [x] **Task 1.3.3 Complete**

Catena's rich type system exists only at compile time. At runtime, BEAM operates on untyped terms. Type erasure removes all type information from the AST, leaving only computational content. Polymorphic functions use uniform representation with runtime type information where necessary, and type classes disappear after instance resolution through dictionary-passing transformation. We ensure that erasure preserves program semantics—well-typed programs don't change behavior.

- [x] 1.3.3.1 Implement type annotation removal stripping all type information from expressions
- [x] 1.3.3.2 Implement trait dictionary passing for type classes converting to explicit parameters
- [x] 1.3.3.3 Implement polymorphism handling through monomorphization or uniform representation
- [x] 1.3.3.4 Verify semantic preservation ensuring erased code has same behavior as typed code

### 1.3.4 Module Generation
- [x] **Task 1.3.4 Complete**

Each Catena module compiles to a BEAM module with exports, imports, and module attributes. We generate module metadata including function signatures and documentation. Private functions are excluded from exports. Module names follow Erlang naming conventions, potentially using namespaces. We emit `.core` files that erlc compiles to `.beam` bytecode.

- [x] 1.3.4.1 Implement module structure generation with module name, exports, and attributes
- [x] 1.3.4.2 Implement function compilation generating Core Erlang function definitions with arities
- [x] 1.3.4.3 Implement export list generation including only public functions with correct arities
- [x] 1.3.4.4 Implement Core Erlang file output writing valid .core files that erlc can compile

### 1.3.5 Effect Runtime System
- [x] **Task 1.3.5 Complete**

The effect runtime implements process-based effect handlers leveraging BEAM's lightweight processes. Each try/with handler spawns a handler process that receives perform messages, executes handler operations, and sends results back. This provides true effect isolation and leverages BEAM's strengths in concurrency and message passing. **PoC implements minimal runtime with IO and Process effects only.**

- [x] 1.3.5.1 Implement handler process spawning where try/with blocks spawn handler process with operation implementations and register for effect operations
- [x] 1.3.5.2 Implement perform operation compilation translating perform to send message to handler process and receive result with timeout handling
- [x] 1.3.5.3 Implement effect message protocol using tagged tuples `{perform, EffectName, Operation, Args, ReplyPid}` and `{effect_result, Value}` for bidirectional communication
- [x] 1.3.5.4 Implement builtin IO effect handler providing readFile, writeFile, print operations as baseline demonstrating process-based effect handling (success: can execute simple programs with IO effects)

### Unit Tests - Section 1.3
- [x] **Unit Tests 1.3 Complete**
- [x] Test expression translation generating correct Core Erlang for all expression forms
- [x] Test pattern compilation producing optimal decision trees with exhaustiveness checking
- [x] Test type erasure preserving semantics while removing all type information
- [x] Test module generation producing valid .core files that compile to working .beam modules
- [x] Test effect runtime system spawning handler processes and routing perform messages correctly
- [x] Test builtin IO effect handler executing file operations and returning results via message passing
- [x] Test effect translation generating correct Core Erlang for perform operations and try/with handlers

---

## 1.4 Integration Tests
- [x] **Section 1.4 Complete**

Integration tests validate the entire compilation pipeline from source to executable bytecode. We test realistic Catena programs that exercise multiple compiler phases together, ensuring that the lexer, parser, type checker, and code generator work cohesively. These tests catch interaction bugs that unit tests miss and validate that generated BEAM modules execute correctly.

### 1.4.1 End-to-End Compilation
- [x] **Task 1.4.1 Complete**

End-to-end tests compile complete Catena programs and verify they produce correct results. We test examples like factorial, fibonacci, list operations, and simple recursive functions. Each test compiles the source, loads the generated .beam module, calls exported functions, and checks outputs against expected values.

- [x] 1.4.1.1 Test compilation and execution of simple arithmetic expressions returning correct results
- [x] 1.4.1.2 Test compilation and execution of recursive functions (factorial, fibonacci) with correct outputs
- [x] 1.4.1.3 Test compilation and execution of polymorphic functions with different type instantiations
- [x] 1.4.1.4 Test compilation and execution of pattern matching code with multiple clauses and guards

### 1.4.2 BEAM Bytecode Validation
- [x] **Task 1.4.2 Complete**

We validate that generated .beam files are well-formed and meet BEAM VM requirements. This includes checking module exports match declarations, function arities are correct, and bytecode passes BEAM validation. We use tools like `beam_lib` to inspect generated modules and verify metadata.

- [x] 1.4.2.1 Verify generated .beam files load without errors using Erlang's code loader
- [x] 1.4.2.2 Verify module exports match source declarations with correct function names and arities
- [x] 1.4.2.3 Verify function calls use correct module-qualified names and argument counts
- [x] 1.4.2.4 Verify pattern matching compiles to efficient BEAM select_val instructions where applicable

### 1.4.3 Error Handling Pipeline
- [x] **Task 1.4.3 Complete**

We test that errors at each compiler stage are caught and reported appropriately. Syntax errors produce helpful parser messages, type errors show clear type mismatches, and code generation errors indicate internal compiler issues. Error messages include source locations and suggestions for fixes.

- [x] 1.4.3.1 Test syntax error reporting with multiple intentional syntax errors showing clear messages
- [x] 1.4.3.2 Test type error reporting with type mismatches showing expected and actual types
- [x] 1.4.3.3 Test exhaustiveness warnings for non-exhaustive pattern matches
- [x] 1.4.3.4 Test error recovery allowing compiler to report multiple errors in single pass

### 1.4.4 Effect System Integration
- [x] **Task 1.4.4 Complete**

End-to-end testing of the algebraic effect system from parsing through execution. We validate that effect declarations, perform operations, and try/with handlers work cohesively, that effect tracking catches unhandled effects at compile time, and that the process-based runtime correctly executes effectful programs.

- [x] 1.4.4.1 Test compilation and execution of simple effectful programs using IO effect (file read/write operations) producing correct results
- [x] 1.4.4.2 Test effect type checking catching unhandled effects with clear error messages pointing to perform sites
- [x] 1.4.4.3 Test handler exhaustiveness checking detecting missing operation handlers and reporting them clearly
- [x] 1.4.4.4 Test process-based effect runtime spawning handlers, routing messages, and returning results correctly without process leaks

---

---

## 1.5 Standard Library Validation
- [ ] **Section 1.5 Complete**

This section validates that Catena's standard library compiles to BEAM and executes correctly. The standard library (`lib/catena/stdlib/`) contains the category theory traits (Comparable, Combiner, Accumulator, Mapper, Pipeline), testing framework, and effect modules. These are written in Catena itself, demonstrating that the compiler can handle real-world code. We validate trait/instance resolution, higher-kinded types, law verification through the Test module, and do-notation desugaring.

**Architectural Note**: Per Catena's minimal-core design, category theory abstractions are library code, not compiler features. The compiler provides `trait`, `instance`, and `effect` keywords; the standard library builds all abstractions on top.

### 1.5.1 Standard Library Compilation
- [x] **Task 1.5.1 Complete**

Validate that all standard library files parse, type-check, and compile to BEAM bytecode. This proves the compiler can handle real Catena code with traits, instances, and effects.

- [x] 1.5.1.1 Parse `lib/catena/stdlib/prelude.cat` successfully producing valid AST for all trait and instance declarations
- [x] 1.5.1.2 Type-check prelude.cat validating all trait method signatures and instance implementations
- [x] 1.5.1.3 Parse and type-check `lib/catena/stdlib/test.cat` for testing framework
- [x] 1.5.1.4 Parse and type-check effect modules (`effect/io.cat`, `effect/state.cat`, `effect/error.cat`)
- [x] 1.5.1.5 Generate Core Erlang and compile to .beam for all stdlib modules

### 1.5.2 Trait Instance Resolution
- [x] **Task 1.5.2 Complete**

Validate that trait instances resolve correctly during type checking. The prelude defines instances for Maybe, Either, List for various traits (Mapper, Applicator, Chainable, Pipeline, Comparable, etc.).

- [x] 1.5.2.1 Resolve Mapper instance for Maybe when type-checking `map f (Some x)`
- [x] 1.5.2.2 Resolve Mapper instance for List when type-checking `map f [1, 2, 3]`
- [x] 1.5.2.3 Resolve constrained instances like `Comparable a => Comparable (List a)` with nested resolution
- [x] 1.5.2.4 Detect and report missing instances with clear error messages
- [x] 1.5.2.5 Verify trait hierarchy resolution (Pipeline requires Applicator requires Mapper)

### 1.5.3 Higher-Kinded Type Validation
- [x] **Task 1.5.3 Complete**

Validate that higher-kinded types work correctly for traits like Mapper and Pipeline. These traits are parameterized by type constructors (`f : Type -> Type`), not simple types.

- [x] 1.5.3.1 Validate kind checking for `trait Mapper f where map : (a -> b) -> f a -> f b`
- [x] 1.5.3.2 Validate kind inference for instance declarations like `instance Mapper Maybe`
- [x] 1.5.3.3 Validate partially applied type constructors like `instance Mapper (Either e)`
- [x] 1.5.3.4 Report kind errors clearly when HKT constraints are violated

### 1.5.4 Law Verification via Test Module
- [ ] **Task 1.5.4 Complete**

Validate that the Test module can express and verify trait laws through property-based testing. Laws are library code using `Test.property`, not special syntax.

- [ ] 1.5.4.1 Compile law verification code using `Test.property` for Mapper identity law
- [ ] 1.5.4.2 Compile law verification code for Mapper composition law
- [ ] 1.5.4.3 Compile law verification code for Pipeline monad laws (left/right identity, associativity)
- [ ] 1.5.4.4 Execute law tests and verify they can detect both passing and failing properties
- [ ] 1.5.4.5 Integrate with PropEr for random test case generation

### 1.5.5 Do-Notation Desugaring
- [ ] **Task 1.5.5 Complete**

Implement do-notation as syntactic sugar that desugars to `>>=` (bind) chains. This is a compiler transformation, not a keyword—the parser recognizes `do` blocks and transforms them during compilation.

- [ ] 1.5.5.1 Parse do-block syntax: `do { x <- ma; y <- mb; pure (x + y) }`
- [ ] 1.5.5.2 Desugar bind: `x <- ma; rest` becomes `ma >>= (fn x -> rest)`
- [ ] 1.5.5.3 Desugar sequence: `ma; rest` becomes `ma >> rest`
- [ ] 1.5.5.4 Desugar let in do: `let x = e; rest` becomes `let x = e in rest`
- [ ] 1.5.5.5 Verify desugared code type-checks with correct Pipeline constraint inference

### 1.5.6 Effect Integration with Kleisli Arrows
- [ ] **Task 1.5.6 Complete**

Validate that effects integrate correctly with category theory through Kleisli composition. Effectful functions compose with effect set union.

- [ ] 1.5.6.1 Type-check Kleisli composition `>=>` with effect tracking: `(a -> b / ε₁) >=> (b -> c / ε₂) : a -> c / (ε₁ ∪ ε₂)`
- [ ] 1.5.6.2 Validate that `perform` operations introduce effects into function signatures
- [ ] 1.5.6.3 Validate that `handle` blocks remove effects from the effect set
- [ ] 1.5.6.4 Compile and execute effectful Kleisli composition example

### 1.5.7 Operator Desugaring
- [ ] **Task 1.5.7 Complete**

Validate that category theory operators desugar to trait method calls. Operators like `<$>`, `>>=`, `<>` are syntactic sugar for Mapper.map, Pipeline.chain, Combiner.combine.

- [ ] 1.5.7.1 Desugar `<$>` to `Mapper.map` calls during compilation
- [ ] 1.5.7.2 Desugar `>>=` to `Chainable.chain` calls
- [ ] 1.5.7.3 Desugar `<>` to `Combiner.combine` calls
- [ ] 1.5.7.4 Desugar `===` and `!==` to `Comparable.equals` and derived `notEquals`
- [ ] 1.5.7.5 Verify desugared operators resolve correct trait instances

### Unit Tests - Section 1.5
- [ ] **Unit Tests 1.5 Complete**
- [ ] Test parsing of all stdlib files produces valid ASTs
- [ ] Test type-checking of prelude.cat with all traits and instances
- [ ] Test trait instance resolution for Mapper, Applicator, Pipeline on Maybe, List, Either
- [ ] Test constrained instance resolution with nested constraints
- [ ] Test higher-kinded type kind checking and inference
- [ ] Test do-notation desugaring produces correct bind chains
- [ ] Test operator desugaring resolves correct trait methods
- [ ] Test effect tracking through Kleisli composition

### Integration Tests - Section 1.5
- [ ] **Integration Tests 1.5 Complete**
- [ ] Compile and execute: `map (fn x -> x + 1) [1, 2, 3]` produces `[2, 3, 4]`
- [ ] Compile and execute: `Some 5 >>= (fn x -> Some (x + 1))` produces `Some 6`
- [ ] Compile and execute: `[1, 2] <> [3, 4]` produces `[1, 2, 3, 4]`
- [ ] Compile and execute do-notation with Maybe producing correct result
- [ ] Compile and execute law verification tests using Test.property
- [ ] Compile and execute effectful Kleisli composition with correct effect handling
- [ ] Load compiled stdlib modules and call exported functions from Erlang

---

## Success Criteria

1. **Lexer and Parser**: Successfully parse valid Catena programs including effect syntax and trait system into ASTs with error recovery for malformed input
2. **Type-and-Effect Inference**: Correctly infer types and track effects for polymorphic functions, detecting both type errors and unhandled effects with clear messages
3. **Code Generation**: Generate valid Core Erlang that compiles to working .beam modules including process-based effect runtime
4. **Integration**: Compile and run example programs (factorial, list processing, simple IO effects) producing correct outputs
5. **Effect System**: Parse effect declarations, track effects through inference, and execute effectful programs via process-based handlers
6. **Standard Library Validation**:
   - Parse and type-check all stdlib files (prelude.cat, test.cat, effect modules)
   - Trait instance resolution works for Mapper, Applicator, Pipeline on Maybe, List, Either
   - Higher-kinded types work correctly for Mapper and Pipeline traits
   - Do-notation desugars to `>>=` chains with correct type inference
   - Operators (`<$>`, `>>=`, `<>`, `===`) desugar to trait method calls
   - Law verification via Test.property compiles and executes
   - Kleisli composition tracks effect set union correctly
7. **Error Messages**: Provide helpful, localized error messages for syntax, type, effect, and trait errors
8. **Test Coverage**: 85% test coverage with comprehensive unit and integration tests including effect system and stdlib validation

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 2**: REPL and standard library requiring working compilation, type-and-effect inference, and effect execution
- **Phase 3**: Advanced pattern matching building on pattern compilation infrastructure
- **Phase 4**: Module system extending import/export mechanisms and effect propagation across modules
- **Phase 5**: Actor model integration leveraging effect runtime infrastructure and compiling actor syntax to OTP behaviors
- **Phase 6**: Advanced effect features building on minimal viable effect system (polymorphism, optimization, expanded standard library)

## Key Outputs

- Lexer and parser producing structured ASTs from Catena source code including effect syntax
- Type-and-effect inference engine supporting Hindley-Milner with traits, row polymorphism, and monomorphic effect tracking
- Process-based effect runtime leveraging BEAM processes for effect handler execution
- Core Erlang code generator producing valid .beam modules with effect compilation
- Compiled standard library modules (prelude, test, effects) as .beam files
- Do-notation and operator desugaring transformations
- Comprehensive test suite covering all compiler phases including stdlib validation
- Error reporting infrastructure with source locations and helpful messages for type and effect errors
- Working compilation pipeline from source to executable bytecode with effect support
