# Phase 3: Pattern Matching Engine

## Overview

This phase implements Catena's advanced pattern matching system, going beyond basic constructor matching to include guards, or-patterns, nested patterns, and as-patterns. Pattern matching is fundamental to functional programming, providing both data destructuring and control flow. We extend the basic pattern compilation from Phase 1 into a sophisticated system that handles complex patterns efficiently while maintaining categorical semantics—patterns are morphisms in the category of data types.

The pattern compiler transforms high-level patterns into efficient decision trees that minimize runtime checks. We implement exhaustiveness checking to warn when patterns don't cover all cases, and redundancy detection to identify unreachable clauses. This phase also adds pattern guards that allow boolean conditions within patterns, enabling more expressive case analysis. **Pattern guards must be pure** (effect-free) in the PoC—effectful guards are deferred to Phase 6.

This phase runs for **3.5 weeks** and focuses on correctness, completeness, and helpful error messages. By the end, Catena will support pattern matching comparable to ML-family languages, with strong static guarantees and optimal runtime performance.

---

## 3.1 Advanced Pattern Features
- [ ] **Section 3.1 Complete**

Advanced patterns go beyond simple constructor matching to include guards (boolean predicates), or-patterns (multiple patterns in one clause), nested patterns (patterns within patterns), and as-patterns (naming subpatterns). These features dramatically increase pattern expressiveness, allowing concise and readable code for complex data destructuring. We implement each feature separately then combine them into a unified pattern matching system that interacts correctly.

### 3.1.1 Pattern Guards
- [ ] **Task 3.1.1 Complete**

Guards extend patterns with boolean conditions evaluated after successful pattern match. Syntax: `| pattern when condition -> expression`. Guards enable filtering based on computed properties of matched values, like `| (x, y) when x > y -> ...`. Multiple guards on the same pattern create independent alternatives. **Guards must be pure** (no effects) in the PoC—we enforce this through effect checking, rejecting guards with non-empty effect sets.

- [ ] 3.1.1.1 Implement guard syntax parsing extending pattern clauses with `when` keyword and boolean expressions
- [ ] 3.1.1.2 Implement guard evaluation in pattern compiler generating conditional tests after pattern binding
- [ ] 3.1.1.3 Implement guard failure handling causing pattern to fail and try next clause
- [ ] 3.1.1.4 Implement effect checking for guard expressions ensuring guards have empty effect set (pure only, effectful guards deferred to Phase 6)

### 3.1.2 Or-Patterns
- [ ] **Task 3.1.2 Complete**

Or-patterns allow multiple alternatives within a single pattern clause using `|`. Example: `| Red | Green | Blue -> "color"` matches any of three constructors with one action. Or-patterns must bind the same variables in all alternatives (checked statically). They compile to shared code with multiple entry points in the decision tree.

- [ ] 3.1.2.1 Implement or-pattern syntax parsing supporting `pattern1 | pattern2 | pattern3` notation
- [ ] 3.1.2.2 Implement variable consistency checking ensuring all alternatives bind same variables with same types
- [ ] 3.1.2.3 Implement or-pattern compilation generating efficient shared code for common right-hand sides
- [ ] 3.1.2.4 Implement or-pattern pretty-printing for error messages and documentation

### 3.1.3 Nested Patterns
- [ ] **Task 3.1.3 Complete**

Nested patterns destructure data recursively in a single pattern. Example: `| Cons (Some x) xs -> ...` matches a list whose head is a Some value. Nesting depth is arbitrary—patterns can nest as deeply as types allow. The compiler flattens nested patterns into a sequence of tests, preserving the intuitive nested structure in source code.

- [ ] 3.1.3.1 Implement nested pattern parsing supporting arbitrary nesting depth with correct precedence
- [ ] 3.1.3.2 Implement nested pattern type checking verifying inner patterns match nested type structure
- [ ] 3.1.3.3 Implement nested pattern compilation flattening to sequential tests in decision tree
- [ ] 3.1.3.4 Implement optimizations for deeply nested patterns avoiding redundant tests

### 3.1.4 As-Patterns
- [ ] **Task 3.1.4 Complete**

As-patterns bind names to both the whole matched value and its destructured parts. Syntax: `| Cons x xs as list -> ...` binds `x`, `xs`, and `list`. This avoids reconstructing values when you need both the parts and the whole. As-patterns are pure syntactic sugar, desugaring to nested patterns with additional bindings.

- [ ] 3.1.4.1 Implement as-pattern syntax parsing with `pattern as identifier` form
- [ ] 3.1.4.2 Implement as-pattern variable binding ensuring both subpattern and whole value are bound
- [ ] 3.1.4.3 Implement as-pattern type inference assigning correct types to all bound variables
- [ ] 3.1.4.4 Implement as-pattern compilation generating efficient code reusing matched value

### Unit Tests - Section 3.1
- [ ] **Unit Tests 3.1 Complete**
- [ ] Test pattern guards with various boolean conditions succeeding and failing appropriately
- [ ] Test guard purity checking rejecting guards with non-empty effect sets
- [ ] Test or-patterns with multiple alternatives ensuring correct matching and variable binding
- [ ] Test nested patterns at various depths destructuring complex data structures correctly
- [ ] Test as-patterns binding both whole values and parts with correct types

---

## 3.2 Pattern Compilation Strategy
- [ ] **Section 3.2 Complete**

The pattern compiler transforms high-level patterns into efficient low-level code. We use decision tree algorithms that minimize the number of runtime tests required to match a pattern. The compiler reorders tests, shares common prefixes, and eliminates redundant checks. This section implements the core compilation algorithm plus optimizations that leverage BEAM's pattern matching strengths.

### 3.2.1 Decision Tree Generation
- [ ] **Task 3.2.1 Complete**

Decision trees represent pattern matching as a tree of tests. Each node tests one aspect of the input (constructor tag, field value, boolean condition). Leaves contain action expressions. The algorithm builds trees by picking test order heuristically—test the most discriminating positions first. We use occurrence counting to identify shared subtrees and common prefixes.

- [ ] 3.2.1.1 Implement decision tree data structure with test nodes, leaf nodes, and failure branches
- [ ] 3.2.1.2 Implement tree construction algorithm choosing optimal test order based on heuristics
- [ ] 3.2.1.3 Implement occurrence analysis identifying common subpatterns for sharing
- [ ] 3.2.1.4 Implement tree balancing minimizing average path length through tree

### 3.2.2 Code Generation Optimization
- [ ] **Task 3.2.2 Complete**

From decision trees, we generate Core Erlang case expressions. Optimizations include sharing branches with identical code, using select_val instructions for tag tests, and eliminating unreachable branches. We also optimize for common patterns like head/tail list matching and record field access, leveraging BEAM's specialized instructions.

- [ ] 3.2.2.1 Implement Core Erlang case generation from decision trees with optimal instruction selection
- [ ] 3.2.2.2 Implement branch sharing identifying identical subtrees and generating shared code
- [ ] 3.2.2.3 Implement select_val generation for tag tests using BEAM's optimized jump tables
- [ ] 3.2.2.4 Implement peephole optimizations for common pattern sequences (list cons, record access)

### 3.2.3 Test Ordering Heuristics
- [ ] **Task 3.2.3 Complete**

Test order dramatically affects performance. We use heuristics: test constructor tags before guards (tags are cheaper), test positions that eliminate most patterns first, test simple patterns before complex ones. We implement cost models estimating test expense (constructor check < guard evaluation < nested match) and benefit (how many patterns eliminated).

- [ ] 3.2.3.1 Implement cost model estimating runtime expense of different test types
- [ ] 3.2.3.2 Implement benefit analysis calculating how many patterns each test eliminates
- [ ] 3.2.3.3 Implement greedy test selection choosing highest benefit/cost ratio at each step
- [ ] 3.2.3.4 Implement backtracking search for optimal test order when greedy fails

### 3.2.4 Failure Compilation
- [ ] **Task 3.2.4 Complete**

When all patterns fail, we generate appropriate error handling. For exhaustive matches (checked statically), failure is impossible—we prove all cases covered. For non-exhaustive matches, we generate runtime failures with helpful messages indicating the unmatched value. We optionally compile to lenient mode where missing patterns return a default value.

- [ ] 3.2.4.1 Implement match failure code generation with descriptive error messages
- [ ] 3.2.4.2 Implement lenient mode supporting default values for non-exhaustive matches
- [ ] 3.2.4.3 Implement match failure value inclusion showing what value failed to match in error
- [ ] 3.2.4.4 Implement stack trace preservation ensuring match failures include source locations

### Unit Tests - Section 3.2
- [ ] **Unit Tests 3.2 Complete**
- [ ] Test decision tree generation producing optimal trees for various pattern sets
- [ ] Test code generation emitting efficient Core Erlang with correct semantics
- [ ] Test optimization heuristics improving generated code performance measurably
- [ ] Test failure handling generating appropriate errors for non-exhaustive matches

---

## 3.3 Exhaustiveness and Redundancy Checking
- [ ] **Section 3.3 Complete**

Static analysis ensures pattern matches are safe and complete. Exhaustiveness checking verifies that every possible input matches some pattern—preventing runtime crashes from unhandled cases. Redundancy detection finds unreachable patterns—code that can never execute because earlier patterns subsume it. Both checks improve code quality and help developers catch logical errors.

### 3.3.1 Exhaustiveness Analysis
- [ ] **Task 3.3.1 Complete**

Exhaustiveness analysis constructs the complement of the matched pattern space, showing what values aren't covered. For algebraic data types, we generate example missing patterns. For guards, we use SMT solving to check satisfiability. The analysis must handle recursive types, polymorphic patterns, and infinite types like lists. Warnings include concrete examples of unmatched values.

- [ ] 3.3.1.1 Implement pattern coverage calculation building set of covered values from pattern list
- [ ] 3.3.1.2 Implement complement construction finding values not covered by any pattern
- [ ] 3.3.1.3 Implement example generation creating concrete values showing missing cases
- [ ] 3.3.1.4 Implement polymorphic exhaustiveness checking handling type variables and constraints

### 3.3.2 Redundancy Detection
- [ ] **Task 3.3.2 Complete**

Redundancy detection identifies patterns subsumed by earlier patterns. Pattern P is redundant if all values matching P also match some earlier pattern. We use subsumption checking: P₁ subsumes P₂ if P₂'s coverage is a subset of P₁'s. Redundant patterns receive warnings with explanations of which earlier pattern subsumes them.

- [ ] 3.3.2.1 Implement subsumption checking determining if one pattern completely covers another
- [ ] 3.3.2.2 Implement redundancy scanning checking each pattern against all earlier patterns
- [ ] 3.3.2.3 Implement redundancy reporting showing which pattern makes another unreachable
- [ ] 3.3.2.4 Implement partial overlap detection warning about patterns that overlap but neither subsumes the other

### 3.3.3 Warning Generation
- [ ] **Task 3.3.3 Complete**

Warnings must be clear, actionable, and well-localized. For missing patterns, we show concrete examples of unmatched values and suggest patterns to add. For redundant patterns, we highlight the redundant clause and indicate which earlier pattern makes it unreachable. Warnings include source locations, code snippets, and suggestions for fixes.

- [ ] 3.3.3.1 Implement warning message formatting with source locations and code snippets
- [ ] 3.3.3.2 Implement missing pattern suggestions generating patterns that cover gaps
- [ ] 3.3.3.3 Implement redundancy explanations showing subsumption relationships clearly
- [ ] 3.3.3.4 Implement warning suppression allowing developers to silence specific warnings when justified

### 3.3.4 SMT Integration for Guards
- [ ] **Task 3.3.4 Complete**

Guard exhaustiveness requires reasoning about boolean expressions and arithmetic. We integrate an SMT solver (Z3) to check if guard combinations cover all cases. For example, checking that `| _ when x > 0`, `| _ when x < 0`, `| _ when x == 0` is exhaustive. SMT integration is optional—falling back to conservative approximation if unavailable.

- [ ] 3.3.4.1 Implement SMT solver integration translating guards to SMT-LIB format
- [ ] 3.3.4.2 Implement satisfiability checking verifying guard complement is unsatisfiable for exhaustive matches
- [ ] 3.3.4.3 Implement timeout handling ensuring SMT checks don't block compilation indefinitely
- [ ] 3.3.4.4 Implement fallback strategy providing conservative analysis when SMT unavailable

### Unit Tests - Section 3.3
- [ ] **Unit Tests 3.3 Complete**
- [ ] Test exhaustiveness checking detecting missing patterns and suggesting additions
- [ ] Test redundancy detection finding subsumed patterns and explaining subsumption
- [ ] Test warning messages ensuring clarity, accuracy, and helpful suggestions
- [ ] Test guard exhaustiveness checking with SMT solver (when available) and fallback

---

## 3.4 Integration Tests
- [ ] **Section 3.4 Complete**

Integration tests validate that all pattern matching features work together in realistic programs. We test complex pattern combinations (nested patterns with guards and or-patterns), exhaustiveness checking on large pattern sets, and performance on deeply nested matches. These tests ensure the pattern compiler scales to production code.

### 3.4.1 Complex Pattern Programs
- [ ] **Task 3.4.1 Complete**

We write programs using all pattern features together—nested patterns with guards, or-patterns with as-patterns, multiple match expressions in sequence. Examples include expression evaluators, JSON parsers, and tree traversals. These programs must compile correctly, execute efficiently, and receive appropriate warnings when patterns are incomplete.

- [ ] 3.4.1.1 Test expression evaluator using nested patterns and guards for operator precedence
- [ ] 3.4.1.2 Test JSON parser using or-patterns for multiple value types and nested destructuring
- [ ] 3.4.1.3 Test tree algorithms using as-patterns to maintain parent references during traversal
- [ ] 3.4.1.4 Test pattern combinations mixing all advanced features in single match expression

### 3.4.2 Exhaustiveness on Real Code
- [ ] **Task 3.4.2 Complete**

We run exhaustiveness checker on realistic pattern matches from standard libraries and example programs. This validates that checker scales to production code, handles complex types correctly, and produces actionable warnings. We measure false positive rate (warnings on complete matches) and false negative rate (missing warnings on incomplete matches).

- [ ] 3.4.2.1 Test exhaustiveness checking on prelude implementations ensuring no false positives
- [ ] 3.4.2.2 Test exhaustiveness detection on intentionally incomplete matches catching missing cases
- [ ] 3.4.2.3 Test exhaustiveness on recursive types (lists, trees) handling infinite pattern spaces
- [ ] 3.4.2.4 Test exhaustiveness performance on large pattern sets (100+ clauses) completing quickly

### 3.4.3 Performance Benchmarking
- [ ] **Task 3.4.3 Complete**

We benchmark compiled pattern matching against hand-written Core Erlang and Erlang's native patterns. Metrics include instruction count, decision tree depth, and runtime performance on representative inputs. We aim for generated code within 10% of hand-optimized BEAM code, demonstrating that our abstractions don't sacrifice performance.

- [ ] 3.4.3.1 Benchmark constructor matching comparing generated code to hand-written equivalent
- [ ] 3.4.3.2 Benchmark guard evaluation measuring overhead of boolean tests in patterns
- [ ] 3.4.3.3 Benchmark decision tree depth ensuring logarithmic average case for large pattern sets
- [ ] 3.4.3.4 Benchmark overall compilation speed ensuring pattern compilation doesn't dominate build time

---

## Success Criteria

1. **Feature Completeness**: Guards, or-patterns, nested patterns, and as-patterns all working correctly
2. **Exhaustiveness Checking**: Detecting missing patterns with concrete examples and helpful warnings
3. **Redundancy Detection**: Finding unreachable patterns and explaining subsumption relationships
4. **Optimal Code Generation**: Generated pattern matching within 10% performance of hand-written code
5. **Warning Quality**: Clear, localized warnings with actionable suggestions for fixes
6. **Test Coverage**: 85% coverage with comprehensive tests for all pattern features and edge cases

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 4**: Module system using pattern matching in import/export filters
- **Phase 5**: Actor model relying on pattern matching for message handlers
- All future language features using patterns for data destructuring
- Standard library implementations using advanced patterns throughout

## Key Outputs

- Complete implementation of guards, or-patterns, nested patterns, and as-patterns
- Decision tree compiler generating optimal Core Erlang code
- Exhaustiveness checker detecting missing patterns with examples
- Redundancy detector finding unreachable patterns
- SMT integration for guard exhaustiveness (optional)
- Comprehensive pattern matching test suite
- Performance benchmarks validating efficiency
- Documentation of pattern matching semantics and usage patterns
