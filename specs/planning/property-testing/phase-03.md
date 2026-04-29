# Phase 3: Property Testing Framework

## Overview

This phase implements the property testing framework that ties together generators, shrinking, and test execution. We build the property specification DSL (`property` and `forall`), the test runner infrastructure, failure reporting with counterexamples, counterexample minimization via shrinking, and seed-based reproducibility. The goal is a complete, usable property testing system.

By the end of this phase, developers can write property tests using a familiar QuickCheck-style API, run them with automatic shrinking to minimal counterexamples, and reproduce failures deterministically using seeds. The framework integrates with Catena's existing test infrastructure while remaining usable standalone.

**Design Philosophy**: The property DSL should be minimal and expressive. Error messages should be clear and actionable, showing exactly what values caused failure. Reproducibility is paramount - any failure can be reproduced by running with the same seed.

This phase runs for **4 weeks** and focuses on developer experience, ensuring that property testing is accessible and productive.

**Implementation Note - Blockers and Workarounds**: This phase includes macro-based DSL features (`property_test/2` macro in Section 3.2.4) that would ideally be implemented in Catena. However, since Catena's macro system is not yet available, we will implement these features using **Erlang parse transforms or function-based APIs** initially. Specifically:
- The `property/2`, `forall/2`, and related constructs will be implemented as regular Erlang functions
- The `property_test/2` macro will use Erlang's parse_transform mechanism
- Once Catena's macro system is implemented, we can add native Catena syntax as syntactic sugar over the existing function-based API

This approach allows immediate implementation while preserving the option to add ergonomic Catena syntax later.

---

## 3.1 Property Specification DSL
- [ ] **Section 3.1 Complete**

The property DSL provides syntax for defining properties with quantified variables. Properties use `forall` to bind generated values and express universal properties that should hold for all inputs. The DSL integrates with Catena's syntax while supporting standalone usage.

### 3.1.1 Property Type Definition
- [ ] **Task 3.1.1 Complete**

Define the Property type that represents a testable property. Properties encapsulate a generator and a predicate, plus configuration like test count.

- [ ] 3.1.1.1 Define `Property` record with `generator`, `predicate`, and `config` fields
- [ ] 3.1.1.2 Define `PropertyConfig` with `test_count`, `max_shrinks`, `seed` options
- [ ] 3.1.1.3 Implement `property/2` creating a property from name and body
- [ ] 3.1.1.4 Implement default configuration values (100 tests, 1000 shrinks)

### 3.1.2 Forall Syntax
- [ ] **Task 3.1.2 Complete**

Implement the `forall` construct for binding generated values in property bodies. Forall introduces variables bound to generators that can be used in the property predicate.

- [ ] 3.1.2.1 Implement `forall/2` binding a single variable to a generator
- [ ] 3.1.2.2 Implement nested forall for multiple variables
- [ ] 3.1.2.3 Implement `forall/3` with guard conditions (`when` clause)
- [ ] 3.1.2.4 Ensure forall uses applicative combination when variables are independent

### 3.1.3 Implication and Preconditions
- [ ] **Task 3.1.3 Complete**

Implement implication for conditional properties. When a precondition fails, the test case is discarded rather than counted as pass or fail.

- [ ] 3.1.3.1 Implement `==>` operator for implication (precondition ==> property)
- [ ] 3.1.3.2 Implement `implies/2` function form of implication
- [ ] 3.1.3.3 Track discard count and fail if too many discards (> 10x tests)
- [ ] 3.1.3.4 Report discard rate in test output

### 3.1.4 Property Combinators
- [ ] **Task 3.1.4 Complete**

Implement combinators for composing and modifying properties. Enable grouping related properties and adjusting configuration.

- [ ] 3.1.4.1 Implement `property_group/2` for grouping related properties
- [ ] 3.1.4.2 Implement `with_config/2` modifying property configuration
- [ ] 3.1.4.3 Implement `label/2` for labeling test case distribution
- [ ] 3.1.4.4 Implement `classify/3` for conditional labeling

### Unit Tests - Section 3.1
- [ ] **Unit Tests 3.1 Complete**
- [ ] Test property creation with various configurations
- [ ] Test forall binds generator values correctly
- [ ] Test nested forall works with multiple generators
- [ ] Test implication discards failing preconditions
- [ ] Test excessive discards cause test failure
- [ ] Test property groups execute all properties
- [ ] Test labels are tracked and reported

---

## 3.2 Test Runner Infrastructure
- [ ] **Section 3.2 Complete**

The test runner executes properties, generating random test cases, checking the predicate, and shrinking on failure. It manages seed generation, size scaling across tests, and overall test orchestration.

### 3.2.1 Test Execution Engine
- [ ] **Task 3.2.1 Complete**

Implement the core test execution loop that runs a property for the configured number of tests.

- [ ] 3.2.1.1 Implement `run_property/1` executing a property with default config
- [ ] 3.2.1.2 Implement `run_property/2` with custom configuration
- [ ] 3.2.1.3 Implement size scaling across tests (0 to 100, then random)
- [ ] 3.2.1.4 Implement early exit on failure (don't run remaining tests)

### 3.2.2 Result Types
- [ ] **Task 3.2.2 Complete**

Define result types representing property test outcomes: success, failure with counterexample, discarded, or error.

- [ ] 3.2.2.1 Define `PropertyResult` type with success/failure/error variants
- [ ] 3.2.2.2 Include test count, shrink count, and seed in results
- [ ] 3.2.2.3 Include counterexample value and shrunk value in failures
- [ ] 3.2.2.4 Include label distribution statistics in success results

### 3.2.3 Test Orchestration
- [ ] **Task 3.2.3 Complete**

Implement orchestration for running multiple properties and aggregating results.

- [ ] 3.2.3.1 Implement `run_properties/1` running a list of properties
- [ ] 3.2.3.2 Implement parallel execution option for independent properties
- [ ] 3.2.3.3 Implement timeout per property to prevent infinite loops
- [ ] 3.2.3.4 Aggregate results into summary with pass/fail counts

### 3.2.4 Integration with Test Framework
- [ ] **Task 3.2.4 Complete**

Integrate property testing with Catena's existing test infrastructure and common test runners.

- [ ] 3.2.4.1 Implement `property_test/2` macro for defining property tests
- [ ] 3.2.4.2 Generate EUnit-compatible test cases from properties
- [ ] 3.2.4.3 Report property results in standard test output format
- [ ] 3.2.4.4 Support `--only-properties` flag for running only property tests

### Unit Tests - Section 3.2
- [ ] **Unit Tests 3.2 Complete**
- [ ] Test runner executes correct number of tests
- [ ] Test size scales across test runs
- [ ] Test early exit on first failure
- [ ] Test result types capture all relevant information
- [ ] Test parallel execution produces same results as sequential
- [ ] Test timeout prevents infinite property loops
- [ ] Test EUnit integration generates valid test cases

---

## 3.3 Failure Reporting
- [ ] **Section 3.3 Complete**

Clear failure reporting is essential for debugging property failures. Reports must show the failing input, the expected property, and a trace of shrinking attempts. Color output and formatting enhance readability.

### 3.3.1 Counterexample Display
- [ ] **Task 3.3.1 Complete**

Implement display of counterexamples in a readable format. Complex values need structured formatting.

- [ ] 3.3.1.1 Implement `format_counterexample/1` for readable value display
- [ ] 3.3.1.2 Implement pretty-printing for nested structures (lists, maps, tuples)
- [ ] 3.3.1.3 Implement truncation for very large values with `...` indicator
- [ ] 3.3.1.4 Implement custom formatters for user-defined types

### 3.3.2 Failure Context
- [ ] **Task 3.3.2 Complete**

Include contextual information in failure reports to aid debugging.

- [ ] 3.3.2.1 Include property name and source location in failure report
- [ ] 3.3.2.2 Include original (unshrunk) counterexample alongside shrunk version
- [ ] 3.3.2.3 Include shrink history (which shrinks were tried)
- [ ] 3.3.2.4 Include reproducible seed for re-running the exact failure

### 3.3.3 Terminal Output Formatting
- [ ] **Task 3.3.3 Complete**

Implement rich terminal output with colors, formatting, and progress indicators.

- [ ] 3.3.3.1 Implement colored output (green for pass, red for fail)
- [ ] 3.3.3.2 Implement progress indicator during test run
- [ ] 3.3.3.3 Implement summary statistics (tests run, passed, failed, discarded)
- [ ] 3.3.3.4 Support plain text mode for non-terminal output

### 3.3.4 Structured Output
- [ ] **Task 3.3.4 Complete**

Implement structured output formats for CI integration and programmatic processing.

- [ ] 3.3.4.1 Implement JSON output format for test results
- [ ] 3.3.4.2 Implement JUnit XML format for CI systems
- [ ] 3.3.4.3 Implement machine-readable counterexample format
- [ ] 3.3.4.4 Support multiple output formats simultaneously

### Unit Tests - Section 3.3
- [ ] **Unit Tests 3.3 Complete**
- [ ] Test counterexample formatting for various types
- [ ] Test truncation of large values
- [ ] Test failure context includes all required information
- [ ] Test colored output contains correct ANSI codes
- [ ] Test JSON output is valid JSON
- [ ] Test JUnit XML is valid XML

---

## 3.4 Counterexample Minimization
- [ ] **Section 3.4 Complete**

Counterexample minimization uses the shrink tree to find the smallest failing input. This is essential for debugging as minimal examples are much easier to understand than random large inputs.

### 3.4.1 Shrink Search Algorithm
- [ ] **Task 3.4.1 Complete**

Implement the search algorithm that traverses the shrink tree to find minimal failures.

- [ ] 3.4.1.1 Implement depth-first search through shrink tree
- [ ] 3.4.1.2 Implement binary search optimization for ordered shrinks
- [ ] 3.4.1.3 Implement shrink attempt limit to bound search time
- [ ] 3.4.1.4 Track best (smallest) failure seen during search

### 3.4.2 Shrink Progress Reporting
- [ ] **Task 3.4.2 Complete**

Report progress during shrinking to show that minimization is working.

- [ ] 3.4.2.1 Implement shrink step counter in output
- [ ] 3.4.2.2 Implement periodic progress updates during long shrinks
- [ ] 3.4.2.3 Implement size reduction metrics (original size vs shrunk size)
- [ ] 3.4.2.4 Allow user interruption of shrinking with Ctrl-C

### 3.4.3 Shrink Termination
- [ ] **Task 3.4.3 Complete**

Implement termination conditions for shrinking to prevent infinite shrink loops.

- [ ] 3.4.3.1 Implement maximum shrink attempt limit (default 1000)
- [ ] 3.4.3.2 Implement shrink timeout (default 30 seconds)
- [ ] 3.4.3.3 Implement size-based termination (stop when size can't decrease)
- [ ] 3.4.3.4 Report termination reason if limit reached before minimal found

### 3.4.4 Advanced Shrinking Strategies
- [ ] **Task 3.4.4 Complete**

Implement advanced strategies for more effective shrinking in complex cases.

- [ ] 3.4.4.1 Implement multi-pass shrinking (shrink, then try to shrink more)
- [ ] 3.4.4.2 Implement component-wise shrinking for complex structures
- [ ] 3.4.4.3 Implement shrink caching to avoid re-testing same values
- [ ] 3.4.4.4 Document shrinking effectiveness metrics

### Unit Tests - Section 3.4
- [ ] **Unit Tests 3.4 Complete**
- [ ] Test shrinking finds smaller failing values
- [ ] Test shrink limits prevent infinite loops
- [ ] Test binary search finds minimal efficiently
- [ ] Test progress reporting shows shrink attempts
- [ ] Test interruption works during long shrinks
- [ ] Test shrink caching avoids duplicate work

---

## 3.5 Seed-Based Reproducibility
- [ ] **Section 3.5 Complete**

Reproducibility is critical for debugging. Any property failure must be reproducible by running with the same seed. Seeds must be displayed in failure output and accepted as input.

### 3.5.1 Seed Display and Input
- [ ] **Task 3.5.1 Complete**

Implement seed display in output and seed specification in input.

- [ ] 3.5.1.1 Display seed used for each test run in output
- [ ] 3.5.1.2 Display specific seed that caused failure in failure report
- [ ] 3.5.1.3 Implement `--seed` command line option for specifying seed
- [ ] 3.5.1.4 Implement `with_seed/2` for programmatic seed specification

### 3.5.2 Seed Persistence
- [ ] **Task 3.5.2 Complete**

Implement optional persistence of failing seeds for regression testing.

- [ ] 3.5.2.1 Implement seed file storage for failing seeds
- [ ] 3.5.2.2 Implement automatic replay of stored seeds before random testing
- [ ] 3.5.2.3 Implement seed cleanup when failures are fixed
- [ ] 3.5.2.4 Support version control-friendly seed file format

### 3.5.3 Determinism Verification
- [ ] **Task 3.5.3 Complete**

Verify that test execution is truly deterministic given a seed.

- [ ] 3.5.3.1 Implement determinism check mode running same seed twice
- [ ] 3.5.3.2 Detect and report non-deterministic generators
- [ ] 3.5.3.3 Document requirements for deterministic properties
- [ ] 3.5.3.4 Warn about common sources of non-determinism

### Unit Tests - Section 3.5
- [ ] **Unit Tests 3.5 Complete**
- [ ] Test same seed produces identical test runs
- [ ] Test seed from failure report reproduces failure
- [ ] Test seed file is created on failure
- [ ] Test stored seeds are replayed
- [ ] Test determinism check catches non-determinism

---

## 3.6 Integration Tests - Phase 3
- [ ] **Integration Tests 3.6 Complete**

Integration tests verify the complete property testing workflow from property definition through execution and reporting.

- [ ] Test end-to-end property testing: define, run, pass
- [ ] Test end-to-end failure scenario: define, run, fail, shrink, report
- [ ] Test reproducibility: run with seed, get same result
- [ ] Test multiple properties in a group
- [ ] Test property with preconditions (implications)
- [ ] Test labeled properties with distribution statistics
- [ ] Test integration with EUnit test runner
- [ ] Test CI output formats (JSON, JUnit XML)
- [ ] Test performance: 100 tests with complex generators < 5 seconds
- [ ] Test user experience: clear error messages, helpful output
- [ ] Test edge cases: all tests discarded, timeout, very large counterexamples

---

## Success Criteria

1. **Property DSL**: Clean syntax for `property`, `forall`, and implications
2. **Test Runner**: Reliable execution with size scaling and early exit
3. **Failure Reporting**: Clear counterexamples with context and formatting
4. **Shrinking**: Effective minimization with reasonable time bounds
5. **Reproducibility**: Perfect reproduction with seeds
6. **Integration**: Works with existing test infrastructure
7. **Performance**: Fast enough for interactive development
8. **Documentation**: Clear examples and troubleshooting guides

---

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 4**: Law testing building on property testing infrastructure
- **Phase 5**: Stateful testing extending property runner with state machine execution
- **Phase 6**: BEAM integration using property framework for process testing
- **Phase 7**: Coverage-guided generation using test runner feedback
