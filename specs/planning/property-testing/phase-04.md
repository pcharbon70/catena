# Phase 4: Law Testing

## Overview

This phase implements law testing infrastructure for verifying categorical properties of trait implementations. Laws are mathematical properties that trait instances must satisfy - for example, Functor's identity and composition laws. We build a three-tier architecture: law specifications (pure definitions), discipline framework (test suite packaging), and test integration (macro-based derivation). This enables automatic verification that trait instances are well-behaved.

By the end of this phase, developers can verify their trait implementations satisfy required laws using simple declarations like `derive_law_tests for: [Mapper, Pipeline]`. The system generates property tests for each law, runs them with the property testing infrastructure from Phase 3, and reports any law violations.

**Design Philosophy**: Laws are defined in pure, composable modules separate from test infrastructure. The discipline framework packages laws into reusable test suites. Integration with Catena's trait system enables automatic law derivation. Law names and descriptions should be educational, helping developers understand category theory.

This phase runs for **4 weeks** and focuses on correctness verification and developer education through clear law descriptions.

**Implementation Note - Blockers and Workarounds**: This phase includes features requiring Catena constructs not yet implemented:

1. **`derive_law_tests` Macro (Section 4.4)**: Requires Catena's macro system and derive/attribute system
   - **Workaround**: Implement law test generation as Erlang functions. Instead of `@derive Generator` attributes, use explicit `law_tests_for(Module, [functor, monad])` function calls that return test cases.

2. **Type Reflection (Sections 4.4.2, 4.4.3)**: Requires inspecting trait implementations at compile time
   - **Workaround**: Use explicit configuration rather than automatic reflection. Users specify which traits their type implements and provide the required generator. This mirrors how PropEr currently works.

3. **Trait Introspection**: Checking if a type implements specific traits
   - **Workaround**: Maintain a manual registry or require explicit trait lists in law test definitions.

The function-based approach is actually more flexible and can serve as the foundation for ergonomic Catena syntax once the macro system is available. Example:

```erlang
%% Instead of: derive_law_tests for: [Mapper, Pipeline]
%% Use: catena_laws:test_suite(my_type, [{generator, gen_my_type()}, {traits, [functor, monad]}])
```

---

## 4.1 Law Specification Architecture
- [ ] **Section 4.1 Complete**

The law specification architecture defines how laws are represented, parameterized, and composed. Laws are functions that take generators and equality functions as parameters and produce properties. This parameterization enables the same law to be tested across different types.

### 4.1.1 Law Type Definition
- [ ] **Task 4.1.1 Complete**

Define the Law type that represents a single mathematical law. Laws are parameterized by generators and equality to be type-agnostic.

- [ ] 4.1.1.1 Define `Law` record with `name`, `description`, and `property_fn` fields
- [ ] 4.1.1.2 Define `LawParams` record with `generator`, `eq_fn`, and optional `extra_gens`
- [ ] 4.1.1.3 Implement `law/3` constructor creating a law from name, description, and property function
- [ ] 4.1.1.4 Implement `apply_law/2` instantiating a law with concrete parameters

### 4.1.2 Equality Abstraction
- [ ] **Task 4.1.2 Complete**

Laws use an equality function parameter rather than hardcoded `==`. This handles types with custom equality (e.g., floating point with epsilon, sets ignoring order).

- [ ] 4.1.2.1 Define `EqFn` type as `(A, A) -> Bool`
- [ ] 4.1.2.2 Implement `eq_structural/0` using Erlang's `=:=`
- [ ] 4.1.2.3 Implement `eq_float/1` with configurable epsilon
- [ ] 4.1.2.4 Implement `eq_ignore_order/1` for collections where order doesn't matter

### 4.1.3 Law Composition
- [ ] **Task 4.1.3 Complete**

Implement composition of laws into law sets. Traits have multiple laws that should be tested together.

- [ ] 4.1.3.1 Implement `law_set/2` grouping multiple laws under a name
- [ ] 4.1.3.2 Implement `law_set_concat/2` combining law sets
- [ ] 4.1.3.3 Implement `law_set_map/2` transforming laws in a set
- [ ] 4.1.3.4 Implement `law_set_filter/2` selecting subset of laws

### Unit Tests - Section 4.1
- [ ] **Unit Tests 4.1 Complete**
- [ ] Test law creation with various parameters
- [ ] Test law application produces valid properties
- [ ] Test equality abstractions work correctly
- [ ] Test law set composition
- [ ] Test law set transformations

---

## 4.2 Trait Law Definitions
- [ ] **Section 4.2 Complete**

Define the mathematical laws for each of Catena's category theory traits. Laws are expressed as properties using the law specification architecture. Each law includes a clear name and description for educational purposes.

### 4.2.1 Functor (Mapper) Laws
- [ ] **Task 4.2.1 Complete**

Define the two Functor laws: identity and composition. Functor is the foundation of the categorical hierarchy.

- [ ] 4.2.1.1 Define identity law: `map id x == x`
- [ ] 4.2.1.2 Define composition law: `map (f . g) x == map f (map g x)`
- [ ] 4.2.1.3 Include clear descriptions explaining each law
- [ ] 4.2.1.4 Package as `functor_laws/0` law set

### 4.2.2 Applicative (Applicator) Laws
- [ ] **Task 4.2.2 Complete**

Define the Applicative laws: identity, composition, homomorphism, and interchange. Applicative builds on Functor.

- [ ] 4.2.2.1 Define identity law: `ap (pure id) x == x`
- [ ] 4.2.2.2 Define composition law: `ap (ap (ap (pure (.)) f) g) x == ap f (ap g x)`
- [ ] 4.2.2.3 Define homomorphism law: `ap (pure f) (pure x) == pure (f x)`
- [ ] 4.2.2.4 Define interchange law: `ap f (pure x) == ap (pure (fn g -> g x)) f`
- [ ] 4.2.2.5 Package as `applicative_laws/0` law set

### 4.2.3 Monad (Pipeline) Laws
- [ ] **Task 4.2.3 Complete**

Define the three Monad laws: left identity, right identity, and associativity.

- [ ] 4.2.3.1 Define left identity law: `bind (pure a) f == f a`
- [ ] 4.2.3.2 Define right identity law: `bind m pure == m`
- [ ] 4.2.3.3 Define associativity law: `bind (bind m f) g == bind m (fn x -> bind (f x) g)`
- [ ] 4.2.3.4 Package as `monad_laws/0` law set

### 4.2.4 Semigroup (Combiner) Laws
- [ ] **Task 4.2.4 Complete**

Define the Semigroup associativity law.

- [ ] 4.2.4.1 Define associativity law: `combine (combine a b) c == combine a (combine b c)`
- [ ] 4.2.4.2 Package as `semigroup_laws/0` law set

### 4.2.5 Monoid (Accumulator) Laws
- [ ] **Task 4.2.5 Complete**

Define Monoid laws: left identity, right identity, and associativity (inherits from Semigroup).

- [ ] 4.2.5.1 Define left identity law: `combine empty a == a`
- [ ] 4.2.5.2 Define right identity law: `combine a empty == a`
- [ ] 4.2.5.3 Inherit associativity from Semigroup
- [ ] 4.2.5.4 Package as `monoid_laws/0` law set

### 4.2.6 Setoid (Comparable) Laws
- [ ] **Task 4.2.6 Complete**

Define Setoid laws: reflexivity, symmetry, and transitivity.

- [ ] 4.2.6.1 Define reflexivity law: `equals a a == true`
- [ ] 4.2.6.2 Define symmetry law: `equals a b == equals b a`
- [ ] 4.2.6.3 Define transitivity law: `equals a b && equals b c ==> equals a c`
- [ ] 4.2.6.4 Package as `setoid_laws/0` law set

### 4.2.7 Ord Laws
- [ ] **Task 4.2.7 Complete**

Define Ord laws for total ordering.

- [ ] 4.2.7.1 Define antisymmetry law: `lte a b && lte b a ==> equals a b`
- [ ] 4.2.7.2 Define transitivity law: `lte a b && lte b c ==> lte a c`
- [ ] 4.2.7.3 Define totality law: `lte a b || lte b a == true`
- [ ] 4.2.7.4 Package as `ord_laws/0` law set

### Unit Tests - Section 4.2
- [ ] **Unit Tests 4.2 Complete**
- [ ] Test Functor laws hold for List
- [ ] Test Applicative laws hold for Maybe
- [ ] Test Monad laws hold for Maybe
- [ ] Test Semigroup laws hold for String
- [ ] Test Monoid laws hold for List
- [ ] Test Setoid laws hold for Integer
- [ ] Test Ord laws hold for Integer

---

## 4.3 Discipline Framework
- [ ] **Section 4.3 Complete**

The discipline framework packages laws into reusable test suites. A discipline defines what laws apply to a trait, how to parameterize them, and what auxiliary generators are needed (e.g., function generators for composition laws).

### 4.3.1 Discipline Type Definition
- [ ] **Task 4.3.1 Complete**

Define the Discipline type that packages laws for a specific trait.

- [ ] 4.3.1.1 Define `Discipline` record with `trait_name`, `laws`, and `requires` fields
- [ ] 4.3.1.2 Implement `discipline/3` constructor
- [ ] 4.3.1.3 Implement `discipline_laws/2` extracting laws with parameters
- [ ] 4.3.1.4 Handle trait hierarchy (Monad discipline includes Applicative laws)

### 4.3.2 Standard Disciplines
- [ ] **Task 4.3.2 Complete**

Implement standard disciplines for all category theory traits.

- [ ] 4.3.2.1 Implement `functor_discipline/0` for Mapper trait
- [ ] 4.3.2.2 Implement `applicative_discipline/0` for Applicator trait
- [ ] 4.3.2.3 Implement `monad_discipline/0` for Pipeline trait
- [ ] 4.3.2.4 Implement `semigroup_discipline/0` for Combiner trait
- [ ] 4.3.2.5 Implement `monoid_discipline/0` for Accumulator trait
- [ ] 4.3.2.6 Implement `setoid_discipline/0` for Comparable trait
- [ ] 4.3.2.7 Implement `ord_discipline/0` for Ord trait

### 4.3.3 Auxiliary Generators
- [ ] **Task 4.3.3 Complete**

Laws like composition require function generators. Define auxiliary generator requirements for each discipline.

- [ ] 4.3.3.1 Define function generator requirement for Functor composition
- [ ] 4.3.3.2 Define Kleisli arrow generator requirement for Monad associativity
- [ ] 4.3.3.3 Implement default auxiliary generators using Phase 2 function generators
- [ ] 4.3.3.4 Allow custom auxiliary generator specification

### 4.3.4 Discipline Composition
- [ ] **Task 4.3.4 Complete**

Implement composition of disciplines for types implementing multiple traits.

- [ ] 4.3.4.1 Implement `discipline_combine/2` combining two disciplines
- [ ] 4.3.4.2 Handle duplicate law removal when disciplines overlap
- [ ] 4.3.4.3 Implement `all_laws_for/1` collecting laws for a list of traits
- [ ] 4.3.4.4 Validate trait hierarchy (can't test Monad without Applicative)

### Unit Tests - Section 4.3
- [ ] **Unit Tests 4.3 Complete**
- [ ] Test discipline creation and law extraction
- [ ] Test standard disciplines include correct laws
- [ ] Test auxiliary generator requirements are satisfied
- [ ] Test discipline composition handles overlaps
- [ ] Test trait hierarchy validation

---

## 4.4 derive_law_tests Macro
- [ ] **Section 4.4 Complete**

The `derive_law_tests` macro automatically generates property tests for a type's trait implementations. It inspects which traits a type implements, collects the corresponding disciplines, and generates test cases.

### 4.4.1 Macro Definition
- [ ] **Task 4.4.1 Complete**

Define the macro that generates law tests from trait implementations.

- [ ] 4.4.1.1 Implement `derive_law_tests/1` macro accepting trait list
- [ ] 4.4.1.2 Extract generator from type module or require explicit specification
- [ ] 4.4.1.3 Extract equality function or default to structural equality
- [ ] 4.4.1.4 Generate property tests for each law in specified traits

### 4.4.2 Generator Resolution
- [ ] **Task 4.4.2 Complete**

Implement generator resolution: finding or requiring generators for the type being tested.

- [ ] 4.4.2.1 Look for `generator/0` export in type module
- [ ] 4.4.2.2 Accept explicit `gen: Gen` option
- [ ] 4.4.2.3 Provide helpful error if no generator found
- [ ] 4.4.2.4 Support generator composition for parameterized types (e.g., `Maybe Int`)

### 4.4.3 Test Generation
- [ ] **Task 4.4.3 Complete**

Generate the actual test cases from disciplines and parameters.

- [ ] 4.4.3.1 Generate EUnit test function for each law
- [ ] 4.4.3.2 Use meaningful test names including trait and law name
- [ ] 4.4.3.3 Include law description in test documentation
- [ ] 4.4.3.4 Support test filtering by trait or law name

### 4.4.4 Error Reporting
- [ ] **Task 4.4.4 Complete**

Provide clear error messages when law tests fail.

- [ ] 4.4.4.1 Include law name and description in failure message
- [ ] 4.4.4.2 Show counterexample with meaningful variable names (not just `x`, `y`)
- [ ] 4.4.4.3 Explain what the law means in plain English
- [ ] 4.4.4.4 Suggest common causes of law violations

### Unit Tests - Section 4.4
- [ ] **Unit Tests 4.4 Complete**
- [ ] Test macro generates correct number of tests
- [ ] Test generator resolution finds exported generators
- [ ] Test explicit generator option works
- [ ] Test generated tests have correct names
- [ ] Test failure messages are helpful

---

## 4.5 Law Test Integration
- [ ] **Section 4.5 Complete**

Integrate law testing with the broader test ecosystem: CI pipelines, test runners, and development workflows.

### 4.5.1 Test Runner Integration
- [ ] **Task 4.5.1 Complete**

Integrate law tests with Catena's test runner.

- [ ] 4.5.1.1 Register generated law tests with EUnit
- [ ] 4.5.1.2 Support `--laws-only` flag for running only law tests
- [ ] 4.5.1.3 Group law tests by trait in output
- [ ] 4.5.1.4 Report law test coverage (which traits/laws were tested)

### 4.5.2 CI Integration
- [ ] **Task 4.5.2 Complete**

Ensure law tests work well in CI environments.

- [ ] 4.5.2.1 Support deterministic seeds for reproducible CI runs
- [ ] 4.5.2.2 Generate JUnit XML output for CI systems
- [ ] 4.5.2.3 Fail fast on law violations
- [ ] 4.5.2.4 Report timing for performance monitoring

### 4.5.3 Development Workflow
- [ ] **Task 4.5.3 Complete**

Support interactive development with law tests.

- [ ] 4.5.3.1 Implement `check_laws/2` for REPL usage
- [ ] 4.5.3.2 Support quick check mode (fewer tests for fast iteration)
- [ ] 4.5.3.3 Implement watch mode for continuous law checking
- [ ] 4.5.3.4 Provide hints when writing new trait instances

### 4.5.4 Documentation Generation
- [ ] **Task 4.5.4 Complete**

Generate documentation about law compliance.

- [ ] 4.5.4.1 Generate law compliance report for a module
- [ ] 4.5.4.2 Include law descriptions in generated documentation
- [ ] 4.5.4.3 Link laws to category theory references
- [ ] 4.5.4.4 Generate badge for "laws verified" status

### Unit Tests - Section 4.5
- [ ] **Unit Tests 4.5 Complete**
- [ ] Test runner integration executes law tests
- [ ] Test CI output formats are valid
- [ ] Test REPL usage works correctly
- [ ] Test documentation generation produces valid output

---

## 4.6 Integration Tests - Phase 4
- [ ] **Integration Tests 4.6 Complete**

Integration tests verify the complete law testing workflow from trait implementation through verification.

- [ ] Test complete workflow: implement trait, add derive_law_tests, run, pass
- [ ] Test law violation detection: implement broken trait, verify failure
- [ ] Test all standard traits with built-in types (List, Maybe, Either)
- [ ] Test custom type with custom generator
- [ ] Test trait hierarchy (Monad implies Applicative implies Functor)
- [ ] Test CI integration with JUnit XML output
- [ ] Test REPL quick-check workflow
- [ ] Test error messages are educational
- [ ] Test performance: law tests complete in reasonable time
- [ ] Test edge cases: empty generators, always-failing predicates

---

## Success Criteria

1. **Law Specifications**: Complete law definitions for all category theory traits
2. **Discipline Framework**: Reusable test suite packaging with trait hierarchy support
3. **derive_law_tests Macro**: One-line law verification for trait implementations
4. **Error Messages**: Educational messages explaining law violations
5. **Integration**: Works with existing test infrastructure and CI
6. **Documentation**: Laws explained with category theory context
7. **Performance**: Law tests fast enough for development workflows
8. **Coverage**: All standard library trait instances verified

---

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 5**: Stateful testing with state machine laws (simulation laws)
- **Phase 6**: BEAM integration with process behavior laws
- **Phase 7**: Type-directed properties using law-based verification
