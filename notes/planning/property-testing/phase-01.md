# Phase 1: Core Generators

## Overview

This phase establishes the foundational generator infrastructure for Catena's property testing library. We implement the core `Generator<A>` type that produces rose trees for integrated shrinking, the categorical instances (Functor, Applicative, Monad) that enable generator composition, primitive combinators for basic value generation, and the Range type system for controlling generation bounds. The goal is to create a mathematically sound foundation where generators are first-class values that automatically preserve shrinking invariants through composition.

By the end of this phase, we will have a working generator system that can produce random values with their shrink trees, compose generators using categorical abstractions, and control generation size through ranges. This establishes the technical foundation for all subsequent phases, enabling standard generators, property testing, and law verification to build on a principled categorical base.

**Design Philosophy**: Generators are explicit values (not type classes/protocols) to maximize flexibility and avoid global state. Integrated shrinking via rose trees eliminates the class of bugs where shrinking violates generator invariants. Both Applicative and Monadic interfaces are provided, with documentation guiding users toward Applicative when possible for better shrinking behavior.

This phase runs for **4 weeks** and focuses on correctness and categorical law compliance, establishing clean abstractions that support future feature additions.

---

## 1.1 Rose Tree Data Structure
- [ ] **Section 1.1 Complete**

The rose tree (multi-way tree) is the fundamental data structure for integrated shrinking. Each node contains a value and a lazy list of child trees representing possible shrinks. This structure enables shrinking to be generated alongside values, ensuring that all shrinks respect the same invariants as the original generation. The tree is comonadic, supporting `extract` (get current value), `duplicate` (tree of subtrees), and `extend` (context-aware transformations).

### 1.1.1 Rose Tree Type Definition
- [ ] **Task 1.1.1 Complete**

Define the core rose tree type and its basic operations. The tree must support lazy evaluation of children to avoid computing all possible shrinks upfront, which would be prohibitively expensive for complex types.

- [ ] 1.1.1.1 Define `Tree<A>` record type with `value :: A` and `children :: () -> [Tree<A>]` fields
- [ ] 1.1.1.2 Implement `tree/2` constructor function creating a tree from value and child thunk
- [ ] 1.1.1.3 Implement `singleton/1` creating a tree with no children (leaf node)
- [ ] 1.1.1.4 Implement `unfold/2` for generating trees from a seed and expansion function

### 1.1.2 Comonadic Operations
- [ ] **Task 1.1.2 Complete**

Implement the comonadic interface for rose trees. These operations enable powerful shrinking strategies by allowing context-aware transformations across the entire tree structure.

- [ ] 1.1.2.1 Implement `extract/1` returning the root value of the tree
- [ ] 1.1.2.2 Implement `duplicate/1` creating a tree where each node contains the subtree rooted at that node
- [ ] 1.1.2.3 Implement `extend/2` applying a function that sees the entire subtree at each position
- [ ] 1.1.2.4 Verify comonad laws hold: `extract . duplicate = id`, `fmap extract . duplicate = id`, `duplicate . duplicate = fmap duplicate . duplicate`

### 1.1.3 Functor Instance
- [ ] **Task 1.1.3 Complete**

Implement the Functor instance for rose trees, enabling transformation of values while preserving tree structure. This is essential for building composed generators that maintain proper shrinking.

- [ ] 1.1.3.1 Implement `map/2` applying a function to every value in the tree
- [ ] 1.1.3.2 Ensure `map` preserves tree structure (same number of children at each level)
- [ ] 1.1.3.3 Ensure `map` is lazy, not forcing evaluation of children until needed
- [ ] 1.1.3.4 Verify functor laws: `map id = id`, `map (f . g) = map f . map g`

### 1.1.4 Applicative Instance
- [ ] **Task 1.1.4 Complete**

Implement the Applicative instance for rose trees. Applicative combination produces trees where shrinking explores all combinations of shrinks from both inputs, providing thorough shrinking for independent values.

- [ ] 1.1.4.1 Implement `pure/1` creating a singleton tree (no shrinks)
- [ ] 1.1.4.2 Implement `ap/2` applying a tree of functions to a tree of values
- [ ] 1.1.4.3 Ensure `ap` produces interleaved shrinking (shrink left, shrink right, shrink both)
- [ ] 1.1.4.4 Verify applicative laws: identity, composition, homomorphism, interchange

### 1.1.5 Monad Instance
- [ ] **Task 1.1.5 Complete**

Implement the Monad instance for rose trees. Monadic bind is necessary for dependent generation but note that shrinking the first component doesn't automatically re-shrink dependent components.

- [ ] 1.1.5.1 Implement `bind/2` (flatMap) threading a value through a tree-producing function
- [ ] 1.1.5.2 Document the shrinking trade-off: monadic bind has suboptimal shrinking for dependent values
- [ ] 1.1.5.3 Implement `flatten/1` collapsing nested trees
- [ ] 1.1.5.4 Verify monad laws: left identity, right identity, associativity

### Unit Tests - Section 1.1
- [ ] **Unit Tests 1.1 Complete**
- [ ] Test rose tree construction with various depths and branching factors
- [ ] Test `extract` returns root value correctly
- [ ] Test `map` transforms all values while preserving structure
- [ ] Test `ap` produces correct interleaved shrinking order
- [ ] Test `bind` produces correct flattened tree structure
- [ ] Test comonad laws with property-based verification
- [ ] Test functor laws with property-based verification
- [ ] Test applicative laws with property-based verification
- [ ] Test monad laws with property-based verification
- [ ] Test lazy evaluation of children (children not forced until accessed)

---

## 1.2 Generator Type and Seed Management
- [ ] **Section 1.2 Complete**

The Generator type is a function from size and seed to a rose tree. The size parameter enables gradual complexity scaling for recursive structures, while the seed provides deterministic reproducibility for debugging. Proper seed management is critical for reproducible test failures and shrinking.

### 1.2.1 Generator Type Definition
- [ ] **Task 1.2.1 Complete**

Define the core generator type as a function that takes generation parameters and produces a rose tree. The type must be opaque to prevent direct manipulation while exposing a rich combinator interface.

- [ ] 1.2.1.1 Define `Generator<A>` as `{generator, fun((Size, Seed) -> Tree<A>)}`
- [ ] 1.2.1.2 Define `Size` type as non-negative integer controlling generation complexity
- [ ] 1.2.1.3 Define `Seed` type wrapping random state for deterministic generation
- [ ] 1.2.1.4 Implement `run/3` executing a generator with given size and seed

### 1.2.2 Seed Operations
- [ ] **Task 1.2.2 Complete**

Implement seed creation, splitting, and management. Seed splitting is essential for combining generators while maintaining independence and reproducibility.

- [ ] 1.2.2.1 Implement `seed_new/0` creating a random seed from system entropy
- [ ] 1.2.2.2 Implement `seed_from_int/1` creating a deterministic seed from an integer
- [ ] 1.2.2.3 Implement `seed_split/1` splitting a seed into two independent seeds
- [ ] 1.2.2.4 Implement `seed_next/1` advancing a seed to produce (value, new_seed) pair
- [ ] 1.2.2.5 Ensure seed splitting produces statistically independent streams

### 1.2.3 Size Management
- [ ] **Task 1.2.3 Complete**

Implement size parameter handling. Size starts small and grows during test runs, enabling quick discovery of simple counterexamples before exploring complex cases.

- [ ] 1.2.3.1 Implement `sized/1` creating a generator that receives the current size
- [ ] 1.2.3.2 Implement `resize/2` running a generator with a modified size
- [ ] 1.2.3.3 Implement `scale/2` scaling size by a function (e.g., halving for recursion)
- [ ] 1.2.3.4 Document size semantics: size 0 = simplest, size 100 = typical max

### Unit Tests - Section 1.2
- [ ] **Unit Tests 1.2 Complete**
- [ ] Test generator execution produces consistent results with same seed
- [ ] Test different seeds produce different results
- [ ] Test seed splitting produces independent streams
- [ ] Test sized generators receive correct size parameter
- [ ] Test resize modifies size correctly
- [ ] Test scale applies function to size

---

## 1.3 Categorical Instances for Generators
- [ ] **Section 1.3 Complete**

Generators form a monad, enabling powerful composition. The Functor instance transforms generated values, Applicative combines independent generators, and Monad enables dependent generation. These instances lift the rose tree instances through the generator function wrapper.

### 1.3.1 Functor Instance
- [ ] **Task 1.3.1 Complete**

Implement Functor for generators, enabling transformation of generated values while preserving the generation strategy and shrinking.

- [ ] 1.3.1.1 Implement `gen_map/2` applying a function to generated values
- [ ] 1.3.1.2 Ensure mapping preserves shrinking (shrinks are also mapped)
- [ ] 1.3.1.3 Implement `gen_map2/3`, `gen_map3/4`, `gen_map4/5` for multi-argument mapping
- [ ] 1.3.1.4 Verify functor laws for generators

### 1.3.2 Applicative Instance
- [ ] **Task 1.3.2 Complete**

Implement Applicative for generators. Applicative combination is preferred over Monadic when generators are independent, as it produces better shrinking behavior.

- [ ] 1.3.2.1 Implement `gen_pure/1` creating a generator that always produces the same value
- [ ] 1.3.2.2 Implement `gen_ap/2` applying a generator of functions to a generator of values
- [ ] 1.3.2.3 Document that `gen_ap` uses independent seeds for each generator
- [ ] 1.3.2.4 Verify applicative laws for generators

### 1.3.3 Monad Instance
- [ ] **Task 1.3.3 Complete**

Implement Monad for generators, enabling dependent generation where the second generator depends on the value produced by the first.

- [ ] 1.3.3.1 Implement `gen_bind/2` (flatMap) threading generated value to next generator
- [ ] 1.3.3.2 Document shrinking caveat: dependent values don't re-shrink when first shrinks
- [ ] 1.3.3.3 Implement `gen_flatten/1` for nested generators
- [ ] 1.3.3.4 Verify monad laws for generators

### 1.3.4 Alternative Instance
- [ ] **Task 1.3.4 Complete**

Implement Alternative for generators, enabling choice between generators. This is essential for sum types and conditional generation.

- [ ] 1.3.4.1 Implement `gen_empty/0` creating a generator that always fails
- [ ] 1.3.4.2 Implement `gen_alt/2` choosing between two generators
- [ ] 1.3.4.3 Implement `gen_one_of/1` choosing uniformly from a list of generators
- [ ] 1.3.4.4 Implement `gen_frequency/1` choosing with weighted probabilities

### Unit Tests - Section 1.3
- [ ] **Unit Tests 1.3 Complete**
- [ ] Test `gen_map` transforms values correctly
- [ ] Test `gen_pure` produces constant values with no shrinks
- [ ] Test `gen_ap` combines independent generators correctly
- [ ] Test `gen_bind` produces dependent values correctly
- [ ] Test `gen_one_of` chooses from all options with uniform probability
- [ ] Test `gen_frequency` respects weights
- [ ] Test functor laws for generators
- [ ] Test applicative laws for generators
- [ ] Test monad laws for generators

---

## 1.4 Primitive Combinators
- [ ] **Section 1.4 Complete**

Primitive combinators are the building blocks for all generators. They generate basic values like booleans, integers within ranges, and elements from lists. Each primitive includes appropriate shrinking toward "simpler" values (false for bool, 0 for int, first element for lists).

### 1.4.1 Constant and Element Generators
- [ ] **Task 1.4.1 Complete**

Implement generators for constant values and selection from finite sets. These form the base cases for generator composition.

- [ ] 1.4.1.1 Implement `constant/1` generating a fixed value with no shrinks
- [ ] 1.4.1.2 Implement `element/1` choosing uniformly from a non-empty list
- [ ] 1.4.1.3 Implement `elements/1` as alias for `element/1` for compatibility
- [ ] 1.4.1.4 Add shrinking to `element` that shrinks toward earlier list elements

### 1.4.2 Boolean Generator
- [ ] **Task 1.4.2 Complete**

Implement boolean generation with shrinking toward false (the "simpler" boolean value).

- [ ] 1.4.2.1 Implement `gen_bool/0` generating true or false with equal probability
- [ ] 1.4.2.2 Add shrinking: true shrinks to false, false has no shrinks
- [ ] 1.4.2.3 Implement `gen_bool/1` with configurable probability of true

### 1.4.3 Integer Generators
- [ ] **Task 1.4.3 Complete**

Implement integer generation with range control and shrinking toward zero. Integers are fundamental for most property tests.

- [ ] 1.4.3.1 Implement `gen_int/1` generating integers within a Range
- [ ] 1.4.3.2 Implement shrinking toward zero (or toward range bound if zero not in range)
- [ ] 1.4.3.3 Implement `gen_pos_int/0` generating positive integers (1 to size)
- [ ] 1.4.3.4 Implement `gen_neg_int/0` generating negative integers (-size to -1)
- [ ] 1.4.3.5 Implement `gen_nat/0` generating natural numbers (0 to size)

### 1.4.4 Filter Combinator
- [ ] **Task 1.4.4 Complete**

Implement filtering to constrain generated values. Filtering should be used sparingly as it can cause generation to fail if the predicate rarely holds.

- [ ] 1.4.4.1 Implement `gen_filter/2` keeping only values satisfying a predicate
- [ ] 1.4.4.2 Implement retry logic with configurable max attempts (default 100)
- [ ] 1.4.4.3 Implement `gen_such_that/2` as alias for `gen_filter/2`
- [ ] 1.4.4.4 Ensure shrinks also satisfy the predicate

### 1.4.5 Sample and Debug
- [ ] **Task 1.4.5 Complete**

Implement utilities for debugging and exploring generators. Essential for development and troubleshooting.

- [ ] 1.4.5.1 Implement `sample/1` generating a list of example values
- [ ] 1.4.5.2 Implement `sample/2` with configurable count
- [ ] 1.4.5.3 Implement `print_tree/1` displaying the shrink tree structure
- [ ] 1.4.5.4 Implement `shrinks/1` returning the list of immediate shrinks

### Unit Tests - Section 1.4
- [ ] **Unit Tests 1.4 Complete**
- [ ] Test `constant` always produces same value with no shrinks
- [ ] Test `element` produces only values from the list
- [ ] Test `element` shrinks toward earlier elements
- [ ] Test `gen_bool` produces both true and false
- [ ] Test `gen_bool` shrinking (true -> false)
- [ ] Test `gen_int` respects range bounds
- [ ] Test `gen_int` shrinks toward zero
- [ ] Test `gen_filter` only produces values satisfying predicate
- [ ] Test `gen_filter` shrinks satisfy predicate
- [ ] Test `sample` produces requested number of values

---

## 1.5 Range Types
- [ ] **Section 1.5 Complete**

Ranges provide first-class control over generation bounds and how values scale with the size parameter. A range specifies origin (shrink target), bounds, and scaling behavior. This Hedgehog-inspired innovation makes generation more predictable and shrinking more effective.

### 1.5.1 Range Type Definition
- [ ] **Task 1.5.1 Complete**

Define the Range type that encapsulates generation bounds and scaling. Ranges separate the concerns of "what values" from "how to generate them".

- [ ] 1.5.1.1 Define `Range` record with `origin`, `min`, `max`, and `scale_fn` fields
- [ ] 1.5.1.2 Implement `range_bounds/2` computing actual bounds for a given size
- [ ] 1.5.1.3 Implement `range_origin/1` returning the shrink target
- [ ] 1.5.1.4 Ensure origin is always within bounds after scaling

### 1.5.2 Range Constructors
- [ ] **Task 1.5.2 Complete**

Implement various range constructors for different scaling behaviors. Linear ranges grow proportionally with size, constant ranges ignore size, exponential ranges grow rapidly.

- [ ] 1.5.2.1 Implement `range_constant/1` creating a range that ignores size (fixed bounds)
- [ ] 1.5.2.2 Implement `range_linear/2` creating a range that scales linearly with size
- [ ] 1.5.2.3 Implement `range_linear_from/3` with explicit origin
- [ ] 1.5.2.4 Implement `range_exponential/2` for exponential scaling
- [ ] 1.5.2.5 Implement `range_exponential_from/3` with explicit origin

### 1.5.3 Range Integration
- [ ] **Task 1.5.3 Complete**

Integrate ranges with integer generation, replacing ad-hoc size handling with principled range-based control.

- [ ] 1.5.3.1 Update `gen_int/1` to accept Range instead of tuple bounds
- [ ] 1.5.3.2 Implement automatic shrinking toward range origin
- [ ] 1.5.3.3 Implement `gen_int_range/2` for backward compatibility with tuple bounds
- [ ] 1.5.3.4 Document range usage patterns and best practices

### Unit Tests - Section 1.5
- [ ] **Unit Tests 1.5 Complete**
- [ ] Test `range_constant` produces same bounds regardless of size
- [ ] Test `range_linear` scales proportionally with size
- [ ] Test `range_exponential` grows exponentially with size
- [ ] Test origin is always within computed bounds
- [ ] Test `gen_int` with various ranges produces values in bounds
- [ ] Test shrinking moves toward origin

---

## 1.6 Basic Shrinking Infrastructure
- [ ] **Section 1.6 Complete**

Shrinking finds minimal counterexamples by systematically trying smaller values. Integrated shrinking (via rose trees) ensures shrinks respect generator invariants. This section implements shrink search strategies and result minimization.

### 1.6.1 Shrink Strategies
- [ ] **Task 1.6.1 Complete**

Implement strategies for traversing shrink trees to find minimal failing cases. Different strategies trade off speed vs minimality.

- [ ] 1.6.1.1 Implement `shrink_towards/2` creating shrink sequence toward a target
- [ ] 1.6.1.2 Implement `shrink_binary/2` binary search shrinking for numeric values
- [ ] 1.6.1.3 Implement `shrink_list/1` list shrinking (remove elements, shrink elements)
- [ ] 1.6.1.4 Implement `shrink_halves/1` repeatedly halving toward zero

### 1.6.2 Shrink Tree Traversal
- [ ] **Task 1.6.2 Complete**

Implement tree traversal for finding minimal failing cases. The traversal must efficiently search potentially large trees.

- [ ] 1.6.2.1 Implement `find_minimal/2` finding smallest value failing a predicate
- [ ] 1.6.2.2 Implement depth-first search with early termination
- [ ] 1.6.2.3 Implement configurable shrink attempt limits
- [ ] 1.6.2.4 Track shrink path for debugging (which shrinks were taken)

### 1.6.3 Custom Shrinking
- [ ] **Task 1.6.3 Complete**

Enable users to customize shrinking for domain-specific types while maintaining integration with the rose tree infrastructure.

- [ ] 1.6.3.1 Implement `with_shrink/2` adding custom shrink function to generator
- [ ] 1.6.3.2 Implement `no_shrink/1` disabling shrinking for a generator
- [ ] 1.6.3.3 Implement `shrink_map/2` transforming shrink values
- [ ] 1.6.3.4 Document when custom shrinking is needed vs relying on integrated shrinking

### Unit Tests - Section 1.6
- [ ] **Unit Tests 1.6 Complete**
- [ ] Test `shrink_towards` produces correct sequence
- [ ] Test `shrink_binary` finds values efficiently
- [ ] Test `shrink_list` removes and shrinks elements
- [ ] Test `find_minimal` finds smallest failing value
- [ ] Test shrink limits prevent infinite loops
- [ ] Test `no_shrink` produces trees with no children
- [ ] Test `with_shrink` custom shrinks are used

---

## 1.7 Integration Tests - Phase 1
- [ ] **Integration Tests 1.7 Complete**

Integration tests verify that all Phase 1 components work together correctly. These tests exercise realistic generator compositions and verify end-to-end behavior.

- [ ] Test composing multiple generators with `gen_map`, `gen_ap`, `gen_bind`
- [ ] Test generating complex values (pairs, triples, nested structures)
- [ ] Test shrinking composed values finds minimal cases
- [ ] Test reproducibility: same seed produces identical results
- [ ] Test size scaling affects generation appropriately
- [ ] Test range-based generation with various scaling functions
- [ ] Test categorical laws hold for composed generators
- [ ] Test performance: generating 10000 values completes in < 1 second
- [ ] Test memory: generating values doesn't leak (children evaluated lazily)
- [ ] Test interoperability with Erlang data types (atoms, tuples, maps)

---

## Success Criteria

1. **Rose Tree Implementation**: Complete comonadic rose tree with verified laws
2. **Generator Type**: Functional generator type with size and seed parameters
3. **Categorical Instances**: Functor, Applicative, Monad, Alternative for both trees and generators
4. **Primitive Combinators**: Boolean, integer, element generators with shrinking
5. **Range Types**: Linear, constant, exponential ranges with proper shrinking
6. **Shrinking**: Integrated shrinking via rose trees with efficient traversal
7. **Law Compliance**: All categorical laws verified through property tests
8. **Performance**: Generation fast enough for practical use (< 1ms per value)
9. **Documentation**: Clear documentation of shrinking trade-offs (applicative vs monadic)

---

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 2**: Standard generators building on primitive combinators and ranges
- **Phase 3**: Property testing framework using generators and shrink tree traversal
- **Phase 4**: Law testing leveraging categorical generator composition
- **Phase 5**: Stateful testing extending generators with command generation
- **Phase 6**: BEAM integration using generators for process and message testing
- **Phase 7**: Advanced features building on the solid categorical foundation
