# Phase 2: Standard Generators

## Overview

This phase builds the standard library of generators for common Erlang and Catena types. Using the core generator infrastructure from Phase 1, we implement generators for numeric types (floats, arbitrary precision), text and binary data, collections (lists, tuples, maps), recursive structures, and functions. Each generator includes appropriate shrinking behavior and integrates with the range system for size control.

By the end of this phase, developers will have a comprehensive toolkit of generators covering all common data types, enabling property testing of real-world Catena and Erlang applications. The generators follow consistent patterns for composition, shrinking, and size scaling.

**Design Philosophy**: Standard generators should be predictable and well-documented. Shrinking should produce "simpler" values in an intuitive sense (shorter lists, smaller numbers, simpler structures). All generators should compose cleanly using the categorical instances from Phase 1.

This phase runs for **2 weeks** and focuses on completeness and usability, ensuring developers have generators for all types they commonly use.

---

## 2.1 Numeric Generators
- [ ] **Section 2.1 Complete**

Numeric generators cover integers, floats, and arbitrary precision numbers. Each supports range-based generation and shrinks toward zero (or the range origin). Special care is needed for floating-point edge cases like NaN, infinity, and negative zero.

### 2.1.1 Float Generators
- [ ] **Task 2.1.1 Complete**

Implement floating-point generators with proper handling of IEEE 754 special values. Floats are tricky because of precision issues and special values.

- [ ] 2.1.1.1 Implement `gen_float/1` generating floats within a Range
- [ ] 2.1.1.2 Implement shrinking toward zero with decreasing magnitude
- [ ] 2.1.1.3 Implement `gen_float_special/0` including NaN, infinity, negative zero
- [ ] 2.1.1.4 Implement `gen_float_normal/1` excluding special values
- [ ] 2.1.1.5 Document precision considerations for float comparison in properties

### 2.1.2 Arbitrary Precision Numbers
- [ ] **Task 2.1.2 Complete**

Implement generators for Erlang's arbitrary precision integers and rationals. These are essential for testing numeric algorithms without overflow concerns.

- [ ] 2.1.2.1 Implement `gen_bigint/1` generating large integers beyond machine word size
- [ ] 2.1.2.2 Implement shrinking for big integers toward smaller values
- [ ] 2.1.2.3 Implement `gen_rational/1` generating exact fractions (if supported)
- [ ] 2.1.2.4 Implement `gen_number/0` choosing between int, float, bigint

### 2.1.3 Numeric Ranges
- [ ] **Task 2.1.3 Complete**

Extend the range system to support floating-point and provide common numeric ranges as convenient constructors.

- [ ] 2.1.3.1 Implement `range_float/2` for floating-point ranges
- [ ] 2.1.3.2 Implement `range_unit/0` for 0.0 to 1.0 range
- [ ] 2.1.3.3 Implement `range_percent/0` for 0 to 100 range
- [ ] 2.1.3.4 Implement `range_byte/0` for 0 to 255 range

### Unit Tests - Section 2.1
- [ ] **Unit Tests 2.1 Complete**
- [ ] Test float generation respects range bounds
- [ ] Test float shrinking reduces magnitude
- [ ] Test special float values are generated when requested
- [ ] Test bigint generation produces large values
- [ ] Test bigint shrinking produces smaller values
- [ ] Test numeric ranges scale correctly with size

---

## 2.2 Text and Binary Generators
- [ ] **Section 2.2 Complete**

Text generators produce strings, binaries, atoms, and characters. String generation must consider character sets (ASCII, Unicode, alphanumeric) and encoding. Binary generation supports arbitrary byte sequences. Shrinking removes characters and simplifies to earlier alphabet positions.

### 2.2.1 Character Generators
- [ ] **Task 2.2.1 Complete**

Implement single-character generators with various character sets. Characters are the building blocks for string generation.

- [ ] 2.2.1.1 Implement `gen_char/0` generating any printable ASCII character
- [ ] 2.2.1.2 Implement `gen_char_alpha/0` generating a-z, A-Z
- [ ] 2.2.1.3 Implement `gen_char_digit/0` generating 0-9
- [ ] 2.2.1.4 Implement `gen_char_alphanumeric/0` combining alpha and digit
- [ ] 2.2.1.5 Implement `gen_char_unicode/0` generating valid Unicode codepoints

### 2.2.2 String Generators
- [ ] **Task 2.2.2 Complete**

Implement string generators with configurable character sets and length ranges. Strings shrink by removing characters and simplifying remaining characters.

- [ ] 2.2.2.1 Implement `gen_string/1` generating strings from a Range (length)
- [ ] 2.2.2.2 Implement `gen_string/2` with character set specification
- [ ] 2.2.2.3 Implement shrinking: shorter strings, then simpler characters
- [ ] 2.2.2.4 Implement `gen_string_nonempty/1` guaranteeing at least one character
- [ ] 2.2.2.5 Implement `gen_utf8/1` generating valid UTF-8 encoded strings

### 2.2.3 Binary Generators
- [ ] **Task 2.2.3 Complete**

Implement binary generators for raw byte sequences. Essential for testing binary protocols and file formats.

- [ ] 2.2.3.1 Implement `gen_binary/1` generating binaries with length from Range
- [ ] 2.2.3.2 Implement shrinking: shorter binaries, then simpler bytes
- [ ] 2.2.3.3 Implement `gen_bitstring/1` for non-byte-aligned bit sequences
- [ ] 2.2.3.4 Implement `gen_binary_of/2` with custom byte generator

### 2.2.4 Atom Generators
- [ ] **Task 2.2.4 Complete**

Implement atom generators. Atoms require care due to the atom table limit and garbage collection considerations.

- [ ] 2.2.4.1 Implement `gen_atom/0` generating atoms from a predefined pool
- [ ] 2.2.4.2 Implement `gen_atom_unsafe/1` generating arbitrary atoms (with warning)
- [ ] 2.2.4.3 Implement `gen_bool_atom/0` generating `true` or `false`
- [ ] 2.2.4.4 Document atom table exhaustion risks and mitigations

### Unit Tests - Section 2.2
- [ ] **Unit Tests 2.2 Complete**
- [ ] Test character generators produce valid characters in expected sets
- [ ] Test string generation respects length range
- [ ] Test string shrinking reduces length first, then character complexity
- [ ] Test UTF-8 strings are valid encodings
- [ ] Test binary generation produces correct lengths
- [ ] Test atom generation uses predefined pool safely

---

## 2.3 Collection Generators
- [ ] **Section 2.3 Complete**

Collection generators produce lists, tuples, maps, and sets. These compose element generators with structural generation. Shrinking reduces collection size first, then shrinks elements. The size parameter controls maximum collection size.

### 2.3.1 List Generators
- [ ] **Task 2.3.1 Complete**

Implement list generators with various size constraints. Lists are fundamental and must have excellent shrinking behavior.

- [ ] 2.3.1.1 Implement `gen_list/1` generating lists with element generator
- [ ] 2.3.1.2 Implement `gen_list/2` with Range for list length
- [ ] 2.3.1.3 Implement shrinking: remove elements, then shrink remaining elements
- [ ] 2.3.1.4 Implement `gen_nonempty_list/1` guaranteeing at least one element
- [ ] 2.3.1.5 Implement `gen_list_of_length/2` for exact length lists

### 2.3.2 Tuple Generators
- [ ] **Task 2.3.2 Complete**

Implement tuple generators for fixed-arity tuples. Tuples use applicative composition for independent shrinking.

- [ ] 2.3.2.1 Implement `gen_tuple/1` generating tuples from list of generators
- [ ] 2.3.2.2 Implement `gen_tuple2/2`, `gen_tuple3/3`, `gen_tuple4/4` for common arities
- [ ] 2.3.2.3 Ensure shrinking uses applicative combination for interleaved shrinks
- [ ] 2.3.2.4 Implement `gen_pair/2` as alias for `gen_tuple2/2`

### 2.3.3 Map Generators
- [ ] **Task 2.3.3 Complete**

Implement map generators with key and value generators. Maps shrink by removing entries, then shrinking keys and values.

- [ ] 2.3.3.1 Implement `gen_map/2` with key and value generators
- [ ] 2.3.3.2 Implement `gen_map/3` with Range for map size
- [ ] 2.3.3.3 Implement shrinking: remove entries, then shrink remaining
- [ ] 2.3.3.4 Implement `gen_map_fixed/1` from list of {key_gen, val_gen} pairs
- [ ] 2.3.3.5 Handle duplicate keys (last value wins, matching Erlang semantics)

### 2.3.4 Set Generators
- [ ] **Task 2.3.4 Complete**

Implement set generators. Sets require deduplication which may affect size constraints.

- [ ] 2.3.4.1 Implement `gen_ordset/1` generating ordered sets
- [ ] 2.3.4.2 Implement `gen_set/1` generating sets (using maps as representation)
- [ ] 2.3.4.3 Implement `gen_gb_set/1` generating gb_sets
- [ ] 2.3.4.4 Implement shrinking that maintains set invariants

### Unit Tests - Section 2.3
- [ ] **Unit Tests 2.3 Complete**
- [ ] Test list generation respects length range
- [ ] Test list shrinking removes elements before shrinking elements
- [ ] Test tuple generation produces correct arities
- [ ] Test tuple shrinking interleaves element shrinks
- [ ] Test map generation respects size constraints
- [ ] Test map shrinking removes entries
- [ ] Test set generation produces unique elements

---

## 2.4 Recursive Structure Support
- [ ] **Section 2.4 Complete**

Recursive structures (trees, nested lists, ASTs) require special handling to ensure termination. The `sized` and `recursive` combinators enable controlled recursion with size decreasing at each level.

### 2.4.1 Sized Recursion
- [ ] **Task 2.4.1 Complete**

Implement the sized recursion pattern where recursive calls use reduced size to ensure termination.

- [ ] 2.4.1.1 Implement `gen_recursive/2` with base case and recursive case generators
- [ ] 2.4.1.2 Implement automatic size reduction for recursive calls
- [ ] 2.4.1.3 Implement `gen_tree/2` for binary trees with element generator
- [ ] 2.4.1.4 Document size reduction strategies (halving, decrementing)

### 2.4.2 Lazy and Deferred Generators
- [ ] **Task 2.4.2 Complete**

Implement lazy evaluation for recursive generators to prevent infinite expansion at definition time.

- [ ] 2.4.2.1 Implement `gen_lazy/1` deferring generator construction
- [ ] 2.4.2.2 Implement `gen_delay/1` for self-referential generators
- [ ] 2.4.2.3 Ensure lazy generators evaluate at most once per run
- [ ] 2.4.2.4 Implement memoization for expensive generator construction

### 2.4.3 Example Recursive Structures
- [ ] **Task 2.4.3 Complete**

Provide pre-built generators for common recursive structures as examples and utilities.

- [ ] 2.4.3.1 Implement `gen_json/0` generating JSON-compatible structures
- [ ] 2.4.3.2 Implement `gen_nested_list/1` for arbitrarily nested lists
- [ ] 2.4.3.3 Implement `gen_binary_tree/1` for balanced binary trees
- [ ] 2.4.3.4 Document patterns for custom recursive structures

### Unit Tests - Section 2.4
- [ ] **Unit Tests 2.4 Complete**
- [ ] Test recursive structures terminate (don't infinite loop)
- [ ] Test size reduction affects recursion depth
- [ ] Test lazy generators don't evaluate until needed
- [ ] Test JSON generator produces valid JSON structures
- [ ] Test tree generators produce correct tree shapes

---

## 2.5 Function Generators
- [ ] **Section 2.5 Complete**

Function generators create random functions for testing higher-order code. Functions are generated by using the input as a seed variation, ensuring deterministic behavior. Shrinking functions is challenging and requires special techniques.

### 2.5.1 Pure Function Generators
- [ ] **Task 2.5.1 Complete**

Implement generators for pure functions that produce consistent outputs for the same inputs.

- [ ] 2.5.1.1 Implement `gen_function/2` with input and output generators
- [ ] 2.5.1.2 Use input hashing to derive seed for output generation
- [ ] 2.5.1.3 Implement `gen_function1/1` for single-argument functions
- [ ] 2.5.1.4 Ensure generated functions are pure (same input = same output)

### 2.5.2 Function Shrinking
- [ ] **Task 2.5.2 Complete**

Implement shrinking strategies for generated functions. Function shrinking typically means simpler return values.

- [ ] 2.5.2.1 Implement shrinking by simplifying function outputs
- [ ] 2.5.2.2 Implement `gen_const_function/1` that shrinks to constant functions
- [ ] 2.5.2.3 Document limitations of function shrinking
- [ ] 2.5.2.4 Implement `show_function/2` for displaying function behavior on sample inputs

### 2.5.3 Predicate and Comparator Generators
- [ ] **Task 2.5.3 Complete**

Implement generators for common function types: predicates (returning boolean) and comparators.

- [ ] 2.5.3.1 Implement `gen_predicate/1` generating functions returning boolean
- [ ] 2.5.3.2 Implement `gen_ordering/0` generating comparison functions
- [ ] 2.5.3.3 Implement `gen_equivalence/1` generating equivalence relations
- [ ] 2.5.3.4 Document usage for testing filter, sort, and partition functions

### Unit Tests - Section 2.5
- [ ] **Unit Tests 2.5 Complete**
- [ ] Test generated functions are deterministic
- [ ] Test function generators produce valid return types
- [ ] Test function shrinking simplifies outputs
- [ ] Test predicates return only true/false
- [ ] Test comparators produce valid orderings

---

## 2.6 Integration Tests - Phase 2
- [ ] **Integration Tests 2.6 Complete**

Integration tests verify that standard generators compose correctly and integrate with the core infrastructure from Phase 1.

- [ ] Test composing standard generators (lists of maps, tuples of strings, etc.)
- [ ] Test all generators shrink correctly toward simpler values
- [ ] Test size parameter affects all generators appropriately
- [ ] Test reproducibility with standard generators (same seed = same values)
- [ ] Test interoperability between numeric, text, and collection generators
- [ ] Test recursive generators with various depths
- [ ] Test function generators work with higher-order property testing
- [ ] Test performance: generating 10000 complex values (nested structures) < 5 seconds
- [ ] Test memory: no leaks when generating large collections
- [ ] Test edge cases: empty collections, boundary values, special floats

---

## Success Criteria

1. **Numeric Generators**: Integers, floats, and arbitrary precision with proper shrinking
2. **Text Generators**: Strings, binaries, atoms with charset support
3. **Collection Generators**: Lists, tuples, maps, sets with size control
4. **Recursive Support**: Trees and nested structures with termination guarantees
5. **Function Generators**: Pure functions with deterministic behavior
6. **Shrinking Quality**: All generators shrink toward intuitively "simpler" values
7. **Composition**: All generators compose cleanly via categorical operations
8. **Performance**: Generation fast enough for practical property testing
9. **Documentation**: Clear examples for each generator type

---

## Provides Foundation

This phase establishes the infrastructure for:
- **Phase 3**: Property testing using standard generators for real-world types
- **Phase 4**: Law testing with generators for trait instances
- **Phase 5**: Stateful testing with command generators built on standard generators
- **Phase 6**: BEAM integration with message and process generators
- **Phase 7**: Type-directed properties leveraging type-to-generator mapping
