# Language Revamp Migration - Keyword Updates

## Overview

This migration updates the Catena language to use more pragmatic, developer-friendly keywords while maintaining the category theory foundation. The migration completely replaces old keywords with new ones - no backward compatibility is maintained for `shape` and `flow` keywords, which are replaced with `type` and `transform` respectively. This is a breaking change that requires all existing Catena source code to be updated.

**Timeline**: 14 hours (2-3 days)
**Risk Level**: High - affects core parsing and all existing code
**Breaking Change**: Yes - no backward compatibility

## 1.1 Lexer Token Updates (1 hour)

This section updates the lexical analyzer to recognize the new keywords `type` and `transform` instead of `shape` and `flow`. The lexer is the first stage of compilation and must be updated before any other components.

### 1.1.1 Update Lexer Token Definitions ✅ COMPLETE

- [x] 1.1.1.1 Update `src/compiler/lexer/catena_lexer.xrl` line 5 comment
  - Change: `%% - Keywords (shape, flow, match, etc.)`
  - To: `%% - Keywords (type, transform, match, etc.)`

- [x] 1.1.1.2 Replace `shape` token with `type` token (line 51)
  - Change: `shape : {token, {shape, TokenLine}}.`
  - To: `type : {token, {type, TokenLine}}.`

- [x] 1.1.1.3 Replace `flow` token with `transform` token (line 52)
  - Change: `flow : {token, {flow, TokenLine}}.`
  - To: `transform : {token, {transform, TokenLine}}.`

### 1.1.2 Rebuild Lexer ✅ COMPLETE

- [x] 1.1.2.1 Run lexer build script
  ```bash
  ./scripts/build_lexer.sh
  ```

- [x] 1.1.2.2 Verify generated `catena_lexer.erl` contains new tokens

### 1.1 Unit Tests ✅ COMPLETE

- [x] 1.1.T1 Test lexer recognizes `type` keyword
- [x] 1.1.T2 Test lexer recognizes `transform` keyword
- [x] 1.1.T3 Test lexer rejects `shape` as identifier (not keyword)
- [x] 1.1.T4 Test lexer rejects `flow` as identifier (not keyword)

**Implementation Status**: Section 1.1 completed on 2024-11-19. See `notes/implementation/language-revamp-section-1.1-summary.md` for details.

## 1.2 Parser Grammar Rules - Core Updates (2 hours) ✅ COMPLETE

This section updates the parser grammar to use the new keywords throughout. The parser defines the syntactic structure of the language and must be comprehensively updated to recognize the new syntax.

### 1.2.1 Update Parser Nonterminals and Terminals ✅ COMPLETE

- [x] 1.2.1.1 Update nonterminals in `src/compiler/parser/catena_parser.yrl` (line 22)
  - Change: `shape_decl flow_decl effect_decl trait_decl instance_decl`
  - To: `type_decl transform_decl effect_decl trait_decl instance_decl`

- [x] 1.2.1.2 Update flow-related nonterminals (line 30)
  - Change: `flow_signature flow_clauses flow_clause`
  - To: `transform_signature transform_clauses transform_clause`

- [x] 1.2.1.3 Update terminals list (line 51)
  - Change: `shape flow match where 'let' 'in' 'do' 'end'`
  - To: `type transform match where 'let' 'in' 'do' 'end'`

### 1.2.2 Update Declaration Grammar Rules ✅ COMPLETE

- [x] 1.2.2.1 Update declaration alternatives (lines 190-194)
  - Change: `declaration -> shape_decl : '$1'.`
  - To: `declaration -> type_decl : '$1'.`
  - Change: `declaration -> flow_decl : '$1'.`
  - To: `declaration -> transform_decl : '$1'.`

- [x] 1.2.2.2 Update error recovery rules (lines 197-200)
  - Change: `declaration -> error shape : ...`
  - To: `declaration -> error type : ...`
  - Update error message: `"Malformed declaration before 'type'"`
  - Change: `declaration -> error flow : ...`
  - To: `declaration -> error transform : ...`
  - Update error message: `"Malformed declaration before 'transform'"`

### 1.2.3 Rename Type Declaration Rules ✅ COMPLETE

- [x] 1.2.3.1 Update section comment (line 208)
  - Change: `%% Shape Declarations (Algebraic Data Types)`
  - To: `%% Type Declarations (Algebraic Data Types)`

- [x] 1.2.3.2 Rename all `shape_decl` rules to `type_decl` (lines 209-226)
  - Replace all occurrences of `shape_decl` with `type_decl`
  - Replace all occurrences of `shape` keyword with `type`
  - Update all error messages from "shape declaration" to "type declaration"

### 1.2.4 Update Trait and Instance Method Rules ✅ COMPLETE

- [x] 1.2.4.1 Update trait default method rule (line 358)
  - Change: `trait_default_method -> flow lower_ident`
  - To: `trait_default_method -> transform lower_ident`

- [x] 1.2.4.2 Update instance method rules (lines 404, 407)
  - Change: `instance_method -> flow lower_ident pattern_list equals expr`
  - To: `instance_method -> transform lower_ident pattern_list equals expr`
  - Change: `instance_method -> flow lower_ident pattern_list equals match`
  - To: `instance_method -> transform lower_ident pattern_list equals match`

### 1.2 Unit Tests ✅ COMPLETE

- [x] 1.2.T1 Test parser accepts `type` declarations
- [x] 1.2.T2 Test parser accepts `transform` declarations
- [x] 1.2.T3 Test parser rejects `shape` keyword
- [x] 1.2.T4 Test parser rejects `flow` keyword
- [x] 1.2.T5 Test parser error recovery with new keywords

**Implementation Status**: Section 1.2 completed on 2024-11-19. See `notes/implementation/language-revamp-section-1.2-summary.md` for details.
**Note**: Parts of Section 1.3 were also completed as they were required to build the parser successfully.

## 1.3 Parser Grammar Rules - Transform Declarations (2 hours) ✅ COMPLETE

This section completes the parser updates by renaming all flow-related grammar rules to transform-related rules. This is a comprehensive update affecting multiple production rules.

### 1.3.1 Rename Transform Declaration Rules ✅ COMPLETE

- [x] 1.3.1.1 Update transform declaration with signature (lines 414-419)
  - Change: `flow_decl -> flow_signature flow_clauses`
  - To: `transform_decl -> transform_signature transform_clauses`
  - Update AST node: `{flow_decl, ...}` to `{transform_decl, ...}`
  - Update helper calls: `extract_flow_name` to `extract_transform_name`
  - Update helper calls: `extract_flow_type` to `extract_transform_type`

- [x] 1.3.1.2 Update signature-only rule (lines 422-427)
  - Change: `flow_decl -> flow_signature`
  - To: `transform_decl -> transform_signature`
  - Update AST node construction

- [x] 1.3.1.3 Update simple transform rule (lines 429-435)
  - Change: `flow_decl -> flow lower_ident`
  - To: `transform_decl -> transform lower_ident`
  - Update AST nodes: `{flow_decl, ...}` and `{flow_clause, ...}`
  - To: `{transform_decl, ...}` and `{transform_clause, ...}`

### 1.3.2 Update Transform Clauses and Signatures ✅ COMPLETE

- [x] 1.3.2.1 Update transform signature rule (lines 459-460)
  - Change: `flow_signature -> flow lower_ident colon type_expr`
  - To: `transform_signature -> transform lower_ident colon type_expr`
  - Update AST: `{flow_sig, ...}` to `{transform_sig, ...}`

- [x] 1.3.2.2 Update transform clauses rules (lines 462-465)
  - Change: `flow_clauses -> flow_clause`
  - To: `transform_clauses -> transform_clause`

- [x] 1.3.2.3 Update transform clause rules (lines 467-486)
  - Replace all `flow_clause` with `transform_clause`
  - Replace all `flow` keywords with `transform`
  - Update all AST nodes from `{flow_clause, ...}` to `{transform_clause, ...}`

### 1.3.3 Update Helper Functions ✅ COMPLETE

- [x] 1.3.3.1 Rename `extract_flow_name` function (line 904)
  - Change function name to `extract_transform_name`
  - Update pattern match: `{flow_sig, Name, _}` to `{transform_sig, Name, _}`

- [x] 1.3.3.2 Rename `extract_flow_type` function (line 907)
  - Change function name to `extract_transform_type`
  - Update pattern match: `{flow_sig, _, Type}` to `{transform_sig, _, Type}`

### 1.3.4 Rebuild Parser ✅ COMPLETE

- [x] 1.3.4.1 Run parser build script
  ```bash
  ./scripts/build_parser.sh
  ```

- [x] 1.3.4.2 Verify generated `catena_parser.erl` contains new rules

### 1.3 Unit Tests ✅ COMPLETE

- [x] 1.3.T1 Test transform declarations with signatures
- [x] 1.3.T2 Test transform declarations without signatures
- [x] 1.3.T3 Test transform clauses with patterns
- [x] 1.3.T4 Test transform clauses with guards
- [x] 1.3.T5 Test transform clauses with match expressions

**Implementation Status**: Section 1.3 completed on 2024-11-19. See `notes/implementation/language-revamp-section-1.3-summary.md` for details.
**Note**: Most of the technical work was completed during Section 1.2. This phase focused on cleanup and verification.

## 2.1 AST Node Definitions (1 hour)

This section updates the Abstract Syntax Tree (AST) node definitions to use the new naming conventions. The AST is the internal representation of parsed code and must be consistent throughout the compiler.

### 2.1.1 Update AST Record Definitions

- [ ] 2.1.1.1 Update `src/compiler/parser/catena_ast.hrl` record definitions
  - Rename record: `-record(shape_decl, ...)` to `-record(type_decl, ...)`
  - Rename record: `-record(flow_decl, ...)` to `-record(transform_decl, ...)`
  - Rename record: `-record(flow_signature, ...)` to `-record(transform_signature, ...)`
  - Rename record: `-record(flow_clause, ...)` to `-record(transform_clause, ...)`

### 2.1.2 Update AST Smart Constructors

- [ ] 2.1.2.1 Update exports in `src/compiler/ast/catena_ast.erl` (lines 54-56)
  - Change: `shape_decl/5, shape_decl/4,`
  - To: `type_decl/5, type_decl/4,`
  - Change: `flow_decl/4, flow_decl/3,`
  - To: `transform_decl/4, transform_decl/3,`
  - Change: `flow_clause/4, flow_clause/3,`
  - To: `transform_clause/4, transform_clause/3,`

- [ ] 2.1.2.2 Rename constructor functions in module body
  - Rename all `shape_decl` functions to `type_decl`
  - Rename all `flow_decl` functions to `transform_decl`
  - Rename all `flow_clause` functions to `transform_clause`
  - Update all `{flow_sig, ...}` tuples to `{transform_sig, ...}`

### 2.1 Unit Tests

- [ ] 2.1.T1 Test AST node creation for type declarations
- [ ] 2.1.T2 Test AST node creation for transform declarations
- [ ] 2.1.T3 Test AST node creation for transform clauses
- [ ] 2.1.T4 Verify old node types are no longer accepted

## 2.2 AST Utilities and Traversal (1 hour)

This section updates the AST utility functions that traverse and manipulate the syntax tree. These utilities are used throughout the compiler for various analyses and transformations.

### 2.2.1 Update AST Pattern Matching

- [ ] 2.2.1.1 Update `src/compiler/ast/catena_ast_utils.erl` pattern matches
  - Replace all matches: `{shape_decl, ...}` with `{type_decl, ...}`
  - Replace all matches: `{flow_decl, ...}` with `{transform_decl, ...}`
  - Replace all matches: `{flow_clause, ...}` with `{transform_clause, ...}`
  - Replace all matches: `{flow_sig, ...}` with `{transform_sig, ...}`

- [ ] 2.2.1.2 Update `format_decl/1` function cases
  - Add cases for new node types: `type_decl`, `transform_decl`
  - Remove cases for old node types: `shape_decl`, `flow_decl`

### 2.2.2 Update Parser Utilities

- [ ] 2.2.2.1 Update `src/compiler/parser/catena_parse.erl`
  - Search and replace all references to old AST node types
  - Update any hardcoded node construction

- [ ] 2.2.2.2 Update `src/compiler/parser/catena_parser_wrapper.erl`
  - Update keyword suggestions list (line 207)
  - Change: `["shape", "flow", "effect", ...]`
  - To: `["type", "transform", "effect", ...]`

### 2.2.3 Update Compiler Utilities

- [ ] 2.2.3.1 Update `src/compiler/catena_compiler_utils.erl`
  - Replace all references to old AST node types
  - Update any node type checking functions

- [ ] 2.2.3.2 Check `src/compiler/parser/catena_location.erl`
  - Verify no hardcoded references to old node types

### 2.2 Unit Tests

- [ ] 2.2.T1 Test AST traversal with new node types
- [ ] 2.2.T2 Test AST formatting with new node types
- [ ] 2.2.T3 Test parser wrapper keyword suggestions
- [ ] 2.2.T4 Test compiler utilities with new AST

## 3.1 Type System Error Messages (1 hour)

This section updates all error messages and diagnostic output to use the new terminology. Clear error messages are crucial for developer experience.

### 3.1.1 Update Type Error Formatters

- [ ] 3.1.1.1 Update `src/compiler/types/catena_type_error_formatter.erl`
  - Line 205: Change example from `"shape MyType = ... derives [~s]"`
  - To: `"type MyType = ... derives [~s]"`
  - Search for any other references to old keywords in error messages

- [ ] 3.1.1.2 Update `src/compiler/types/catena_type_error.erl`
  - Search for error messages mentioning "shape" or "flow"
  - Update all to use "type" and "transform"

- [ ] 3.1.1.3 Update `src/compiler/types/catena_type_error_explain.erl`
  - Update any explanatory text using old keywords
  - Update example code snippets in error explanations

### 3.1 Unit Tests

- [ ] 3.1.T1 Test error messages for malformed type declarations
- [ ] 3.1.T2 Test error messages for malformed transform declarations
- [ ] 3.1.T3 Test type error messages use correct terminology
- [ ] 3.1.T4 Verify no old keywords appear in error output

## 3.2 Documentation Updates (1 hour)

This section updates all documentation to reflect the new language keywords. Documentation must be consistent with the implementation.

### 3.2.1 Update Project Documentation

- [ ] 3.2.1.1 Update `CLAUDE.md` (lines 100, 137-144)
  - Replace all `shape` examples with `type`
  - Replace all `flow` examples with `transform`
  - Update lexer/parser descriptions

- [ ] 3.2.1.2 Update language examples in documentation
  - Search all `.md` files for Catena code examples
  - Update syntax from `shape User = ...` to `type User = ...`
  - Update syntax from `flow greet : ...` to `transform greet : ...`

### 3.2.2 Update Language Reference

- [ ] 3.2.2.1 Update `notes/language_dictionary.md` if needed
  - Ensure terminology is consistent
  - Update any keyword lists

- [ ] 3.2.2.2 Create migration guide
  - Document the breaking changes
  - Provide before/after examples
  - Include sed scripts for user code migration

### 3.2 Unit Tests

- [ ] 3.2.T1 Verify documentation examples are syntactically valid
- [ ] 3.2.T2 Test that code snippets in docs compile
- [ ] 3.2.T3 Ensure no old keywords remain in documentation

## 4.1 Lexer Test Updates (1 hour)

This section updates all lexer tests to verify the new tokens are recognized and old tokens are rejected.

### 4.1.1 Update Property-Based Tests

- [ ] 4.1.1.1 Update `test/compiler/lexer/catena_lexer_properties.erl`
  - Update keyword generator to use `"type"` instead of `"shape"`
  - Update keyword generator to use `"transform"` instead of `"flow"`
  - Update property descriptions

- [ ] 4.1.1.2 Update negative test cases
  - Add tests verifying `shape` is lexed as identifier, not keyword
  - Add tests verifying `flow` is lexed as identifier, not keyword

### 4.1 Unit Tests

- [ ] 4.1.T1 Test lexer tokenizes `type` keyword correctly
- [ ] 4.1.T2 Test lexer tokenizes `transform` keyword correctly
- [ ] 4.1.T3 Test lexer treats `shape` as regular identifier
- [ ] 4.1.T4 Test lexer treats `flow` as regular identifier

## 4.2 Parser Test Updates - Core (2 hours)

This section updates the core parser tests. With 109+ pattern match occurrences across multiple test files, this is the most substantial testing update required.

### 4.2.1 Update Test Pattern Matches

- [ ] 4.2.1.1 Update `test/compiler/parser/catena_parser_flow_integration_tests.erl`
  - 18+ occurrences of `{flow_decl, ...}` to `{transform_decl, ...}`
  - Update all `{flow_clause, ...}` to `{transform_clause, ...}`
  - Update all `{flow_sig, ...}` to `{transform_sig, ...}`

- [ ] 4.2.1.2 Update `test/compiler/parser/catena_parser_simple_tests.erl`
  - Replace all `{shape_decl, ...}` with `{type_decl, ...}`
  - Replace all `{flow_decl, ...}` with `{transform_decl, ...}`

- [ ] 4.2.1.3 Update `test/compiler/parser/catena_parser_negative_tests.erl`
  - Update expected error messages
  - Update test case names mentioning old keywords

### 4.2.2 Update Additional Parser Tests

- [ ] 4.2.2.1 Update `test/compiler/parser/catena_parser_effect_tests.erl`
  - Update any pattern matches on old AST nodes

- [ ] 4.2.2.2 Update `test/compiler/parser/catena_parser_precedence_tests.erl`
  - Update test cases using old syntax

- [ ] 4.2.2.3 Update `test/compiler/parser/catena_parser_higher_order_types_tests.erl`
  - Update higher-order type examples

### 4.2 Unit Tests

- [ ] 4.2.T1 All parser tests pass with new AST nodes
- [ ] 4.2.T2 Test coverage remains ≥86%
- [ ] 4.2.T3 No tests reference old keywords

## 4.3 Parser Test Updates - Helpers and Integration (1 hour)

This section updates test helper functions and integration tests to use the new AST node types.

### 4.3.1 Update Test Helpers

- [ ] 4.3.1.1 Update `test/compiler/parser/catena_parser_test_helpers.erl`
  - Rename helper functions: `make_flow_decl` to `make_transform_decl`
  - Update return values to use new AST nodes
  - Update any test data generators

- [ ] 4.3.1.2 Update `test/compiler/parser/catena_parse_tests.erl`
  - Update all pattern matches on AST nodes
  - Update test case descriptions

### 4.3.2 Update Additional Test Files

- [ ] 4.3.2.1 Update `test/compiler/ast/catena_ast_utils_tests.erl`
  - Update AST utility tests for new node types

- [ ] 4.3.2.2 Update `test/compiler/catena_compiler_utils_tests.erl`
  - Update compiler utility tests

- [ ] 4.3.2.3 Update `test/compiler/parser/catena_parser_wrapper_tests.erl`
  - Update wrapper tests for new keywords

### 4.3 Unit Tests

- [ ] 4.3.T1 Test helpers generate correct new AST nodes
- [ ] 4.3.T2 Integration tests pass with new syntax
- [ ] 4.3.T3 All test utilities work with new keywords

## 5.1 System Validation (1 hour)

This section performs comprehensive validation of the migrated system to ensure all components work correctly with the new keywords.

### 5.1.1 Run Complete Test Suite

- [ ] 5.1.1.1 Clean and rebuild everything
  ```bash
  make clean
  ./scripts/build.sh
  make compile
  ```

- [ ] 5.1.1.2 Run all tests
  ```bash
  make test
  ```

- [ ] 5.1.1.3 Run property-based tests
  ```bash
  rebar3 proper -m catena_lexer_properties
  rebar3 proper -m catena_parser_properties
  ```

### 5.1.2 Verify Coverage

- [ ] 5.1.2.1 Generate coverage report
  ```bash
  make coverage
  make coverage-report
  ```

- [ ] 5.1.2.2 Verify coverage ≥86%
- [ ] 5.1.2.3 Review uncovered lines for missing test cases

### 5.1 Unit Tests

- [ ] 5.1.T1 All lexer tests pass
- [ ] 5.1.T2 All parser tests pass
- [ ] 5.1.T3 All type system tests pass
- [ ] 5.1.T4 All integration tests pass

## 5.2 Migration Verification (1 hour)

This section verifies that the migration is complete and no references to old keywords remain in the codebase.

### 5.2.1 Search for Remaining Old Keywords

- [ ] 5.2.1.1 Search source files
  ```bash
  grep -r "shape_decl\|flow_decl\|flow_clause\|flow_sig" src/
  ```
  - Verify no matches found

- [ ] 5.2.1.2 Search test files
  ```bash
  grep -r "shape_decl\|flow_decl\|flow_clause\|flow_sig" test/
  ```
  - Verify no matches found

- [ ] 5.2.1.3 Search for keyword tokens
  ```bash
  grep -E "\bshape\b|\bflow\b" src/compiler/lexer/catena_lexer.xrl
  grep -E "\bshape\b|\bflow\b" src/compiler/parser/catena_parser.yrl
  ```
  - Verify no matches found

### 5.2.2 Create Migration Artifacts

- [ ] 5.2.2.1 Create user migration script
  ```bash
  #!/bin/bash
  # migrate_catena_code.sh
  # Updates user code from old to new keywords
  find . -name "*.cat" | xargs sed -i 's/\bshape\b/type/g'
  find . -name "*.cat" | xargs sed -i 's/\bflow\b/transform/g'
  ```

- [ ] 5.2.2.2 Update changelog
  - Document breaking changes
  - List all keyword replacements
  - Reference migration script

- [ ] 5.2.2.3 Tag release
  ```bash
  git tag -a v0.2.0 -m "Language revamp: type and transform keywords"
  ```

### 5.2 Unit Tests

- [ ] 5.2.T1 No old keywords in source code
- [ ] 5.2.T2 No old AST nodes in tests
- [ ] 5.2.T3 Migration script works on sample code
- [ ] 5.2.T4 Documentation is fully updated

## Integration Tests

### I.1 End-to-End Compilation Tests

- [ ] I.1.1 Test compiling simple type declarations
  ```catena
  type User = {name: Text, age: Natural}
  ```

- [ ] I.1.2 Test compiling transform declarations
  ```catena
  transform greet : User -> Text
  transform greet user = "Hello, " <> user.name
  ```

- [ ] I.1.3 Test compiling complex programs with all features
  - Type declarations with variants
  - Transform declarations with pattern matching
  - Trait instances with transform methods
  - Effect handlers with transform signatures

### I.2 Error Reporting Tests

- [ ] I.2.1 Test error messages for malformed type declarations
- [ ] I.2.2 Test error messages for malformed transform declarations
- [ ] I.2.3 Verify error messages use new terminology consistently

### I.3 Parser Recovery Tests

- [ ] I.3.1 Test parser recovery after type declaration errors
- [ ] I.3.2 Test parser recovery after transform declaration errors
- [ ] I.3.3 Verify panic-mode recovery continues parsing correctly

## Success Criteria

- [ ] All occurrences of `shape` keyword replaced with `type`
- [ ] All occurrences of `flow` keyword replaced with `transform`
- [ ] All AST nodes renamed: `shape_decl` → `type_decl`, `flow_*` → `transform_*`
- [ ] All tests updated and passing (100% pass rate)
- [ ] Test coverage maintained at ≥86%
- [ ] Documentation fully updated with new keywords
- [ ] No backward compatibility with old keywords (breaking change)
- [ ] Migration script provided for user code
- [ ] Changelog documenting breaking changes

## Provides Foundation For

This migration provides the foundation for:

1. **Future Language Extensions**: Clean, pragmatic keyword base for new features
2. **Better Developer Experience**: More intuitive keywords for newcomers
3. **Consistent Terminology**: Alignment with pragmatic naming throughout the language
4. **Documentation Clarity**: Simpler explanations without confusing terminology

## Key Outputs

1. **Updated Lexer**: `catena_lexer.xrl` with new token definitions
2. **Updated Parser**: `catena_parser.yrl` with new grammar rules
3. **Updated AST**: New node types throughout the compiler
4. **Updated Tests**: Complete test suite using new syntax
5. **Migration Guide**: Documentation and scripts for user code migration
6. **Breaking Change Documentation**: Clear changelog and version tag