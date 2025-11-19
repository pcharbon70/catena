# Language Revamp Migration Plan

## Overview

This document outlines the comprehensive migration plan to update the Catena codebase to align with the revamped language specification. The primary changes involve updating keywords and terminology to use more pragmatic, developer-friendly names while maintaining the underlying category theory foundation.

## Keyword Changes Summary

| Old Keyword | New Keyword | Purpose |
|------------|------------|---------|
| `shape` | `type` | Define data types (algebraic data types) |
| `flow` | `transform` | Define pure functions (morphisms) |
| `module` | `system` | Define modules/categories (already exists, keeping both) |

**Note**: The `module` keyword is still present in the codebase alongside `system`, both are valid. The `process`, `adapter`, `operator`, `doc`, `property`, `laws`, and `extends` keywords are already implemented.

## Impact Analysis

### Total Scope
- **Source files affected**: 12+ core compiler files
- **Test files affected**: 15+ test modules
- **Documentation files affected**: 4+ markdown files
- **Total estimated changes**: 400-600 locations

### Risk Assessment
- **High Risk**: Parser and lexer changes affect all code parsing
- **Medium Risk**: AST changes affect type system and code generation
- **Low Risk**: Documentation and test updates

## Migration Phases

### Phase 1: Core Grammar Updates (Critical Path)

#### 1.1 Lexer Updates (`src/compiler/lexer/catena_lexer.xrl`)
- [ ] Update line 5 comment: Keywords list
- [ ] Line 51: Change `shape` token to `type`
- [ ] Line 52: Change `flow` token to `transform`

#### 1.2 Parser Grammar (`src/compiler/parser/catena_parser.yrl`)
- [ ] Update nonterminals (lines 22, 30)
- [ ] Update terminals (line 51)
- [ ] Update declaration rules (lines 190-194)
- [ ] Update error recovery rules (lines 197-206)
- [ ] Rename entire `shape_decl` section to `type_decl` (lines 208-226)
- [ ] Update trait default methods (line 358)
- [ ] Update instance methods (lines 404, 407)
- [ ] Rename entire `flow_decl` section to `transform_decl` (lines 410-486)
- [ ] Rename helper functions `extract_flow_*` to `extract_transform_*` (lines 904, 907)

#### 1.3 Rebuild Generated Files
```bash
# After lexer/parser updates
./scripts/build.sh
# or
make clean && make compile
```

### Phase 2: AST and Type System Updates

#### 2.1 AST Node Definitions (`src/compiler/parser/catena_ast.hrl`)
- [ ] Rename `shape_decl` record to `type_decl`
- [ ] Rename `flow_decl` record to `transform_decl`
- [ ] Rename `flow_signature` record to `transform_signature`
- [ ] Rename `flow_clause` record to `transform_clause`

#### 2.2 AST Smart Constructors (`src/compiler/ast/catena_ast.erl`)
- [ ] Update export declarations (lines 54-56)
- [ ] Rename all `shape_decl` functions to `type_decl`
- [ ] Rename all `flow_decl` functions to `transform_decl`
- [ ] Rename all `flow_clause` functions to `transform_clause`
- [ ] Update all `flow_sig` tuples to `transform_sig`

#### 2.3 AST Utilities (`src/compiler/ast/catena_ast_utils.erl`)
- [ ] Update pattern matches for new AST node types
- [ ] Update `format_decl/1` cases

#### 2.4 Parser Utilities
- [ ] Update `src/compiler/parser/catena_parse.erl`
- [ ] Update `src/compiler/parser/catena_parser_wrapper.erl` (line 207: keyword suggestions)
- [ ] Check `src/compiler/parser/catena_location.erl`

#### 2.5 Compiler Utilities (`src/compiler/catena_compiler_utils.erl`)
- [ ] Update references to old AST node types

### Phase 3: Error Messages and Documentation

#### 3.1 Type Error System
- [ ] Update `src/compiler/types/catena_type_error.erl`
- [ ] Update `src/compiler/types/catena_type_error_formatter.erl` (line 205)
- [ ] Update `src/compiler/types/catena_type_error_explain.erl`

#### 3.2 Documentation Updates
- [ ] Update `CLAUDE.md` (lines 100, 137-144)
- [ ] Update code examples in `notes/` directory
- [ ] Update `notes/language_dictionary.md` if needed

### Phase 4: Test Suite Updates

#### 4.1 Lexer Tests
- [ ] Update `test/compiler/lexer/catena_lexer_properties.erl`
  - Update keyword lists in generators
  - Update property descriptions

#### 4.2 Parser Tests (Critical - 109+ pattern matches)
- [ ] `catena_parser_negative_tests.erl`
- [ ] `catena_parser_flow_integration_tests.erl` (18+ occurrences)
- [ ] `catena_parser_simple_tests.erl`
- [ ] `catena_parser_effect_tests.erl`
- [ ] `catena_parser_precedence_tests.erl`
- [ ] `catena_parser_higher_order_types_tests.erl`
- [ ] `catena_parse_tests.erl`
- [ ] `catena_parser_test_helpers.erl`
- [ ] `catena_parser_wrapper_tests.erl`

#### 4.3 AST Tests
- [ ] `catena_ast_utils_tests.erl`
- [ ] `catena_compiler_utils_tests.erl`

#### 4.4 Test Patterns to Update
- All `{shape_decl, ...}` → `{type_decl, ...}`
- All `{flow_decl, ...}` → `{transform_decl, ...}`
- All `{flow_clause, ...}` → `{transform_clause, ...}`
- All `{flow_sig, ...}` → `{transform_sig, ...}`
- Test case names and descriptions
- Helper function names

### Phase 5: Validation and Testing

#### 5.1 Incremental Testing Strategy
```bash
# After each phase, run tests to catch issues early

# Phase 1: Test lexer
rebar3 eunit --module=catena_lexer_tests
rebar3 proper -m catena_lexer_properties

# Phase 2: Test parser
rebar3 eunit --module=catena_parser_tests
rebar3 proper -m catena_parser_properties

# Phase 3: Full test suite
make test

# Phase 4: Coverage check
make coverage
```

#### 5.2 Validation Checklist
- [ ] All lexer tests pass
- [ ] All parser tests pass
- [ ] Property-based tests pass
- [ ] Type system tests pass
- [ ] Integration tests pass
- [ ] Coverage remains ≥86%

## Implementation Strategy

### Automated Refactoring Approach

Given the systematic nature of these changes, we can use a combination of:

1. **Find and Replace with Regex**:
```bash
# Example for source files
find src/ -name "*.erl" -o -name "*.hrl" | xargs sed -i 's/shape_decl/type_decl/g'
find src/ -name "*.erl" -o -name "*.hrl" | xargs sed -i 's/flow_decl/transform_decl/g'
find src/ -name "*.erl" -o -name "*.hrl" | xargs sed -i 's/flow_clause/transform_clause/g'
find src/ -name "*.erl" -o -name "*.hrl" | xargs sed -i 's/flow_sig/transform_sig/g'
```

2. **Manual Updates for Complex Cases**:
- Parser grammar rules (require careful updating)
- Error messages (need contextual review)
- Comments and documentation

3. **Validation Script**:
```bash
#!/bin/bash
# validate_migration.sh

echo "Checking for old keywords in source..."
grep -r "shape_decl\|flow_decl\|flow_clause\|flow_sig" src/
if [ $? -eq 0 ]; then
    echo "WARNING: Old keywords still found in source"
fi

echo "Running tests..."
make test
```

### Rollback Strategy

1. **Git Branch Protection**:
```bash
# Create migration branch
git checkout -b language-revamp-migration

# Make changes incrementally with commits after each phase
git commit -m "Phase 1: Update lexer and parser grammar"
git commit -m "Phase 2: Update AST and type system"
# etc.
```

2. **Backup Critical Files**:
```bash
# Before starting
cp src/compiler/lexer/catena_lexer.xrl{,.backup}
cp src/compiler/parser/catena_parser.yrl{,.backup}
```

## Timeline Estimate

| Phase | Duration | Dependencies |
|-------|----------|-------------|
| Phase 1: Core Grammar | 2-3 hours | None |
| Phase 2: AST & Type System | 2-3 hours | Phase 1 |
| Phase 3: Error Messages & Docs | 1-2 hours | Phase 2 |
| Phase 4: Test Suite | 3-4 hours | Phase 2 |
| Phase 5: Validation | 1-2 hours | All phases |
| **Total** | **9-14 hours** | |

## Success Criteria

1. **Functional Requirements**:
   - [ ] All tests pass with new keywords
   - [ ] Parser correctly recognizes `type` instead of `shape`
   - [ ] Parser correctly recognizes `transform` instead of `flow`
   - [ ] AST generation works with new node types
   - [ ] Type inference works with new AST nodes

2. **Quality Requirements**:
   - [ ] Test coverage remains ≥86%
   - [ ] No performance degradation
   - [ ] Error messages are clear and use new terminology
   - [ ] Documentation is consistent with implementation

3. **Backwards Compatibility**:
   - Note: This is a breaking change. Old Catena source files using `shape` and `flow` will need to be updated.

## Risk Mitigation

1. **Parser Generation Failures**:
   - Keep backup of working `.yrl` and `.xrl` files
   - Test changes incrementally
   - Have rollback branch ready

2. **Hidden Dependencies**:
   - Search for string literals containing old keywords
   - Check error message generation code
   - Review property-based test generators

3. **Test Breakage Cascade**:
   - Update test helpers first
   - Fix one test module at a time
   - Use `--module` flag to test incrementally

## Post-Migration Tasks

1. **Documentation**:
   - [ ] Update README if it contains examples
   - [ ] Create migration guide for users
   - [ ] Update language specification documents

2. **Tooling**:
   - [ ] Update any syntax highlighters
   - [ ] Update any IDE plugins
   - [ ] Update any code formatters

3. **Communication**:
   - [ ] Document breaking changes
   - [ ] Update changelog
   - [ ] Consider semantic versioning implications

## Appendix: File Change Mapping

### Quick Reference for Developers

```erlang
% Old AST pattern
{shape_decl, Line, Name, Params, Constructors}
% New AST pattern
{type_decl, Line, Name, Params, Constructors}

% Old AST pattern
{flow_decl, Line, Name, Type, Clauses}
% New AST pattern
{transform_decl, Line, Name, Type, Clauses}

% Old AST pattern
{flow_clause, Line, Name, Patterns, Body}
% New AST pattern
{transform_clause, Line, Name, Patterns, Body}

% Old AST pattern
{flow_sig, Name, Type}
% New AST pattern
{transform_sig, Name, Type}
```

### Grep Commands for Verification

```bash
# Find remaining old keywords in source
grep -r "shape_decl\|flow_decl\|flow_clause\|flow_sig" src/

# Find remaining old keywords in tests
grep -r "shape_decl\|flow_decl\|flow_clause\|flow_sig" test/

# Find lexer tokens
grep -E "shape|flow" src/compiler/lexer/catena_lexer.xrl

# Find parser rules
grep -E "shape_|flow_" src/compiler/parser/catena_parser.yrl
```

## Conclusion

This migration represents a significant but manageable refactoring of the Catena compiler. The changes are systematic and follow clear patterns, making them suitable for semi-automated refactoring. The key to success will be:

1. Following the phases in order
2. Testing incrementally after each phase
3. Maintaining git history for easy rollback
4. Being thorough in searching for all occurrences

The pragmatic naming approach (`type` and `transform` instead of `shape` and `flow`) will make the language more accessible to developers while maintaining the strong category theory foundation that makes Catena unique.