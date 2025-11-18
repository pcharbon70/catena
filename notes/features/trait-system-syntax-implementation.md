# Feature Implementation Plan: Trait System Syntax (Task 1.1.6)

**Status:** 🟡 Planning Complete - Ready for Implementation
**Feature Branch:** `feature/trait-system-syntax`
**Phase:** Phase 1 - Core Language Infrastructure
**Task:** 1.1.6 Trait System Syntax

---

## 1. Problem Statement

### Description
Catena needs lexer and parser support for its trait system (type classes), which forms the foundation of the category-theory-first approach. Without this syntax, the compiler cannot parse trait declarations (`trait Functor f where ...`), instance implementations (`instance Functor Maybe where ...`), or trait hierarchies (`trait Monad m extends Applicative m`).

### Current State
- **Lexer**: `trait` and `instance` keywords exist (lines 73-74 of catena_lexer.xrl), but `extends` keyword is missing
- **AST**: Basic `trait_decl` record exists (catena_ast.hrl lines 78-83) but lacks:
  - `extends` field for trait hierarchies
  - `default_methods` field for default implementations
  - Complete `instance_decl` record for trait instances
- **Parser**: `trait` and `instance` terminals declared (catena_parser.yrl line 49) but no grammar rules implemented

### Impact
This blocks implementation of:
- Standard library trait definitions (Setoid, Functor, Monad, etc.)
- Trait instances for core types (Maybe, List, Result)
- Trait hierarchy (Applicative extends Functor, Monad extends Applicative)
- Phase 2 prelude development which requires trait system

---

## 2. Solution Overview

### High-Level Approach
Extend the existing lexer/parser infrastructure to fully support trait system syntax in four steps:

1. **Lexer Enhancement**: Add `extends` keyword
2. **AST Extension**: Add missing fields to `trait_decl` and create `instance_decl` record
3. **Parser Grammar**: Implement grammar rules for trait/instance declarations
4. **Testing**: Create comprehensive parser tests for all syntax variants

### Key Design Decisions

#### AST Structure for Trait Declarations
```erlang
-record(trait_decl, {
    name :: atom(),                           % Trait name (e.g., Functor)
    type_params :: [atom()],                  % Type parameters (e.g., [f])
    extends :: [trait_constraint()] | undefined,  % NEW: Trait hierarchy
    methods :: [{atom(), type_expr()}],       % Method signatures
    default_methods :: [{atom(), expr()}] | undefined,  % NEW: Default implementations
    location :: location()
}).

%% NEW: Trait constraint for extends clauses
-record(trait_constraint, {
    trait :: atom(),                          % Trait name (e.g., Applicative)
    type_args :: [type_expr()],               % Type arguments (e.g., [m])
    location :: location()
}).
```

#### AST Structure for Instance Declarations
```erlang
%% NEW: Instance declaration
-record(instance_decl, {
    trait :: atom(),                          % Trait being implemented
    type_args :: [type_expr()],               % Type arguments
    constraints :: [trait_constraint()] | undefined,  % Context constraints
    methods :: [{atom(), expr()}],            % Method implementations
    location :: location()
}).
```

### Grammar Design Principles
- Follow existing Catena grammar conventions
- Use `where` keyword to delimit method blocks (consistent with language design)
- Support optional `extends` for trait hierarchies
- Allow multiple extends constraints separated by commas

---

## 3. Agent Consultations Performed

### Research on Erlang leex/yecc
- **leex** (Erlang lexical analyzer): Pattern-based lexer generator, keyword rules follow format `keyword : {token, {keyword, TokenLine}}.`
- **yecc** (Erlang parser generator): LALR parser using BNF-style grammar rules
- Nonterminal definitions required before use in grammar rules
- Actions use Erlang expressions to construct AST nodes

### Comparison with Similar Languages
- **Haskell**: `class Functor f where fmap :: (a -> b) -> f a -> f b`
- **Rust**: `trait Functor<F> { fn fmap<A, B>(...) -> ...; }`
- **Scala**: `trait Functor[F] { def fmap[A, B](...): ... }`
- **Catena Design**: Combines Haskell-like syntax with explicit `where` blocks

---

## 4. Technical Details

### Files to Modify

#### 4.1 Lexer: `src/compiler/lexer/catena_lexer.xrl`
**Location**: After line 77 (after `supervisor` keyword)
**Changes**:
```erlang
%% Add extends keyword
extends : {token, {extends, TokenLine}}.
```

#### 4.2 AST Header: `src/compiler/parser/catena_ast.hrl`
**Location**: Lines 78-83 (trait_decl record)
**Changes**:
1. Update `trait_decl` record to add `extends` and `default_methods` fields
2. Add new `trait_constraint` record after `trait_decl`
3. Add new `instance_decl` record
4. Update `declaration()` type to include `#instance_decl{}`

**Detailed Changes**:
```erlang
%% Line 78: Update trait_decl
-record(trait_decl, {
    name :: atom(),
    type_params :: [atom()],
    extends :: [trait_constraint()] | undefined,  % NEW
    methods :: [{atom(), type_expr()}],
    default_methods :: [{atom(), expr()}] | undefined,  % NEW
    location :: location()
}).

%% NEW: After trait_decl (around line 84)
-record(trait_constraint, {
    trait :: atom(),
    type_args :: [type_expr()],
    location :: location()
}).

%% NEW: Instance declaration
-record(instance_decl, {
    trait :: atom(),
    type_args :: [type_expr()],
    constraints :: [trait_constraint()] | undefined,
    methods :: [{atom(), expr()}],
    location :: location()
}).

%% Line 138: Update declaration type
-type declaration() :: #shape_decl{} | #flow_decl{} | #trait_decl{} |
                       #instance_decl{} | #effect_decl{}.  % Added #instance_decl{}
```

#### 4.3 Parser: `src/compiler/parser/catena_parser.yrl`
**Location**: Multiple sections

**4.3.1 Nonterminals** (after line 22)
```erlang
Nonterminals
  ... (existing)
  trait_decl instance_decl                          % NEW
  trait_extends trait_extends_list trait_constraint % NEW
  trait_methods trait_method                        % NEW
  instance_methods instance_method                  % NEW
  .
```

**4.3.2 Terminals** (line 49 - already declared)
```erlang
trait instance forall  % Already present
extends                % Need to add to this line
```

**4.3.3 Grammar Rules** (after shape_decl and flow_decl rules)

```erlang
%% ========================================
%% Trait Declaration
%% ========================================

%% Simple trait: trait Functor f where methods
trait_decl -> trait upper_ident type_params_nonempty where trait_methods 'end' :
    #trait_decl{
        name = extract_atom('$2'),
        type_params = '$3',
        extends = undefined,
        methods = '$5',
        default_methods = undefined,
        location = extract_location('$1')
    }.

%% Trait with extends: trait Monad m extends Applicative m where methods
trait_decl -> trait upper_ident type_params_nonempty extends trait_extends_list where trait_methods 'end' :
    #trait_decl{
        name = extract_atom('$2'),
        type_params = '$3',
        extends = '$5',
        methods = '$7',
        default_methods = undefined,
        location = extract_location('$1')
    }.

%% Trait extends list (comma-separated)
trait_extends_list -> trait_constraint : ['$1'].
trait_extends_list -> trait_constraint comma trait_extends_list : ['$1' | '$3'].

%% Trait constraint: TraitName type_args
trait_constraint -> upper_ident type_expr_app :
    #trait_constraint{
        trait = extract_atom('$1'),
        type_args = ['$2'],
        location = extract_location('$1')
    }.

trait_constraint -> upper_ident lower_ident :
    #trait_constraint{
        trait = extract_atom('$1'),
        type_args = [#type_var{name = extract_atom('$2'), location = extract_location('$2')}],
        location = extract_location('$1')
    }.

%% Trait methods (method signatures)
trait_methods -> trait_method : ['$1'].
trait_methods -> trait_method trait_methods : ['$1' | '$2'].

trait_method -> lower_ident colon type_expr :
    {extract_atom('$1'), '$3'}.

%% ========================================
%% Instance Declaration
%% ========================================

%% Simple instance: instance Functor Maybe where methods
instance_decl -> instance upper_ident type_expr_primary where instance_methods 'end' :
    #instance_decl{
        trait = extract_atom('$2'),
        type_args = ['$3'],
        constraints = undefined,
        methods = '$5',
        location = extract_location('$1')
    }.

%% Instance with multiple type args: instance Functor (List a) where methods
instance_decl -> instance upper_ident type_expr_app where instance_methods 'end' :
    #instance_decl{
        trait = extract_atom('$2'),
        type_args = ['$3'],
        constraints = undefined,
        methods = '$5',
        location = extract_location('$1')
    }.

%% Instance methods (method implementations)
instance_methods -> instance_method : ['$1'].
instance_methods -> instance_method instance_methods : ['$1' | '$2'].

instance_method -> lower_ident equals expr :
    {extract_atom('$1'), '$3'}.

%% ========================================
%% Update declaration rule
%% ========================================

declaration -> trait_decl : '$1'.
declaration -> instance_decl : '$1'.
```

**4.3.4 Helper Functions** (Erlang code section at end of .yrl file)
```erlang
extract_atom({_Tag, _Line, Value}) -> Value;
extract_atom({_Tag, _Line}) -> undefined;
extract_atom(Atom) when is_atom(Atom) -> Atom.

extract_location({_Tag, Line}) -> {line, Line};
extract_location({_Tag, Line, _Value}) -> {line, Line}.
```

### Dependencies
- **Build system**: rebar3 compilation of .xrl and .yrl files
- **Testing**: Will need to create test files in `test/compiler/parser/`

### Configuration
No configuration changes required. The lexer and parser regenerate during compilation.

---

## 5. Success Criteria

### Critical Completion Requirements

**No feature is complete without working tests:**
- ✅ All subtasks implemented
- ✅ Lexer regenerated successfully (`.xrl` → `_gen.erl`)
- ✅ Parser regenerated successfully (`.yrl` → `.erl`)
- ✅ All parser tests pass
- ✅ Examples from task requirements successfully parse

### Specific Test Cases (Minimum Required)

#### Test 1: Simple Trait Declaration
```catena
trait Functor f where
  fmap : (a -> b) -> f a -> f b
end
```
**Expected**: `#trait_decl{name=Functor, type_params=[f], extends=undefined, methods=[{fmap, ...}], ...}`

#### Test 2: Trait with Extends
```catena
trait Monad m extends Applicative m where
  bind : m a -> (a -> m b) -> m b
end
```
**Expected**: `#trait_decl{name=Monad, extends=[#trait_constraint{trait=Applicative, ...}], ...}`

#### Test 3: Simple Instance
```catena
instance Functor Maybe where
  fmap f = match
    | None -> None
    | Some x -> Some (f x)
  end
end
```
**Expected**: `#instance_decl{trait=Functor, type_args=[Maybe type], methods=[{fmap, ...}], ...}`

#### Test 4: Instance with Type Application
```catena
instance Functor List where
  fmap f = match
    | Nil -> Nil
    | Cons x xs -> Cons (f x) (fmap f xs)
  end
end
```
**Expected**: Successful parse with proper type_args

### Feature Verification
- ✅ Lexer recognizes `extends` keyword
- ✅ Parser generates correct AST for trait declarations
- ✅ Parser generates correct AST for instance declarations
- ✅ Parser handles trait hierarchies with `extends`
- ✅ Error messages for malformed trait/instance syntax are clear

---

## 6. Implementation Plan

### Step 1: Add `extends` Keyword to Lexer ⏱️ 5 minutes
**File**: `src/compiler/lexer/catena_lexer.xrl`

**Subtasks**:
1. [ ] Add `extends : {token, {extends, TokenLine}}.` after line 77
2. [ ] Regenerate lexer: `cd src/compiler/lexer && erlc +'{outdir, "."}'  catena_lexer.xrl`
3. [ ] Verify lexer compiles without errors
4. [ ] Test: Create simple test that lexer tokenizes `extends` correctly

**Success**: Lexer recognizes `extends` as a keyword token

---

### Step 2: Extend AST Definitions ⏱️ 15 minutes
**File**: `src/compiler/parser/catena_ast.hrl`

**Subtasks**:
1. [ ] Update `trait_decl` record (line 78):
   - Add `extends :: [trait_constraint()] | undefined` field
   - Add `default_methods :: [{atom(), expr()}] | undefined` field
2. [ ] Add `trait_constraint` record definition after `trait_decl`
3. [ ] Add `instance_decl` record definition
4. [ ] Add type definitions:
   - `trait_constraint()` type
   - `instance_decl()` type
5. [ ] Update `declaration()` type to include `#instance_decl{}`
6. [ ] Recompile AST header and verify no syntax errors

**Success**: AST header compiles, new records defined correctly

---

### Step 3: Add Parser Grammar Rules ⏱️ 45 minutes
**File**: `src/compiler/parser/catena_parser.yrl`

**Subtasks**:
1. [ ] Add nonterminals (line ~22):
   - `trait_decl instance_decl`
   - `trait_extends_list trait_constraint`
   - `trait_methods trait_method`
   - `instance_methods instance_method`

2. [ ] Add `extends` to terminals list (line 49)

3. [ ] Add trait declaration grammar rules:
   - [ ] Simple trait (no extends)
   - [ ] Trait with extends
   - [ ] Trait extends list (comma-separated)
   - [ ] Trait constraint parsing
   - [ ] Trait methods parsing
   - [ ] Trait method (signature) parsing

4. [ ] Add instance declaration grammar rules:
   - [ ] Simple instance
   - [ ] Instance with type application
   - [ ] Instance methods parsing
   - [ ] Instance method (implementation) parsing

5. [ ] Update `declaration` rule to include `trait_decl` and `instance_decl`

6. [ ] Add helper functions at end of file:
   - [ ] `extract_atom/1`
   - [ ] `extract_location/1`

7. [ ] Regenerate parser: `cd src/compiler/parser && erlc +'{outdir, "."}'  catena_parser.yrl`

8. [ ] Verify parser compiles without errors
9. [ ] Check shift/reduce conflict count hasn't increased significantly

**Success**: Parser regenerates successfully, grammar rules compile

---

### Step 4: Create Comprehensive Parser Tests ⏱️ 30 minutes
**File**: `test/compiler/parser/catena_parser_trait_test.erl`

**Subtasks**:
1. [ ] Create new test file with module structure
2. [ ] Add test: `test_simple_trait_declaration/0`
   - Parse `trait Functor f where fmap : (a -> b) -> f a -> f b end`
   - Assert AST structure matches expected
3. [ ] Add test: `test_trait_with_extends/0`
   - Parse `trait Monad m extends Applicative m where bind : m a -> (a -> m b) -> m b end`
   - Assert extends field populated correctly
4. [ ] Add test: `test_trait_multiple_extends/0`
   - Parse trait extending multiple traits
   - Assert all extends constraints present
5. [ ] Add test: `test_simple_instance_declaration/0`
   - Parse `instance Functor Maybe where fmap f = expr end`
   - Assert instance AST correct
6. [ ] Add test: `test_instance_with_type_app/0`
   - Parse `instance Functor List where ...`
   - Assert type arguments correct
7. [ ] Add test: `test_instance_multiple_methods/0`
   - Parse instance with multiple method implementations
8. [ ] Add negative tests (malformed syntax)
9. [ ] Run all tests: `rebar3 eunit --module=catena_parser_trait_test`

**Success**: All tests pass, coverage includes success and error cases

---

### Step 5: Integration Testing ⏱️ 15 minutes

**Subtasks**:
1. [ ] Test full parse pipeline (lexer → parser → AST)
2. [ ] Verify examples from Phase 1 requirements parse correctly:
   - [ ] `trait Functor f where fmap : (a -> b) -> f a -> f b`
   - [ ] `instance Functor Maybe where fmap f = match | None -> None | Some x -> Some (f x) end`
   - [ ] `trait Monad m extends Applicative m where bind : m a -> (a -> m b) -> m b`
3. [ ] Run full parser test suite: `rebar3 eunit --dir=test/compiler/parser`
4. [ ] Document any discovered issues or limitations

**Success**: All integration tests pass, examples parse successfully

---

### Step 6: Documentation and Cleanup ⏱️ 10 minutes

**Subtasks**:
1. [ ] Update this planning document with completion status
2. [ ] Create implementation summary document
3. [ ] Mark completed subtasks in Phase 1 planning document
4. [ ] Document any deviations from original plan
5. [ ] Note any discovered limitations or future improvements needed

**Success**: Documentation complete, ready for review and commit

---

## 7. Notes and Considerations

### Edge Cases to Handle
1. **Empty method lists**: Should traits without methods be allowed? (Yes, for marker traits)
2. **Duplicate method names**: Parser should allow (type checker will catch)
3. **Complex type expressions in extends**: Need to handle `extends Functor (Either a)` style constraints
4. **Whitespace sensitivity**: Erlang lexer handles whitespace, but verify `end` keyword parsing

### Known Limitations (Deferred to Later Phases)
1. **Default method implementations**: AST field added but grammar not implemented (deferred to Phase 6)
2. **Instance constraints**: `instance (Functor f, Functor g) => Functor (Compose f g)` syntax (deferred to Phase 1.2 - Type System)
3. **Associated types**: Not part of current design (future consideration)
4. **Multi-parameter type classes**: Supported in AST but not prioritized for PoC

### Risks and Mitigation
- **Risk**: Parser shift/reduce conflicts increase significantly
  - **Mitigation**: Careful grammar design, test incrementally
- **Risk**: Grammar ambiguity with existing constructs
  - **Mitigation**: Review existing grammar rules, use explicit delimiters (`end` keyword)
- **Risk**: Breaking existing parser tests
  - **Mitigation**: Run existing tests after each change, fix regressions immediately

### Future Improvements
1. **Syntax sugar for single-method traits**: Allow shorthand syntax
2. **Inline trait constraints in type signatures**: `flow map : Functor f => (a -> b) -> f a -> f b`
3. **Deriving instances**: Automatic instance generation for common traits

---

## 8. Current Status

### What Works
- Planning document complete
- Design decisions finalized
- File locations identified
- Test cases specified

### What's Next
- Begin Step 1: Add `extends` keyword to lexer
- Follow implementation plan sequentially
- Update this document as tasks complete

### How to Run
```bash
# After implementation:
cd /home/ducky/code/catena

# Regenerate lexer
cd src/compiler/lexer
erlc +'{outdir, "."}'  catena_lexer.xrl

# Regenerate parser
cd ../parser
erlc +'{outdir, "."}'  catena_parser.yrl

# Run tests
cd ../..
rebar3 eunit --module=catena_parser_trait_test

# Run all parser tests
rebar3 eunit --dir=test/compiler/parser
```

---

## Completion Checklist

- [ ] Step 1: Lexer keyword added
- [ ] Step 2: AST extended
- [ ] Step 3: Parser grammar implemented
- [ ] Step 4: Tests created and passing
- [ ] Step 5: Integration testing complete
- [ ] Step 6: Documentation updated
- [ ] All tests passing
- [ ] Phase 1 planning document updated
- [ ] Implementation summary created
- [ ] Ready for code review and commit
