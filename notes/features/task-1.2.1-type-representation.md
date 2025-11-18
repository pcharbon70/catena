# Feature Planning: Task 1.2.1 - Type Representation with Effect Sets

**Task ID:** 1.2.1
**Phase:** 1 (Core Language Infrastructure)
**Section:** 1.2 (Core Type System)
**Created:** 2025-11-13
**Updated:** 2025-11-16
**Status:** IMPLEMENTED

---

## Problem Statement

We need to implement the internal type representation for the Catena type-and-effect inference system. This is the foundation for implementing Algorithm W (Hindley-Milner type inference) in Task 1.2.2.

### Current State

- ✅ Parser generates AST with type expression nodes (`type_var`, `type_con`, `type_fun`, etc.)
- ✅ AST nodes include location metadata for error reporting
- ✅ Internal type representation implemented in `catena_types.erl` (746 lines)
- ✅ Type substitution operations implemented in `catena_type_subst.erl` (237 lines)
- ✅ Type scheme representation implemented in `catena_type_scheme.erl` (260 lines)
- ✅ Pretty-printing implemented in `catena_type_pp.erl` (241 lines)
- ✅ Type environments implemented in `catena_type_env.erl` (230+ lines)
- ✅ Type state management implemented in `catena_type_state.erl` (90+ lines)
- ✅ Type error representation implemented in `catena_type_error.erl` (320+ lines)
- ✅ Effect set integration with function types fully implemented

### Requirements

From phase-01.md, Task 1.2.1 must implement:

1. **Type term representation** with type variables, constructors, functions, records, and variants
2. **Type substitution operations** for unification and instantiation
3. **Type scheme representation** for polymorphic types with quantified variables
4. **Type pretty-printing** for error messages and REPL output
5. **Effect set integration** where function types include effect annotations

### Impact

This task is **critical** because:
- All subsequent type system work (1.2.2-1.2.5) depends on this foundation
- Without proper type representation, we cannot implement type inference
- Effect tracking must be integrated from the start (not retrofitted later)
- Type substitutions are core to Robinson's unification algorithm

---

## Solution Overview

### Design Decision: Separate AST and Type Representations

We will maintain **two separate type representations**:

1. **AST Type Expressions** (`catena_ast.hrl`):
   - Used during parsing
   - Includes location metadata
   - Full surface syntax (forall, effect annotations, etc.)

2. **Internal Types** (`catena_types` module):
   - Used during type inference
   - Optimized for Algorithm W operations
   - Includes fresh type variables and substitutions
   - Simpler structure without location metadata

**Rationale:** This separation follows best practices from ML/Haskell compilers (OCaml, GHC). The internal representation can evolve independently and is optimized for the inference algorithm's needs.

### Key Design Principles

1. **Immutability:** All type operations create new types (functional style)
2. **Fresh Variables:** Type variables use unique integer IDs (not symbols)
3. **Effect Sets:** Represented as sorted lists for deterministic operations
4. **Principled Types:** Support for most general types via type schemes

---

## Technical Details

### Module Structure

We will create these new modules:

```
src/compiler/types/
├── catena_types.erl          # Core type representation and operations
├── catena_type_subst.erl     # Substitution operations
├── catena_type_scheme.erl    # Type schemes (polymorphic types)
├── catena_type_env.erl       # Type environments
└── catena_type_pp.erl        # Pretty-printing
```

### Internal Type Representation

**File:** `src/compiler/types/catena_types.erl`

```erlang
%% Type representation optimized for Algorithm W
-type type_var_id() :: pos_integer().

-type base_type() :: integer | float | string | atom | boolean | unit.

-type ty() :: {tvar, type_var_id()}                    % Type variable
            | {tcon, atom()}                            % Type constructor (List, Maybe, etc.)
            | {tapp, ty(), [ty()]}                      % Type application (List Int)
            | {tfun, ty(), ty(), effect_set()}          % Function type with effects
            | {trecord, [{atom(), ty()}], row_var()}    % Record type (row polymorphism)
            | {ttuple, [ty()]}                          % Tuple type
            | {tvariant, [{atom(), [ty()]}]}.           % Variant type

-type effect_set() :: {effect_set, [atom()]}.  % Sorted list of effect names
-type row_var() :: type_var_id() | closed.    % Row variable or closed record

%% Type schemes (polymorphic types)
-record(type_scheme, {
    quantified :: [type_var_id()],  % Universally quantified type variables
    constraints :: [constraint()],   % Type class constraints
    body :: ty()                     % The actual type
}).

-type type_scheme() :: #type_scheme{} | ty().  % Scheme or monomorphic type
```

**Key Features:**
- Type variables use integer IDs for easy fresh generation
- Function types `{tfun, Param, Return, Effects}` include effect sets
- Effect sets are normalized (sorted, deduplicated)
- Row polymorphism for extensible records
- Type schemes separate quantified vars from body

### Type Substitution

**File:** `src/compiler/types/catena_type_subst.erl`

```erlang
%% Substitution: mapping from type variables to types
-type subst() :: #{type_var_id() => ty()}.

%% Core operations
-spec empty() -> subst().
-spec singleton(type_var_id(), ty()) -> subst().
-spec compose(subst(), subst()) -> subst().
-spec apply(subst(), ty()) -> ty().
-spec apply_scheme(subst(), type_scheme()) -> type_scheme().
```

**Implementation Strategy:**
- Use Erlang maps for efficient lookup
- `compose(S2, S1)` applies S1 then S2 (standard composition)
- `apply(Subst, Type)` recursively substitutes type variables
- Handle occurs-check during unification (not in substitution)

### Type Schemes and Generalization

**File:** `src/compiler/types/catena_type_scheme.erl`

```erlang
%% Create type scheme from type by quantifying free variables
-spec generalize(type_env(), ty()) -> type_scheme().

%% Instantiate type scheme with fresh type variables
-spec instantiate(type_scheme()) -> ty().

%% Get free type variables in a type
-spec ftv(ty()) -> sets:set(type_var_id()).
-spec ftv_scheme(type_scheme()) -> sets:set(type_var_id()).
```

**Key Operations:**
- `generalize(Env, Type)` finds type variables not in Env and quantifies them
- `instantiate(Scheme)` replaces quantified vars with fresh ones
- Supports Algorithm W's let-polymorphism

### Type Environments

**File:** `src/compiler/types/catena_type_env.erl`

```erlang
%% Type environment: maps variable names to type schemes
-type type_env() :: #{atom() => type_scheme()}.

-spec empty() -> type_env().
-spec extend(type_env(), atom(), type_scheme()) -> type_env().
-spec lookup(type_env(), atom()) -> {ok, type_scheme()} | error.
-spec remove(type_env(), atom()) -> type_env().
-spec ftv(type_env()) -> sets:set(type_var_id()).
```

### Pretty-Printing

**File:** `src/compiler/types/catena_type_pp.erl`

```erlang
%% Pretty-print types for error messages
-spec pp_type(ty()) -> iolist().
-spec pp_scheme(type_scheme()) -> iolist().
-spec pp_effect_set(effect_set()) -> iolist().

%% Convert internal type to user-friendly string
-spec type_to_string(ty()) -> string().
```

**Output Examples:**
```
Int                           % Base type
a                             % Type variable
List a                        % Type application
a -> b                        % Function (pure)
a -> b / {IO}                 % Function with effect
forall a. a -> a              % Polymorphic type
{x: Int, y: Float | ρ}        % Extensible record
```

### Effect Set Operations

**File:** `src/compiler/types/catena_types.erl` (part of)

```erlang
%% Effect set operations
-spec empty_effects() -> effect_set().
-spec singleton_effect(atom()) -> effect_set().
-spec union_effects(effect_set(), effect_set()) -> effect_set().
-spec is_pure(effect_set()) -> boolean().
-spec normalize_effects([atom()]) -> effect_set().
```

**Properties:**
- Effect sets are always sorted and deduplicated
- Empty effect set `{effect_set, []}` represents pure functions
- Union is set union (maintains sorted property)

### Fresh Variable Generation

**File:** `src/compiler/types/catena_types.erl` (part of)

```erlang
%% Fresh type variable generation using process dictionary or ETS
-spec fresh_var() -> ty().
-spec fresh_var_id() -> type_var_id().
-spec reset_fresh_var_counter() -> ok.
```

**Implementation:**
- Use process dictionary or ETS table for counter
- Thread-safe if using ETS
- Reset counter at start of each module compilation

---

## Subtask Breakdown

### Subtask 1.2.1.1: Core Type Representation ✅

**File:** `src/compiler/types/catena_types.erl`

- [ ] Define `ty()` type with all type constructors
- [ ] Define `effect_set()` and `type_scheme()` types
- [ ] Implement fresh variable generation
- [ ] Implement effect set operations (empty, union, normalize)
- [ ] Implement basic type equality checking
- [ ] **Success:** Can create and manipulate type terms

**Tests:**
```erlang
% Type construction
TVar = {tvar, 1},
TInt = {tcon, integer},
TFun = {tfun, TInt, TInt, {effect_set, []}},

% Effect sets
Empty = empty_effects(),
IO = singleton_effect(io),
IOProcess = union_effects(IO, singleton_effect(process)),

% Fresh variables
V1 = fresh_var(),
V2 = fresh_var(),
true = V1 =/= V2  % Different variables
```

### Subtask 1.2.1.2: Type Substitution Operations ✅

**File:** `src/compiler/types/catena_type_subst.erl`

- [ ] Implement `empty/0`, `singleton/2`
- [ ] Implement `compose/2` for substitution composition
- [ ] Implement `apply/2` for applying substitution to types
- [ ] Implement `apply_scheme/2` for schemes
- [ ] Handle substitution in effect sets
- [ ] **Success:** Can compose and apply substitutions correctly

**Tests:**
```erlang
% Basic substitution
S1 = singleton(1, {tcon, integer}),
{tcon, integer} = apply(S1, {tvar, 1}),

% Composition: S2 ∘ S1
S2 = singleton(2, {tvar, 1}),
S3 = compose(S2, S1),
{tcon, integer} = apply(S3, {tvar, 2}),

% Substitution in function types
Fun = {tfun, {tvar, 1}, {tvar, 2}, empty_effects()},
Sub = singleton(1, {tcon, integer}),
{tfun, {tcon, integer}, {tvar, 2}, _} = apply(Sub, Fun)
```

### Subtask 1.2.1.3: Type Scheme Representation ✅

**File:** `src/compiler/types/catena_type_scheme.erl`

- [ ] Implement `ftv/1` for finding free type variables
- [ ] Implement `generalize/2` for creating schemes
- [ ] Implement `instantiate/1` for instantiating schemes
- [ ] Handle constraints in schemes (empty for now)
- [ ] **Success:** Can generalize and instantiate polymorphic types

**Tests:**
```erlang
% Generalization
EmptyEnv = catena_type_env:empty(),
IdType = {tfun, {tvar, 1}, {tvar, 1}, empty_effects()},
IdScheme = generalize(EmptyEnv, IdType),
#type_scheme{quantified = [1], body = IdType} = IdScheme,

% Instantiation creates fresh variables
{tfun, {tvar, V1}, {tvar, V2}, _} = instantiate(IdScheme),
{tfun, {tvar, V3}, {tvar, V4}, _} = instantiate(IdScheme),
true = (V1 =/= V3),  % Fresh variables each time
true = (V1 =:= V2),  % But same within instantiation
```

### Subtask 1.2.1.4: Pretty-Printing ✅

**File:** `src/compiler/types/catena_type_pp.erl`

- [ ] Implement `pp_type/1` for internal types
- [ ] Implement `pp_scheme/1` for type schemes
- [ ] Implement `pp_effect_set/1` for effects
- [ ] Handle precedence for nested types
- [ ] Use Greek letters for type variables (α, β, γ...)
- [ ] **Success:** Types print in readable form

**Tests:**
```erlang
% Pretty printing
"Int" = type_to_string({tcon, integer}),
"α" = type_to_string({tvar, 1}),
"α -> β" = type_to_string(
    {tfun, {tvar, 1}, {tvar, 2}, empty_effects()}),
"α -> β / {IO}" = type_to_string(
    {tfun, {tvar, 1}, {tvar, 2}, singleton_effect(io)}),
"forall α. α -> α" = type_to_string(
    #type_scheme{quantified = [1],
                 body = {tfun, {tvar, 1}, {tvar, 1}, empty_effects()}})
```

### Subtask 1.2.1.5: Effect Set Integration ✅

**Enhancement to all modules**

- [ ] Extend function types with effect parameters
- [ ] Implement effect set union in substitution
- [ ] Handle effects in generalization/instantiation
- [ ] Test effect polymorphism basics
- [ ] **Success:** Function types track effect sets correctly

**Tests:**
```erlang
% Pure function
Pure = {tfun, {tcon, integer}, {tcon, integer}, empty_effects()},
true = is_pure_function(Pure),

% Effectful function
Effectful = {tfun, {tcon, string}, {tcon, unit}, singleton_effect(io)},
false = is_pure_function(Effectful),

% Effect in type schemes
EffectScheme = generalize(empty_env(), Effectful),
Inst1 = instantiate(EffectScheme),
Inst2 = instantiate(EffectScheme),
% Effects preserved across instantiation
same_effects(Inst1, Inst2) = true
```

---

## Success Criteria

### Functional Requirements

- [ ] Can represent all Catena types (variables, constructors, functions, records, variants)
- [ ] Type variables have unique identifiers and can be generated fresh
- [ ] Substitutions can be created, composed, and applied
- [ ] Type schemes support let-polymorphism (generalize/instantiate)
- [ ] Function types include effect set annotations
- [ ] Effect sets support union, empty, and membership operations

### Non-Functional Requirements

- [ ] All operations are immutable (functional style)
- [ ] Pretty-printing produces readable, standard notation
- [ ] Module structure is clean and well-organized
- [ ] Code is well-documented with type specs
- [ ] Performance is reasonable (no obvious inefficiencies)

### Testing Requirements

- [ ] Unit tests for all type construction operations
- [ ] Unit tests for substitution operations and composition
- [ ] Unit tests for generalization and instantiation
- [ ] Unit tests for effect set operations
- [ ] Unit tests for pretty-printing all type forms
- [ ] Property-based tests for substitution laws
- [ ] **All tests passing** (100% success rate required)

---

## Implementation Plan

### Phase 1: Core Type Module (Day 1)

1. Create `src/compiler/types/` directory
2. Implement `catena_types.erl`:
   - Type definitions
   - Fresh variable generation
   - Effect set operations
   - Basic equality
3. Write unit tests for type construction
4. Verify tests pass

### Phase 2: Substitution (Day 2)

1. Implement `catena_type_subst.erl`:
   - Empty and singleton substitutions
   - Composition
   - Application to types
2. Write comprehensive substitution tests
3. Test composition laws (associativity, identity)
4. Verify all tests pass

### Phase 3: Type Schemes (Day 3)

1. Implement `catena_type_scheme.erl`:
   - Free variable calculation
   - Generalization
   - Instantiation
2. Implement `catena_type_env.erl`:
   - Environment operations
   - FTV for environments
3. Write tests for polymorphism
4. Verify tests pass

### Phase 4: Pretty-Printing (Day 4)

1. Implement `catena_type_pp.erl`:
   - Type pretty-printing
   - Scheme pretty-printing
   - Effect set formatting
2. Test all type forms print correctly
3. Test precedence and parenthesization
4. Verify output matches examples

### Phase 5: Integration & Testing (Day 5)

1. Write integration tests across all modules
2. Add property-based tests (PropEr)
3. Test effect set integration thoroughly
4. Review and refactor
5. **Final verification: All tests pass**

---

## Dependencies

### Prerequisites

- ✅ Parser and AST (Task 1.1.3 complete)
- ✅ Effect syntax support (Task 1.1.5 complete)

### Provides Foundation For

- Task 1.2.2: Algorithm W Implementation (depends on this)
- Task 1.2.3: Type Inference Engine (depends on this)
- Task 1.2.4: Effect Inference (depends on this)

---

## Risk Assessment

### Technical Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|---------|------------|
| Type variable generation conflicts | Low | High | Use atomic counter with proper initialization |
| Substitution composition errors | Medium | High | Extensive unit tests, verify laws |
| Effect set normalization bugs | Low | Medium | Property-based testing |
| Pretty-printing ambiguities | Low | Low | Test precedence carefully |

### Schedule Risks

| Risk | Probability | Impact | Mitigation |
|------|-------------|---------|------------|
| Underestimated complexity | Low | Medium | Well-researched design |
| Testing takes longer | Medium | Low | Start tests early, parallel development |

---

## Testing Approach

### Unit Testing Strategy

Each module gets comprehensive unit tests:

```erlang
% test/compiler/types/catena_types_tests.erl
% test/compiler/types/catena_type_subst_tests.erl
% test/compiler/types/catena_type_scheme_tests.erl
% test/compiler/types/catena_type_env_tests.erl
% test/compiler/types/catena_type_pp_tests.erl
```

### Property-Based Testing (PropEr)

```erlang
% Substitution properties
prop_subst_identity() ->
    ?FORALL(T, type_gen(),
        apply(empty(), T) =:= T).

prop_subst_composition() ->
    ?FORALL({S1, S2, S3}, {subst_gen(), subst_gen(), subst_gen()},
        compose(S3, compose(S2, S1)) =:= compose(compose(S3, S2), S1)).

% Type scheme properties
prop_generalize_instantiate() ->
    ?FORALL(T, type_gen(),
        begin
            Scheme = generalize(empty_env(), T),
            Inst = instantiate(Scheme),
            % Instantiation should be equivalent modulo renaming
            equivalent_types(T, Inst)
        end).
```

### Integration Testing

Test interaction between modules:
- Create type, substitute, generalize, instantiate, pretty-print
- Verify round-trip properties
- Test with complex nested types

---

## Notes and Considerations

### Design Decisions Explained

1. **Integer Type Variable IDs**
   - Simpler than symbol-based names
   - Easier to guarantee freshness
   - Standard in ML/Haskell compilers

2. **Separate Internal Representation**
   - AST preserves source information
   - Internal types optimized for inference
   - Follows OCaml/GHC design

3. **Effect Sets as Lists**
   - Simple and efficient for small sets
   - Could upgrade to sets module if needed
   - Sorted for deterministic comparison

4. **Type Schemes at Top Level**
   - Simpler than HM rank-n types
   - Sufficient for let-polymorphism
   - Can extend later if needed

### Future Enhancements (Post-POC)

- Row polymorphism for extensible records (partial support now)
- Higher-kinded types for functors/monads
- Type classes (traits) with constraints
- Effect polymorphism (effect variables)
- Qualified types
- Subtyping for records/variants

### Performance Considerations

- Fresh variable generation: O(1) with counter
- Substitution application: O(n) in type size
- Substitution composition: O(n+m) in domain size
- Effect set union: O(n+m) with sorting
- Type equality: O(n) in type size

All operations have reasonable complexity for typical program sizes.

### References

- **Algorithm W:** notes/research/1.12-build-system/1.12.1-algorithm-w.md
- **Robinson Unification:** notes/research/1.12-build-system/1.12.2-robinson-unification.md
- **HM Type System:** https://en.wikipedia.org/wiki/Hindley%E2%80%93Milner_type_system
- **Algorithm W Tutorial:** https://jeremymikkola.com/posts/2018_03_25_understanding_algorithm_w.html

---

## Status Tracking

### Current Status: Planning Complete, Ready to Implement

**What's Done:**
- ✅ Research completed (Algorithm W, unification)
- ✅ Design finalized
- ✅ Module structure defined
- ✅ Planning document created
- ✅ Git branch created

**What's Next:**
- Implement Phase 1: Core type module
- Write unit tests
- Continue through phases 2-5

**How to Run:**
```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit --module=catena_types_tests
rebar3 eunit --dir=test/compiler/types/

# Run property tests
rebar3 proper
```

---

**Last Updated:** 2025-11-13
**Status:** Ready for Implementation
