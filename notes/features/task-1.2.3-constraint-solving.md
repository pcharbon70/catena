# Feature Planning Document: Task 1.2.3 - Constraint Solving

**Task ID:** 1.2.3
**Phase:** 1 (Core Language Infrastructure)
**Section:** 1.2 (Core Type System)
**Status:** 📋 PLANNED
**Created:** 2025-11-17
**Branch:** `feature/task-1.2.3-constraint-solving` (to be created)

---

## Progress Tracking

### Overall Status
- [ ] **Task 1.2.3 Complete**
- [ ] 1.2.3.1 Trait constraint representation and generation
- [ ] 1.2.3.2 Instance resolution and unification
- [ ] 1.2.3.3 Constraint simplification
- [ ] 1.2.3.4 Coherence checking
- [ ] 1.2.3.5 Effect handler verification and exhaustiveness

### Current Phase
**Phase:** Planning → Not Started

**Dependencies:**
✅ **Task 1.2.1** - Type Representation (COMPLETE)
✅ **Task 1.2.2** - Algorithm W Implementation (COMPLETE)

**What's Next:**
1. Create constraint representation module
2. Implement trait constraint generation
3. Implement instance resolution algorithm
4. Add constraint simplification rules
5. Implement coherence checking
6. Add effect handler verification

**How to Run (once implemented):**
```bash
# Run constraint solving tests
rebar3 eunit --module=catena_constraint_tests
rebar3 eunit --module=catena_instance_tests
rebar3 eunit --module=catena_handler_verify_tests

# Run all type system tests including constraints
rebar3 eunit --dir=test/compiler/types
```

---

## Table of Contents

1. [Executive Summary](#executive-summary)
2. [Problem Statement and Goals](#problem-statement-and-goals)
3. [Technical Approach](#technical-approach)
4. [Module Design](#module-design)
5. [Dependencies](#dependencies)
6. [Implementation Steps](#implementation-steps)
7. [Testing Strategy](#testing-strategy)
8. [Success Criteria](#success-criteria)
9. [References](#references)

---

## Executive Summary

Task 1.2.3 extends the Algorithm W type inference engine with **constraint solving** for traits (type classes) and **effect handler verification**. This enables Catena's category-theory-first approach through principled polymorphism and ensures correctness of algebraic effect handlers.

**Key Deliverables:**
- Trait constraint representation and generation during inference
- Instance resolution algorithm searching available trait instances
- Constraint simplification reducing complex constraints to canonical form
- Coherence checking preventing ambiguous instance resolution
- Effect handler verification ensuring completeness and type correctness

**Implementation Complexity:** HIGH
- Estimated effort: 7-9 days
- Dependencies: Tasks 1.2.1, 1.2.2 (COMPLETE)
- Target test coverage: ≥90%

**Architectural Position:**
This task completes the type inference foundation by adding constraint solving on top of Algorithm W. It enables trait-based polymorphism (Functor, Monad, etc.) and verifies algebraic effect handlers, bridging the gap between pure type inference and Catena's category theory abstractions.

---

## Problem Statement and Goals

### What We're Building

An advanced constraint solving system that extends Algorithm W with:

1. **Trait Constraints** - Represent requirements like "T must implement Functor"
2. **Instance Resolution** - Search for trait implementations and unify with constraints
3. **Constraint Simplification** - Reduce complex constraints to canonical minimal form
4. **Coherence Checking** - Ensure unique instance resolution without ambiguity
5. **Effect Handler Verification** - Verify handlers match effect declarations completely

### Why Constraint Solving?

**Theoretical Foundation:**
- Type classes (traits) provide **ad-hoc polymorphism** with principled semantics
- Instance resolution ensures **type-driven dispatch** at compile time
- Coherence guarantees **deterministic behavior** (no ambiguous instances)
- Handler verification prevents **runtime effect errors**

**Practical Benefits:**
- Express category theory abstractions (Functor, Monad, Monoid) naturally
- Enable generic programming with trait-bounded polymorphism
- Catch missing trait implementations at compile time
- Verify effect handlers are complete before runtime

**Catena-Specific Extensions:**
- **Category theory traits** form the standard library foundation
- **Effect verification** ensures algebraic effect safety
- **Dual notation** (keywords + operators) requires trait-based dispatch

### Problem Complexity

**Core Challenges:**

1. **Constraint Generation** - Track which traits are required during inference
2. **Instance Search** - Find matching trait implementations efficiently
3. **Overlapping Instances** - Detect and reject ambiguous instances
4. **Constraint Entailment** - Determine if constraints are satisfied
5. **Handler Exhaustiveness** - Verify all effect operations are handled

**Simplifications for PoC:**
- **No higher-order constraints** - Only trait constraints, no constraint families
- **No functional dependencies** - Simple instance matching without FDs
- **Monomorphic effects** - Effect handler verification without effect polymorphism
- **Simple coherence** - Basic overlap checking without sophisticated algorithms

### Goals

**Primary Goals:**
1. ✅ Represent trait constraints alongside types during inference
2. ✅ Generate constraints from trait-polymorphic function uses
3. ✅ Resolve instances by searching available implementations
4. ✅ Simplify constraints to canonical form
5. ✅ Check for overlapping instances (coherence)
6. ✅ Verify effect handlers match effect declarations

**Non-Goals (Future Tasks):**
- ❌ Advanced constraint solving (type families, GADTs) - Phase 6
- ❌ Functional dependencies - Phase 6
- ❌ Effect polymorphism constraints - Phase 6
- ❌ Constraint-based type error messages - Task 1.2.4

---

## Technical Approach

### Constraint System Overview

Constraints are predicates on types that must be satisfied for a program to type-check:

```
Constraint C ::= TraitName Type                    % T implements Trait
               | Effect EffectName                 % Effect E is available
               | HandlerComplete EffectName [Op]  % Handler covers all operations

Context Γ ::= [Constraint]                        % Set of constraints
```

**Key Invariant:** `Γ ⊢ e : τ` means "under constraints Γ, expression e has type τ"

### Core Operations

#### 1. Constraint Representation

```erlang
%% Constraint types
-type constraint() :: {trait, TraitName :: atom(), Type :: catena_types:ty()}
                    | {effect_avail, EffectName :: atom()}
                    | {handler_complete, EffectName :: atom(), Operations :: [atom()]}.

%% Constraint set (normalized: sorted, no duplicates)
-type constraint_set() :: {constraint_set, [constraint()]}.

%% Type with constraints: ∀α. C => τ
-record(qualified_type, {
    quantified :: [catena_types:type_var_id()],
    constraints :: constraint_set(),
    body :: catena_types:ty()
}).
```

**Example:**
```catena
-- map : ∀a b. Functor f => (a -> b) -> f a -> f b
-- Represented as:
{qualified_type,
  [1, 2, 3],                           % ∀a b f
  {constraint_set, [{trait, 'Functor', {tvar, 3}}]},
  {tfun,
    {tfun, {tvar, 1}, {tvar, 2}, {effect_set, []}},
    {tfun,
      {tapp, {tvar, 3}, [{tvar, 1}]},
      {tapp, {tvar, 3}, [{tvar, 2}]},
      {effect_set, []}
    },
    {effect_set, []}
  }
}
```

#### 2. Constraint Generation

During type inference, generate constraints when:

```erlang
%% Variable lookup with trait constraints
infer_var(Env, Var, State0) ->
    case catena_type_env:lookup(Env, Var) of
        {ok, {qualified_type, QuantVars, Constraints, Type}} ->
            % Instantiate quantified variables
            {InstType, Subst, State1} = instantiate(QuantVars, Type, State0),
            % Apply substitution to constraints
            InstConstraints = apply_subst_constraints(Subst, Constraints),
            % Add constraints to context
            State2 = add_constraints(InstConstraints, State1),
            {InstType, empty_effects(), State2};
        {ok, MonoType} ->
            {MonoType, empty_effects(), State0};
        none ->
            Error = catena_type_error:unbound_variable(Var),
            {error, Error, add_error(Error, State0)}
    end.

%% Trait method usage generates constraints
%% Example: x === y generates constraint Eq(α) where x : α
infer_app(Env, {app, {var, '==='}, [E1, E2]}, State0) ->
    {T1, Eff1, State1} = infer(Env, E1, State0),
    {T2, Eff2, State2} = infer(Env, E2, State1),
    {S, State3} = unify(T1, T2, State2),
    % Add Eq constraint
    Constraint = {trait, 'Eq', apply_subst(S, T1)},
    State4 = add_constraint(Constraint, State3),
    Effects = union_effects(Eff1, Eff2),
    {tcon(boolean), Effects, State4}.
```

#### 3. Instance Resolution

**Goal:** Given constraint `Trait(T)`, find instance declaration matching T

```erlang
%% Instance declaration representation
-record(trait_instance, {
    trait :: atom(),                    % Trait name
    type :: catena_types:ty(),          % Type pattern
    context :: constraint_set(),        % Required constraints
    methods :: #{atom() => expr()}      % Method implementations
}).

%% Instance resolution algorithm
-spec resolve_constraint(constraint(), [trait_instance()], infer_state()) ->
    {ok, trait_instance(), infer_state()} |
    {error, catena_type_error:type_error(), infer_state()}.

resolve_constraint({trait, TraitName, Type}, Instances, State0) ->
    % Filter instances matching trait name
    Candidates = [I || I = #trait_instance{trait = T} <- Instances, T =:= TraitName],

    % Try to unify Type with each instance pattern
    case find_matching_instance(Type, Candidates, State0) of
        {ok, Instance, Subst, State1} ->
            % Check context constraints are satisfied
            InstContext = apply_subst_constraints(Subst, Instance#trait_instance.context),
            State2 = add_constraints(InstContext, State1),
            {ok, Instance, State2};
        {error, no_instance} ->
            Error = catena_type_error:no_instance(TraitName, Type),
            {error, Error, add_error(Error, State0)};
        {error, ambiguous, Instances} ->
            Error = catena_type_error:ambiguous_instance(TraitName, Type, Instances),
            {error, Error, add_error(Error, State0)}
    end.

%% Find instance that unifies with type
find_matching_instance(Type, Instances, State) ->
    Matches = lists:filtermap(
        fun(Instance) ->
            case try_unify_instance(Type, Instance, State) of
                {ok, Subst, State1} -> {true, {Instance, Subst, State1}};
                {error, _} -> false
            end
        end,
        Instances
    ),
    case Matches of
        [] -> {error, no_instance};
        [{Instance, Subst, State1}] -> {ok, Instance, Subst, State1};
        [_,_|_] -> {error, ambiguous, [I || {I, _, _} <- Matches]}
    end.
```

**Example:**
```catena
-- Instance declaration
instance Functor Maybe where
  fmap f = match
    | None -> None
    | Some x -> Some (f x)
  end

-- Resolving constraint Functor(Maybe)
-- Unifies successfully with instance pattern
```

#### 4. Constraint Simplification

**Goal:** Reduce constraint sets to canonical minimal form

```erlang
%% Simplification rules:
%% 1. Remove duplicates
%% 2. Apply substitution to all constraints
%% 3. Resolve trivial constraints (instances without context)
%% 4. Check for unsatisfiable constraints

-spec simplify_constraints(constraint_set(), catena_type_subst:subst(),
                           [trait_instance()], infer_state()) ->
    {constraint_set(), infer_state()}.

simplify_constraints({constraint_set, Cs}, Subst, Instances, State0) ->
    % Apply substitution
    Cs1 = [apply_subst_constraint(Subst, C) || C <- Cs],

    % Remove duplicates
    Cs2 = lists:usort(Cs1),

    % Try to resolve constraints with context-free instances
    {Cs3, State1} = resolve_trivial_constraints(Cs2, Instances, State0),

    % Check for conflicts
    case check_constraint_conflicts(Cs3) of
        ok -> {{constraint_set, Cs3}, State1};
        {error, Error} ->
            State2 = add_error(Error, State1),
            {{constraint_set, Cs3}, State2}
    end.

%% Resolve constraints that have no-context instances
resolve_trivial_constraints(Constraints, Instances, State) ->
    lists:foldl(
        fun(C = {trait, _, _}, {AccCs, AccState}) ->
            case resolve_constraint(C, Instances, AccState) of
                {ok, #trait_instance{context = {constraint_set, []}}, State1} ->
                    % Instance has no context, constraint satisfied
                    {AccCs, State1};
                {ok, #trait_instance{context = Context}, State1} ->
                    % Instance has context, add context constraints
                    {constraint_set, ContextCs} = Context,
                    {AccCs ++ ContextCs, State1};
                {error, _, State1} ->
                    % Keep constraint unresolved
                    {[C | AccCs], State1}
            end;
           (C, {AccCs, AccState}) ->
            % Non-trait constraint, keep as-is
            {[C | AccCs], AccState}
        end,
        {[], State},
        Constraints
    ).
```

#### 5. Coherence Checking

**Goal:** Ensure at most one instance matches any type

```erlang
%% Check for overlapping instances
-spec check_coherence([trait_instance()]) ->
    ok | {error, {overlapping_instances, atom(), trait_instance(), trait_instance()}}.

check_coherence(Instances) ->
    % Group by trait name
    ByTrait = group_by_trait(Instances),

    % Check each trait's instances for overlap
    maps:fold(
        fun(TraitName, TraitInstances, ok) ->
            check_trait_coherence(TraitName, TraitInstances);
           (_, _, Error) ->
            Error
        end,
        ok,
        ByTrait
    ).

%% Check pairs of instances for overlap
check_trait_coherence(TraitName, Instances) ->
    % Check all pairs
    Pairs = [{I1, I2} || I1 <- Instances, I2 <- Instances, I1 < I2],
    lists:foldl(
        fun({I1, I2}, ok) ->
            case instances_overlap(I1, I2) of
                true ->
                    {error, {overlapping_instances, TraitName, I1, I2}};
                false ->
                    ok
            end;
           (_, Error) ->
            Error
        end,
        ok,
        Pairs
    ).

%% Two instances overlap if their type patterns unify
instances_overlap(I1, I2) ->
    State = catena_infer_state:new(),
    T1 = I1#trait_instance.type,
    T2 = I2#trait_instance.type,
    case catena_infer_unify:unify(T1, T2, State) of
        {ok, _, _} -> true;   % Unification succeeded, instances overlap
        {error, _, _} -> false  % No unification, instances don't overlap
    end.
```

**Example:**
```catena
-- REJECTED: Overlapping instances
instance Eq [a] where ...      -- List of any type
instance Eq [Int] where ...    -- List of integers (overlaps!)

-- ACCEPTED: Non-overlapping instances
instance Eq Int where ...
instance Eq String where ...
instance Eq a => Eq [a] where ...  -- Generic list instance
```

#### 6. Effect Handler Verification

**Goal:** Verify try/with handlers match effect declarations

```erlang
%% Effect declaration (from parser)
-record(effect_decl, {
    name :: atom(),
    operations :: [{OpName :: atom(), ArgTypes :: [catena_types:ty()],
                    RetType :: catena_types:ty()}]
}).

%% Handler verification
-spec verify_handler(atom(), [{atom(), expr()}], [effect_decl()],
                     catena_type_env:env(), infer_state()) ->
    {ok, infer_state()} | {error, catena_type_error:type_error(), infer_state()}.

verify_handler(EffectName, HandlerCases, EffectDecls, Env, State0) ->
    % Look up effect declaration
    case lists:keyfind(EffectName, #effect_decl.name, EffectDecls) of
        false ->
            Error = catena_type_error:unknown_effect(EffectName),
            {error, Error, add_error(Error, State0)};
        EffectDecl ->
            % Check exhaustiveness: all operations have handlers
            DeclaredOps = [Op || {Op, _, _} <- EffectDecl#effect_decl.operations],
            HandledOps = [Op || {Op, _} <- HandlerCases],
            MissingOps = DeclaredOps -- HandledOps,
            ExtraOps = HandledOps -- DeclaredOps,

            case {MissingOps, ExtraOps} of
                {[], []} ->
                    % Check each handler case type
                    verify_handler_cases(HandlerCases, EffectDecl, Env, State0);
                {[_|_], _} ->
                    Error = catena_type_error:incomplete_handler(EffectName, MissingOps),
                    {error, Error, add_error(Error, State0)};
                {_, [_|_]} ->
                    Error = catena_type_error:unknown_operations(EffectName, ExtraOps),
                    {error, Error, add_error(Error, State0)}
            end
    end.

%% Verify each handler case matches operation signature
verify_handler_cases(Cases, EffectDecl, Env, State) ->
    lists:foldl(
        fun({OpName, HandlerExpr}, {ok, AccState}) ->
            % Look up operation signature
            {OpName, ArgTypes, RetType} =
                lists:keyfind(OpName, 1, EffectDecl#effect_decl.operations),

            % Handler should have type: (Args, Resume) -> Result
            % where Resume : RetType -> ResumptionType
            {ResumeVar, AccState1} = fresh_var(AccState),
            {ResultVar, AccState2} = fresh_var(AccState1),

            HandlerType = build_handler_type(ArgTypes, RetType, ResumeVar, ResultVar),

            % Infer handler expression type
            case infer(Env, HandlerExpr, AccState2) of
                {InferredType, _, AccState3} ->
                    % Unify with expected handler type
                    case unify(HandlerType, InferredType, AccState3) of
                        {ok, _, AccState4} ->
                            {ok, AccState4};
                        {error, Error, AccState4} ->
                            {error, Error, AccState4}
                    end;
                {error, Error, AccState3} ->
                    {error, Error, AccState3}
            end;
           (_, Error) ->
            Error
        end,
        {ok, State},
        Cases
    ).
```

**Example:**
```catena
-- Effect declaration
effect FileIO {
  operation read(path: String): String
  operation write(path: String, content: String): Unit
}

-- Handler verification
try
  perform FileIO.read("/etc/passwd")
with FileIO {
  read(path) -> resume -> ...      -- ✓ Correct signature
  write(path, content) -> resume -> ...  -- ✓ Correct signature
}
-- ✓ Exhaustive: all operations handled

-- Missing write handler would fail verification
```

### State Management

**Extended Inference State:**

```erlang
-record(infer_state, {
    next_var :: pos_integer(),
    subst :: catena_type_subst:subst(),
    errors :: [catena_type_error:type_error()],
    constraints :: constraint_set(),     % NEW: accumulated constraints
    instances :: [trait_instance()],     % NEW: available trait instances
    effect_decls :: [effect_decl()]      % NEW: effect declarations
}).
```

---

## Module Design

### Module Structure

```
src/compiler/types/
├── catena_constraint.erl          # Constraint representation and operations
├── catena_instance.erl            # Instance resolution and coherence
├── catena_handler_verify.erl      # Effect handler verification
└── catena_infer_state.erl         # Extended with constraint tracking (modified)
```

### Module 1: `catena_constraint.erl`

**Purpose:** Constraint representation, generation, and simplification

**Exports:**
```erlang
-export([
    % Constraint construction
    trait_constraint/2,
    effect_constraint/1,
    handler_constraint/2,

    % Constraint set operations
    empty_constraint_set/0,
    add_constraint/2,
    union_constraints/2,
    apply_subst_constraint/2,
    apply_subst_constraints/2,

    % Constraint simplification
    simplify/3,
    normalize/1,

    % Constraint checking
    is_trivial/1,
    has_conflicts/1,

    % Pretty-printing
    format_constraint/1,
    format_constraint_set/1
]).

-export_type([
    constraint/0,
    constraint_set/0,
    qualified_type/0
]).
```

**Key Types:**
```erlang
-type constraint() :: {trait, atom(), catena_types:ty()}
                    | {effect_avail, atom()}
                    | {handler_complete, atom(), [atom()]}.

-type constraint_set() :: {constraint_set, [constraint()]}.

-record(qualified_type, {
    quantified :: [catena_types:type_var_id()],
    constraints :: constraint_set(),
    body :: catena_types:ty()
}).

-type qualified_type() :: #qualified_type{}.
```

### Module 2: `catena_instance.erl`

**Purpose:** Trait instance representation and resolution

**Exports:**
```erlang
-export([
    % Instance management
    new_instance/4,
    add_instance/2,
    get_instances/2,

    % Instance resolution
    resolve_constraint/3,
    find_matching_instance/3,

    % Coherence checking
    check_coherence/1,
    instances_overlap/2,

    % Built-in instances
    builtin_instances/0,

    % Pretty-printing
    format_instance/1
]).

-export_type([
    trait_instance/0,
    instance_env/0
]).
```

**Key Types:**
```erlang
-record(trait_instance, {
    trait :: atom(),
    type :: catena_types:ty(),
    context :: catena_constraint:constraint_set(),
    methods :: #{atom() => expr()},
    location :: location() | builtin
}).

-type trait_instance() :: #trait_instance{}.

-type instance_env() :: #{atom() => [trait_instance()]}.
```

### Module 3: `catena_handler_verify.erl`

**Purpose:** Effect handler verification and exhaustiveness checking

**Exports:**
```erlang
-export([
    % Handler verification
    verify_handler/5,
    verify_handler_case/5,
    check_exhaustiveness/2,

    % Effect declaration lookup
    lookup_effect/2,
    lookup_operation/3,

    % Handler type construction
    build_handler_type/4,

    % Error construction
    incomplete_handler_error/2,
    unknown_operation_error/2,
    handler_type_mismatch/3
]).

-export_type([
    effect_decl/0,
    operation_decl/0
]).
```

**Key Types:**
```erlang
-record(effect_decl, {
    name :: atom(),
    operations :: [operation_decl()],
    location :: location()
}).

-type operation_decl() :: {OpName :: atom(),
                           ArgTypes :: [catena_types:ty()],
                           RetType :: catena_types:ty()}.

-type effect_decl() :: #effect_decl{}.
```

### Module 4: `catena_infer_state.erl` (Extended)

**Purpose:** Extended inference state with constraints and instances

**New Exports:**
```erlang
-export([
    % Constraint operations
    add_constraint/2,
    add_constraints/2,
    get_constraints/1,
    clear_constraints/1,

    % Instance operations
    add_instance/2,
    get_instances/1,
    set_instances/2,

    % Effect declaration operations
    add_effect_decl/2,
    get_effect_decls/1
]).
```

**Extended State:**
```erlang
-record(infer_state, {
    next_var :: pos_integer(),
    subst :: catena_type_subst:subst(),
    errors :: [catena_type_error:type_error()],
    constraints :: catena_constraint:constraint_set(),
    instances :: catena_instance:instance_env(),
    effect_decls :: [catena_handler_verify:effect_decl()]
}).
```

---

## Dependencies

### Task 1.2.1 Modules (COMPLETE)

All foundational modules from Task 1.2.1 are available:

1. **`catena_types.erl`** ✅ - Type representation and operations
2. **`catena_type_subst.erl`** ✅ - Substitution operations
3. **`catena_type_scheme.erl`** ✅ - Type schemes (will extend to qualified types)
4. **`catena_type_env.erl`** ✅ - Type environments
5. **`catena_type_error.erl`** ✅ - Error constructors (will extend)

### Task 1.2.2 Modules (COMPLETE)

All Algorithm W modules are available:

1. **`catena_infer.erl`** ✅ - Main inference orchestration
2. **`catena_infer_expr.erl`** ✅ - Expression inference
3. **`catena_infer_pattern.erl`** ✅ - Pattern inference
4. **`catena_infer_unify.erl`** ✅ - Unification algorithm
5. **`catena_infer_state.erl`** ✅ - State management (will extend)

### Parser Dependencies (Task 1.1.x)

From the parser, we need:

1. **Trait declarations** - TraitDecl AST nodes
2. **Instance declarations** - InstanceDecl AST nodes
3. **Effect declarations** - EffectDecl AST nodes
4. **Handler expressions** - TryWithExpr and HandlerCase AST nodes

These are already available from Tasks 1.1.5 and 1.1.6.

---

## Implementation Steps

### Subtask 1.2.3.1: Trait Constraint Representation and Generation

**Goal:** Implement constraint types and generate them during inference

**Steps:**

1. Create `catena_constraint.erl` module
   - Define constraint types (trait, effect, handler)
   - Implement constraint set operations
   - Add constraint pretty-printing

2. Extend `catena_type_scheme.erl` with qualified types
   - Add `qualified_type` record
   - Update generalization to include constraints
   - Update instantiation to apply to constraints

3. Modify `catena_infer_expr.erl` to generate constraints
   - Add constraints when looking up trait-polymorphic variables
   - Generate trait constraints for operator uses (`===`, `<>`, etc.)
   - Thread constraints through inference state

4. Extend `catena_infer_state.erl` with constraint tracking
   - Add `constraints` field to state
   - Implement constraint accumulation functions
   - Add constraint retrieval functions

**Test Cases:**
- Constraint set normalization (deduplication, sorting)
- Constraint substitution application
- Constraint generation for trait method uses
- Qualified type representation and pretty-printing

**Success Criteria:**
- Can represent constraints for all Catena traits
- Constraints are generated during inference
- Constraints are properly substituted during unification
- Pretty-printing shows clear constraint syntax

---

### Subtask 1.2.3.2: Instance Resolution and Unification

**Goal:** Search for trait instances and unify with constraints

**Steps:**

1. Create `catena_instance.erl` module
   - Define `trait_instance` record
   - Implement instance environment (map from trait to instances)
   - Add instance lookup and storage functions

2. Implement instance resolution algorithm
   - Filter instances by trait name
   - Try unification with instance type patterns
   - Handle instance context constraints recursively
   - Return resolved instance or error

3. Add built-in instances
   - Define instances for primitive types (Int, String, Bool)
   - Add Eq, Ord instances for built-ins
   - Add Functor instances for List, Maybe

4. Integrate with inference state
   - Store available instances in state
   - Call instance resolution during constraint solving
   - Update constraint set with resolved context constraints

**Test Cases:**
- Instance unification with concrete types
- Instance unification with polymorphic types
- Instance context resolution (transitive constraints)
- Built-in instance lookup
- No matching instance error
- Ambiguous instance detection

**Success Criteria:**
- Can resolve trait constraints by finding instances
- Instance context constraints are added to constraint set
- Built-in instances work for primitive types
- Clear errors for missing instances

---

### Subtask 1.2.3.3: Constraint Simplification

**Goal:** Reduce constraint sets to canonical minimal form

**Steps:**

1. Implement constraint normalization
   - Remove duplicate constraints
   - Sort constraints by trait name then type
   - Canonical form for deterministic testing

2. Implement trivial constraint resolution
   - Identify constraints with context-free instances
   - Resolve immediately during simplification
   - Remove from constraint set when satisfied

3. Implement constraint conflict detection
   - Check for contradictory constraints
   - Detect unsatisfiable constraint combinations
   - Report conflicts as type errors

4. Add substitution-aware simplification
   - Apply current substitution to all constraints
   - Re-simplify after unification steps
   - Maintain simplified invariant throughout inference

**Test Cases:**
- Duplicate constraint removal
- Sorting and normalization
- Trivial constraint resolution
- Constraint conflicts detection
- Simplification after substitution
- Idempotence (simplify twice = simplify once)

**Success Criteria:**
- Constraint sets are always in canonical form
- Trivial constraints are resolved automatically
- Conflicts are detected early
- Performance is acceptable for typical programs

---

### Subtask 1.2.3.4: Coherence Checking

**Goal:** Ensure at most one instance matches any type (no ambiguity)

**Steps:**

1. Implement instance overlap detection
   - Try to unify pairs of instance patterns
   - If unification succeeds, instances overlap
   - Report all overlapping instance pairs

2. Add coherence checking function
   - Group instances by trait name
   - Check each trait's instances pairwise
   - Collect all overlaps into error report

3. Integrate coherence checking into module loading
   - Check coherence when adding instances
   - Reject modules with overlapping instances
   - Report clear errors with instance locations

4. Handle instance ordering and specificity
   - For PoC: reject all overlaps (conservative)
   - Document limitations for future work
   - Note: full specificity ordering deferred to Phase 6

**Test Cases:**
- Non-overlapping instances accepted
- Overlapping instances rejected
- Multiple overlaps reported clearly
- Built-in instances don't overlap
- Edge cases (polymorphic patterns, type constructors)

**Success Criteria:**
- Detects all instance overlaps
- Rejects programs with overlapping instances
- Clear error messages with instance locations
- Built-in instances pass coherence check

---

### Subtask 1.2.3.5: Effect Handler Verification

**Goal:** Verify handlers match effect declarations and are exhaustive

**Steps:**

1. Create `catena_handler_verify.erl` module
   - Define `effect_decl` and `operation_decl` types
   - Implement effect declaration lookup
   - Add operation signature lookup

2. Implement exhaustiveness checking
   - Compare handler operations with declared operations
   - Detect missing operations (incomplete handler)
   - Detect extra operations (unknown operations)
   - Report clear errors for both cases

3. Implement handler case type checking
   - Build expected handler type from operation signature
   - Handler type: (Args..., Resume) -> Result
   - Infer handler expression type
   - Unify with expected type

4. Integrate with `catena_infer_expr.erl`
   - Verify handlers during try/with inference
   - Check exhaustiveness before type inference
   - Type check each handler case
   - Remove handled effects from effect set

**Test Cases:**
- Exhaustive handler accepted
- Incomplete handler rejected (missing operations)
- Unknown operation rejected
- Handler type mismatch detected
- Multiple handlers for same effect
- Nested handlers (effect handling within handlers)

**Success Criteria:**
- Detects incomplete handlers at compile time
- Verifies handler types match operation signatures
- Clear error messages for handler problems
- Effect removal works correctly after handler verification

---

## Testing Strategy

### Test Coverage Goals

Target: ≥90% coverage for all modules

**Coverage by Module:**
- `catena_constraint.erl`: 90%+ (constraint operations)
- `catena_instance.erl`: 95%+ (instance resolution critical)
- `catena_handler_verify.erl`: 90%+ (verification logic)
- Extended `catena_infer_state.erl`: 85%+

### Unit Tests

**Constraint Module Tests:**
```erlang
% catena_constraint_tests.erl
constraint_construction_test()
constraint_set_operations_test()
constraint_normalization_test()
constraint_substitution_test()
constraint_simplification_test()
qualified_type_test()
```

**Instance Module Tests:**
```erlang
% catena_instance_tests.erl
instance_construction_test()
instance_resolution_simple_test()
instance_resolution_polymorphic_test()
instance_context_test()
instance_overlap_detection_test()
coherence_checking_test()
builtin_instances_test()
```

**Handler Verification Tests:**
```erlang
% catena_handler_verify_tests.erl
exhaustiveness_checking_test()
handler_type_checking_test()
missing_operation_error_test()
unknown_operation_error_test()
handler_type_mismatch_test()
nested_handlers_test()
```

### Integration Tests

**End-to-End Constraint Solving:**
```catena
-- Test polymorphic function with trait constraint
flow map : forall a b. Functor f => (a -> b) -> f a -> f b
flow map f xs = fmap f xs

-- Test constraint resolution
let xs = [1, 2, 3]
let ys = map (fn x => x + 1) xs  -- Resolves Functor(List)

-- Test missing instance error
shape MyType = MyConstructor Int
let bad = map (fn x => x) (MyConstructor 42)  -- ERROR: No Functor(MyType)
```

**Effect Handler Verification:**
```catena
-- Test exhaustive handler
effect FileIO {
  operation read(path: String): String
  operation write(path: String, content: String): Unit
}

flow process_file : String -> String / {FileIO}
flow process_file path =
  let content = perform FileIO.read(path)
  content

-- Exhaustive handler (OK)
let result = try
  process_file "/tmp/test.txt"
with FileIO {
  read(path) -> resume -> resume("file contents")
  write(path, content) -> resume -> resume(unit)
}

-- Incomplete handler (ERROR)
let bad = try
  process_file "/tmp/test.txt"
with FileIO {
  read(path) -> resume -> resume("file contents")
  -- Missing write handler!
}
```

---

## Success Criteria

### Functional Requirements

1. **Constraint Representation**
   - ✅ Can represent trait, effect, and handler constraints
   - ✅ Constraint sets are normalized (sorted, deduplicated)
   - ✅ Qualified types with constraints pretty-print correctly

2. **Instance Resolution**
   - ✅ Finds matching instances for trait constraints
   - ✅ Handles instance context constraints recursively
   - ✅ Reports clear errors for missing instances
   - ✅ Built-in instances work for primitive types

3. **Constraint Simplification**
   - ✅ Removes duplicate constraints
   - ✅ Resolves trivial constraints automatically
   - ✅ Detects and reports constraint conflicts
   - ✅ Applies substitutions correctly

4. **Coherence Checking**
   - ✅ Detects overlapping instances
   - ✅ Rejects programs with ambiguous instances
   - ✅ Reports all overlaps with locations

5. **Handler Verification**
   - ✅ Checks handler exhaustiveness (all operations covered)
   - ✅ Verifies handler types match operation signatures
   - ✅ Reports missing and unknown operations
   - ✅ Integrates with effect tracking system

### Performance Requirements

- Instance resolution: O(n) where n = number of instances for a trait
- Coherence checking: O(n²) where n = number of instances per trait
- Constraint simplification: O(m log m) where m = constraint set size
- Acceptable performance for programs with <100 instances per trait

### Error Quality

All errors must include:
- Clear description of the problem
- Source location (file, line, column)
- Suggested fixes where applicable
- Related instance/effect declaration locations

### Test Coverage

- ≥90% line coverage for all new modules
- ≥95% coverage for critical paths (instance resolution, coherence)
- All edge cases tested (polymorphic instances, nested constraints)
- Integration tests for complete workflows

---

## References

### Academic Papers

1. **Wadler, P., & Blott, S. (1989).** "How to make ad-hoc polymorphism less ad hoc." POPL '89.
   - Original type classes paper (Haskell's trait system)

2. **Jones, M. P. (1992).** "A Theory of Qualified Types." Oxford University thesis.
   - Formal foundation for constraint-based type systems

3. **Peyton Jones, S., et al. (1997).** "Type classes: an exploration of the design space." Haskell Workshop.
   - Design space exploration for coherence and overlap

4. **Leijen, D. (2014).** "Koka: Programming with Row Polymorphic Effect Types." MSR-TR-2014-79.
   - Effect handler verification in algebraic effect systems

5. **Lindley, S., McBride, C., & McLaughlin, C. (2017).** "Do Be Do Be Do." POPL '17.
   - Formal semantics of algebraic effects and handlers

### Haskell GHC Implementation

- GHC Constraint Solver: Constraint generation and simplification
- Instance Resolution: Coherence checking and overlap detection
- Error Messages: Constraint-based error reporting

### Prior Catena Tasks

1. **Task 1.2.1 - Type Representation** ✅ COMPLETE
   - Provides type representation and operations

2. **Task 1.2.2 - Algorithm W** ✅ COMPLETE
   - Provides type inference infrastructure
   - State management and unification

3. **Task 1.1.5 - Effect Syntax** ✅ COMPLETE
   - Effect declaration AST nodes

4. **Task 1.1.6 - Trait Syntax** ✅ COMPLETE
   - Trait and instance declaration AST nodes

### Future Enhancements (Phase 6)

- Effect polymorphism with effect variables
- Type families and associated types
- Functional dependencies
- Instance specialization and overlapping instances
- Constraint-based type error messages

---

**Last Updated:** 2025-11-17
**Next Review:** After completing Subtask 1.2.3.1
