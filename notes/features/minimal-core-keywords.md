# Feature: Minimal Core Keywords

**Branch**: `feature/minimal-core-keywords`
**Date**: 2025-11-21
**Status**: In Progress

---

## Problem Statement

Catena currently has 26 keywords, many of which can be expressed using other language constructs or moved to a standard library. This creates:

1. **Higher learning curve** - More syntax to learn
2. **More compiler complexity** - Each keyword needs special handling
3. **Less flexibility** - Hardcoded features can't evolve without language changes
4. **Larger formal semantics** - Harder to reason about

### Impact Analysis

- Lexer: Remove ~14 keyword definitions
- Parser: Simplify grammar rules
- Standard library: New modules for testing, documentation, etc.
- Documentation: Update language specification

---

## Solution Overview

Reduce to **12 core keywords** that require compiler support, and move the rest to standard library conventions or annotations.

### Core Keywords (Compiler Support Required)

| Keyword | Purpose | Why Required |
|---------|---------|--------------|
| `type` | ADT definitions | Memory layout, constructors |
| `transform` | Function definitions | Code generation |
| `let` | Variable binding | Scope management |
| `match` | Pattern matching | Exhaustiveness, compilation |
| `trait` | Type class mechanism | Instance resolution |
| `instance` | Implementations | Dictionary passing |
| `effect` | Effect declarations | Effect type tracking |
| `perform` | Effect invocation | Handler lookup |
| `handle` | Effect handling | Handler installation |
| `actor` | Actor definitions | BEAM process creation |
| `process` | Message handlers | State machine compilation |
| `module` | Module definitions | Replaces `system` |

### Removed Keywords (Moved to Library/Annotations)

| Keyword | Replacement |
|---------|-------------|
| `system` | Renamed to `module` |
| `extends` | Syntax `: ParentTrait` in trait definition |
| `adapter` | Transform with type `f ~> g` |
| `laws` | Property tests in trait or testing library |
| `test` | `Test.unit` library function |
| `property` | `Test.property` library function |
| `doc` | `@doc` attribute |
| `do` | Desugar from `>>=` or library macro |
| `operator` | `@operator` attribute |
| `if`/`then`/`else` | Desugar to `match` |
| `in` | Layout rules |
| `where` | Use braces `{ }` |
| `with` | Part of `handle` or record update syntax |

---

## Technical Details

### Files to Modify

1. **`src/compiler/lexer/catena_lexer.xrl`**
   - Remove keywords: `system`, `extends`, `adapter`, `laws`, `test`, `property`, `doc`, `do`, `if`, `then`, `else`, `in`, `where`
   - Add keyword: `module` (if not present), `handle`
   - Keep keywords: `type`, `transform`, `let`, `match`, `trait`, `instance`, `effect`, `perform`, `actor`, `process`

2. **`src/compiler/parser/catena_parser.yrl`**
   - Update grammar for simplified trait syntax
   - Remove adapter-specific rules
   - Remove test/property rules
   - Update effect handling syntax

3. **New: `lib/catena/stdlib/`**
   - `prelude.catena` - Core traits (Comparable, Mapper, Pipeline, etc.)
   - `test.catena` - Testing framework
   - `doc.catena` - Documentation utilities

4. **`notes/research/language_overview.md`**
   - Update keyword table
   - Update examples

### New Standard Library Structure

```
lib/catena/stdlib/
├── prelude.catena      # Core traits and types
├── test.catena         # Testing framework
├── effect/
│   ├── io.catena       # IO effect
│   ├── state.catena    # State effect
│   └── error.catena    # Error effect
└── data/
    ├── maybe.catena    # Maybe type
    ├── either.catena   # Either type
    └── list.catena     # List type
```

---

## Success Criteria

1. Lexer accepts only 12 core keywords
2. Parser handles simplified syntax
3. Standard library defines removed concepts
4. All existing tests pass (adapted for new syntax)
5. Language specification updated
6. Examples work with new syntax

---

## Implementation Plan

### Step 1: Update Lexer ⬜
- [ ] Remove non-core keywords from lexer
- [ ] Add `module` and `handle` keywords
- [ ] Update token definitions
- [ ] Test lexer changes

### Step 2: Update Parser ⬜
- [ ] Simplify trait syntax (`:` for inheritance)
- [ ] Remove adapter rules
- [ ] Remove test/property rules
- [ ] Update effect handling to use `handle`
- [ ] Add attribute support for `@doc`, `@operator`

### Step 3: Create Standard Library Structure ⬜
- [ ] Create stdlib directory structure
- [ ] Write prelude.catena with core traits
- [ ] Write test.catena with testing framework
- [ ] Write effect modules

### Step 4: Update Language Specification ⬜
- [ ] Update notes/research/language_overview.md
- [ ] Update keyword table
- [ ] Update examples

### Step 5: Update Tests ⬜
- [ ] Adapt lexer tests for new keywords
- [ ] Adapt parser tests for new syntax
- [ ] Add stdlib tests

### Step 6: Documentation ⬜
- [ ] Write summary document
- [ ] Request commit permission

---

## Current Status

### What's Implemented
- Feature branch created
- Planning document created

### What's Next
- Update lexer to remove non-core keywords

### How to Run
```bash
make compile
make test
```

---

## Notes/Considerations

### Backward Compatibility
This is a breaking change. Code using old keywords will need to be updated:
- `system` → `module`
- `adapter` → `transform` with `~>` type
- `test` → `Test.unit`
- `property` → `Test.property`
- `doc` → `@doc` attribute

### Migration Path
1. Old syntax continues to work in current main branch
2. New syntax available in this feature branch
3. Future versions will deprecate old syntax

### Risks
- Lexer/parser changes may break existing tests
- Standard library syntax depends on parser features not yet implemented

### Future Improvements
- Add macro system for `do` notation
- Add derive mechanism for automatic instances
- Add module system with imports
