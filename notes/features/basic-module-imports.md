# Basic Module Import Resolution

**Date**: 2025-11-28
**Branch**: `feature/basic-module-imports`
**Status**: In Progress

---

## Problem Statement

Integration tests for Phase 1.5 (Standard Library Validation) are blocked because they require importing functions from the standard library. The current compiler has no mechanism to:

1. Parse import statements in Catena source
2. Load and parse imported module files
3. Merge imported symbol types into the local type environment
4. Resolve cross-module references during type checking

### Blocked Integration Tests

- `map (fn x -> x + 1) [1, 2, 3]` - needs `map` from prelude
- `Some 5 >>= (fn x -> Some (x + 1))` - needs `>>=` desugaring and Maybe
- `[1, 2] <> [3, 4]` - needs `<>` desugaring and Combiner
- do-notation with Maybe - needs `chain` from prelude

---

## Solution Overview

Implement a **minimal** module import system that:
- Adds parser grammar for simple `import ModuleName` statements
- Creates a module loader to read and parse `.cat` files
- Builds a combined type environment from imported modules
- Integrates with existing type checking pipeline

### Explicit Non-Goals (to keep scope minimal)

- Circular import detection (error on any cycle)
- Qualified imports (`Module.function`)
- Selective imports (`import Module (func1, func2)`)
- Re-exports from imported modules

---

## Current State Analysis

### Parser Infrastructure
- Lexer recognizes `import` keyword (line 78 of `catena_lexer.xrl`)
- Import record defined in `catena_ast.hrl`:
  ```erlang
  -record(import, {
      module :: atom(),
      items :: [atom()] | all,
      qualified :: boolean(),
      alias :: atom() | undefined,
      location :: location()
  }).
  ```
- Module AST is 6-tuple: `{module, Name, Exports, Imports, Decls, Loc}`
- **Gap**: Parser has NO grammar rules for import statements

### Compilation Pipeline (`catena_compile.erl`)
- `compile_file/1` reads file and calls `compile_string/1`
- `compile_string/1` does: Lex -> Parse -> Semantic -> Type check
- `build_type_env/1` builds type environment from declarations
- **Gap**: No import processing exists

### Type Environment (`catena_type_env.erl`)
- Simple map from variable names to type schemes
- Operations: `empty/0`, `extend/3`, `lookup/2`, `merge/2`
- Ready for cross-module extension via `merge/2`

---

## Implementation Plan

### Step 1: Add Import Grammar Rules (Parser)
- [ ] Add `import_decl` nonterminal to `catena_parser.yrl`
- [ ] Grammar: `import upper_ident` -> `{import, ModuleName, Loc}`
- [ ] Add import handling in module structure
- [ ] Test: Parse `import Prelude` and verify AST

### Step 2: Create Module Loader Module
- [ ] Create `src/compiler/catena_module_loader.erl`
- [ ] Implement `load_module/2` - (ModuleName, SearchPaths) -> {ok, AST}
- [ ] Implement `find_module/2` - locate .cat file
- [ ] Implement `get_stdlib_path/0` - return stdlib directory
- [ ] Convert module names to paths: `Prelude` -> `prelude.cat`, `Effect.IO` -> `effect/io.cat`
- [ ] Test: Load prelude module successfully

### Step 3: Build Type Environment from Module AST
- [ ] Add `build_module_exports/1` to extract exported types
- [ ] Reuse existing `build_type_env/1` filtered to exports
- [ ] Test: Build env from prelude, verify `map`, `Maybe` present

### Step 4: Process Imports in Compilation Pipeline
- [ ] Modify `compile_string/1` to process imports before type checking
- [ ] Add `process_imports/1` function
- [ ] Merge imported env with local env
- [ ] Test: Compile file with `import Prelude` that uses `map`

### Step 5: Integration Tests
- [ ] Create `catena_import_integration_tests.erl`
- [ ] Test import and use of prelude function
- [ ] Test error on non-existent module
- [ ] Unblock Phase 1.5 integration tests

---

## Success Criteria

1. **Parser**: `import Prelude` parses to `{import, 'Prelude', Loc}`
2. **Loading**: `catena_module_loader:load_module('Prelude', Paths)` returns valid AST
3. **Environment**: Imported module types are available in local type checking
4. **Integration**: The following compiles successfully:
   ```catena
   import Prelude

   transform double_all xs = map (fn x -> x + x) xs
   ```

---

## Progress Log

### 2025-11-28
- Created feature branch `feature/basic-module-imports`
- Created planning document

**Step 1: Parser Grammar** ✅
- Added `import_decl` nonterminal to parser
- Import declarations parsed as regular declarations then extracted
- Supports simple imports (`import Prelude`) and dotted imports (`import Effect.IO`)
- Added `extract_imports/1` and `filter_imports/1` helper functions

**Step 2: Module Loader** ✅
- Created `src/compiler/catena_module_loader.erl`
- Implements `load_module/2`, `find_module/2`, `get_stdlib_path/0`
- Converts module names to paths: `Prelude` → `prelude.cat`, `Effect.IO` → `effect/io.cat`
- Searches stdlib path first, then current directory

**Step 3: Type Environment from Exports** ✅
- Added `build_module_exports_env/1` to `catena_compile.erl`
- Filters declarations to only exported types/transforms
- Uses existing `build_type_env/1` on filtered declarations

**Step 4: Compilation Pipeline** ✅
- Added `process_imports/1` function
- Updated `compile_string/1` to process imports before type checking
- Added `compile_string/2` with options for import control
- Imported environment merged with local environment

**Step 5: Integration Tests** ✅
- Created `test/compiler/integration/catena_import_tests.erl`
- 21 tests covering module loader, parser, import processing, and compilation
- All tests passing

**Step 6: Stdlib Updates** ✅
- Added type exports to `prelude.cat`: `Ordering`, `Maybe`, `Either`, `Result`
- Constructors (`Some`, `None`, `Left`, `Right`, `Ok`, `Err`, `LT`, `EQ`, `GT`) now importable
