# REPL Runtime

## Status

Promoted status: implemented as a compiler-backed interactive environment with prelude loading, command handling, multiline input support, and REPL workflow tests.

## Design Anchors

- [Current Status](../planning/current_status.md)
- [Runtime Contract](../contracts/runtime_contract.md)
- `src/repl/catena_repl.erl`
- `src/repl/catena_repl_effects.erl`
- `src/stdlib/catena_prelude.erl`
- `test/repl/catena_repl_tests.erl`
- `test/repl/catena_repl_effects_tests.erl`
- `test/integration/catena_repl_workflow_tests.erl`
- `test/integration/catena_repl_programs_tests.erl`

## Current Promoted Surface

- The REPL is a real implemented subsystem, not just a roadmap item.
- Interactive work is compiler-backed: expressions and loaded files go through Catena's existing pipeline rather than a separate ad hoc parser.
- The REPL boots with the Erlang-side prelude bindings unless explicitly told not to.
- The direct effect evaluator automatically exposes IO, Process, Error, and
  State handlers. The standard Process operations `spawn`, `send`, and `self`
  delegate to the reconciled BEAM process façade.
- Phase 2 planning checkboxes are stale relative to the summaries and code, so this spec follows the reconciled implementation state.

## Acceptance Criteria

### AC-REPL-001 Interactive State Model

The REPL must maintain interactive state that includes:

- the current type environment
- user/runtime bindings
- command history
- current prompt state
- continuation text for incomplete multiline input

### AC-REPL-002 Command Surface

The promoted REPL command surface includes the implemented commands in `catena_repl`, specifically:

- `:help`
- `:type`
- `:load`
- `:browse`
- `:env`
- `:clear`
- `:prelude`
- `:quit`

Additional commands may be added later, but these form the current canonical baseline.

### AC-REPL-003 Prelude-Backed Startup

Unless disabled for tests or specialized boot flows, REPL startup must initialize with the runtime prelude bindings so interactive use has immediate access to the standard functional core.

### AC-REPL-004 Compiler-Backed Evaluation

The REPL must continue to rely on the compiler and surrounding type/prelude surfaces for:

- expression parsing/typing
- file loading
- environment merging
- type introspection

This keeps the REPL aligned with the language instead of becoming a separate evaluator with drifting semantics.

### AC-REPL-005 Honest Status

The promoted current status for the REPL/runtime track is:

- REPL: implemented
- prelude/runtime bindings: implemented
- testing framework support: implemented
- integration-test coverage: implemented

This criterion exists to reconcile the spec with the actual code and summaries rather than the stale Phase 2 checklist.

### AC-REPL-006 Direct Effect Boundary

The REPL's automatic handlers are a direct evaluator surface. They must:

- recognize both canonical effect names such as `Process` and their internal
  lowercase aliases
- execute standard Process `spawn`, `send`, and `self` operations
- return evaluator values in the common `{ok, Value}` shape
- convert a missing registered process target into a structured effect error

This direct path does not replace the explicit-context runtime used by
generated code.

## Out Of Scope

- debugger-like introspection
- package management
- the final production tooling experience for interactive development
- automatic support for Process operations beyond the standard effect's
  current `spawn`, `send`, and `self` declarations
