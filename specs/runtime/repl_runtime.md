# REPL Runtime

## Status

Promoted status: implemented as a compiler-backed interactive environment with prelude loading, command handling, multiline input support, and REPL workflow tests.

## Design Anchors

- [Proof-of-concept Phase 2 plan](../../notes/planning/proof-of-concept/phase-02.md)
- [Phase 2.1 REPL summary](../../notes/summaries/phase-2.1-repl-summary.md)
- [Phase 2.2 prelude summary](../../notes/summaries/phase-2.2-prelude-summary.md)
- [Phase 2.4 integration tests summary](../../notes/summaries/phase-2.4-integration-tests.md)
- `src/repl/catena_repl.erl`
- `src/stdlib/catena_prelude.erl`
- `test/repl/catena_repl_tests.erl`
- `test/integration/catena_repl_workflow_tests.erl`
- `test/integration/catena_repl_programs_tests.erl`

## Current Promoted Surface

- The REPL is a real implemented subsystem, not just a roadmap item.
- Interactive work is compiler-backed: expressions and loaded files go through Catena's existing pipeline rather than a separate ad hoc parser.
- The REPL boots with the Erlang-side prelude bindings unless explicitly told not to.
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

## Out Of Scope

- debugger-like introspection
- package management
- the final production tooling experience for interactive development
