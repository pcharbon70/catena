# Scenario Catalog

This catalog defines the stable `SCN-*` scenarios used by the promoted Catena
specs. Their machine-readable EUnit evidence lives in
[`executable_scenarios.tsv`](executable_scenarios.tsv).

| Scenario ID | Name | Description |
| --- | --- | --- |
| `SCN-001` | Parse Valid Source | Valid Catena modules and declarations tokenize and parse into structured ASTs. |
| `SCN-002` | Type And Effect Inference | Type checking, trait resolution, kind checking, and effect tracking produce typed output or explicit diagnostics. |
| `SCN-003` | Lower To Core Erlang | Valid Catena code lowers through code generation to Core Erlang and BEAM-compilable artifacts. |
| `SCN-004` | Run Effectful Runtime Path | Effectful operations execute through the explicit runtime boundary with nested handler semantics and builtin fallbacks where intended. |
| `SCN-005` | Interactive REPL Workflow | The REPL accepts expressions and commands, preserves environment, and surfaces types/introspection behavior. |
| `SCN-006` | Pattern Analysis And Compilation | Advanced patterns, decision trees, and exhaustiveness/redundancy checks behave coherently across parser, typing, and code generation surfaces. |
| `SCN-007` | Validate Standard Library Surfaces | Prelude, test, law, and effect modules parse, type-check, and validate against compiler capabilities. |
| `SCN-008` | Exercise Internal Property Testing | Catena-owned generators, shrinking, properties, runners, reporting, adapters, and advanced helper boundaries execute coherently. |
| `SCN-009` | Build And Test The Repo | Generated-source hooks, module checks, focused workflows, governance checks, and the complete active suite remain reviewable through canonical commands. |
| `SCN-010` | Verify Trait Laws | Structural and concrete Catena laws plus known-instance generic law checks execute through the maintained stdlib/property-testing bridge. |

## Executable Evidence

Each manifest row contains a scenario ID, EUnit module, and repository-relative
source path. A scenario may map to more than one module when its promoted
boundary crosses subsystems.

The focused conformance command executes the unique module set from the
manifest. It complements rather than replaces the complete active-suite gate:

- focused conformance answers whether every stable scenario still has
  executable representative coverage
- the complete suite remains the authority for repository-wide quality
