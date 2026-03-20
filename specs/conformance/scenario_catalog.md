# Scenario Catalog

This catalog defines the stable `SCN-*` scenarios used by the backfilled Catena specs.

| Scenario ID | Name | Description |
| --- | --- | --- |
| `SCN-001` | Parse Valid Source | Valid Catena modules and declarations tokenize and parse into structured ASTs. |
| `SCN-002` | Type And Effect Inference | Type checking, trait resolution, kind checking, and effect tracking produce typed output or explicit diagnostics. |
| `SCN-003` | Lower To Core Erlang | Valid Catena code lowers through code generation to Core Erlang and BEAM-compilable artifacts. |
| `SCN-004` | Run Effectful Runtime Path | Effectful operations execute through the explicit runtime boundary with nested handler semantics and builtin fallbacks where intended. |
| `SCN-005` | Interactive REPL Workflow | The REPL accepts expressions and commands, preserves environment, and surfaces types/introspection behavior. |
| `SCN-006` | Pattern Analysis And Compilation | Advanced patterns, decision trees, and exhaustiveness/redundancy checks behave coherently across parser, typing, and code generation surfaces. |
| `SCN-007` | Validate Standard Library Surfaces | Prelude, test, law, and effect modules parse, type-check, and validate against compiler capabilities. |
| `SCN-008` | Exercise Internal Property Testing | The internal property-testing framework supports the implemented rose-tree, generator/seed, and categorical-generator layers while clearly marking later primitive-combinator and runner work as pending. |
| `SCN-009` | Build And Test The Repo | Build hooks, subsystem tests, and legacy migration boundaries are documented and reviewable. |
| `SCN-010` | Verify Trait Laws | Law definitions, concrete `Test.verify` suites, and the future generator-backed law-testing path remain coherent across stdlib and property-testing surfaces. |
