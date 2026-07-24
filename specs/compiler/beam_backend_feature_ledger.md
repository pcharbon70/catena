# BEAM Backend Feature Ledger

## Status

Phase 5 ending baseline, derived from the parser grammar, validated compilation
unit, normalized AST, declaration dispositions, resolved effect metadata, and
executable backend evidence on 2026-07-24.

This ledger is the maintained inventory required by
[ADR-0005](../adr/ADR-0005-fail-closed-semantics-preserving-beam-backend.md).
It records what the backend actually proves; it does not infer executable
support from parser acceptance or from the ability to construct a Core Erlang
term.

## Support Classes

| Class | Meaning |
| --- | --- |
| Proven | Source reaches OTP `from_core`, loads, and exhibits the expected behavior in an executable test. |
| Lowering-only | An explicit backend lowering exists and has focused tests, but source-to-BEAM execution is not yet conformance evidence. |
| Static-erased | A validated construct is intentionally removed before runtime emission. |
| Runtime-lowered | Lowering targets an explicit Catena runtime boundary; complete source-to-BEAM evidence is still pending. |
| Known-failing | A reproducible fixture demonstrates invalid Core, semantic approximation, placeholder output, or silent omission. |
| Deferred | The frontend surface exists, but the accepted backend representation is incomplete and application emission is not supported. |

The source inventory comes from
`src/compiler/parser/catena_parser.yrl`. Normalization evidence comes from
`src/compiler/semantic/catena_desugar.erl` and
`src/compiler/codegen/catena_codegen_lower.erl`. Validated authority and
declaration classification come from `catena_compilation_unit` and
`catena_declaration_disposition`. Local callable classification comes from
`catena_call_resolution`. Effect and handler classification comes from
`catena_effect_resolution`. Backend evidence comes from the `catena_codegen_*`
modules and the tests linked below.

## Module And Declaration Inventory

| Parser-native surface | Normalized/backend disposition | Class | Evidence |
| --- | --- | --- | --- |
| Module header and nested module name | Core module identity | Proven | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Exported transform | Core export with source arity | Proven | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Exported type, trait, or effect | Frontend metadata only | Static-erased | `catena_codegen_erase:erase_decl/1`; executable export semantics are deferred |
| Import: open, qualified, aliased, or selective | Explicit unsupported disposition retaining linkage metadata; no executable linkage | Deferred | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests`; Phase 6 roadmap |
| Type declaration and constructors | Static-erased only after constructor representation and callable metadata is retained | Static-erased | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests`, `catena_backend_hardening_phase3_tests`, constructor rows below |
| Transform with implementation, including guarded and multi-clause forms | Function plus source-ordered Core case clauses | Proven | `catena_codegen_lower_tests`, `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Signature-only transform | Static-erased only when not runtime-exported; an exported signature fails with `missing_transform_implementation` | Deferred | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests` |
| Effect declaration and operations | Static-erased after typed operation identities, arities, uses, handlers, and runtime dependencies are retained | Static-erased | `catena_effect_resolution_tests`, `catena_handler_runtime_dependency_tests`, `catena_backend_hardening_phase5_tests` |
| Trait declaration, extends list, signatures, and default members | Explicit unsupported disposition retaining dispatch metadata | Deferred | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests`; Phase 6 roadmap |
| Instance declaration and methods | Explicit unsupported disposition retaining dictionary metadata | Deferred | `catena_declaration_disposition_tests`; Phase 6 roadmap |
| Test declaration | Explicit unsupported disposition and application-artifact rejection | Deferred | `catena_backend_hardening_phase1_tests`, `catena_backend_hardening_phase2_tests` |
| Property declaration with `forall` bindings | Explicit unsupported disposition and application-artifact rejection | Deferred | `catena_backend_hardening_phase1_tests`, `catena_backend_hardening_phase2_tests` |
| Unknown or unclassified declaration | `invalid_declaration_disposition` before lowering or module success | Deferred | `catena_backend_baseline_tests`, `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests` |

## Expression Inventory

| Parser-native surface | Backend representation | Class | Evidence |
| --- | --- | --- | --- |
| Integer, float, and string literals | Native BEAM terms | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| Lower-case identifier | Lexical Core variable or resolved eta-expanded top-level callable value | Proven for transform parameters, pattern bindings, lambda/let values, and named transforms as values | `catena_codegen_higher_order_tests`, `catena_backend_hardening_phase3_tests` |
| Nullary and applied upper-case constructor | Arity-validated tagged tuple `{Constructor, ...}` | Proven for nullary, unary, and higher-arity constructors | `catena_core_pipeline_tests`, `catena_codegen_higher_order_tests`, `catena_backend_hardening_phase3_tests` |
| Function application | Resolved local function-name `apply`, closure `apply`, or explicit module call | Proven for local, forward, recursive, mutual, and higher-order local calls; imported calls remain Deferred | `catena_codegen_local_call_tests`, `catena_codegen_higher_order_tests`, `catena_backend_hardening_phase3_tests` |
| Field access | `maps:get/2`, retaining native missing-key failure | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `let` with variable binding | Core `let` with lexical callable-value scope | Proven for let-bound functions | `catena_codegen_expr_tests`, `catena_codegen_higher_order_tests`, `catena_backend_hardening_phase3_tests` |
| `let` with a non-variable binding pattern | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_hardening_phase1_tests` |
| Lambda | Core function with lexical parameter scope | Proven for creation and higher-order execution | `catena_codegen_expr_tests`, `catena_codegen_higher_order_tests`, `catena_backend_hardening_phase3_tests` |
| Match function and match with scrutinee | Core case and source-ordered clauses | Proven for the promoted parser-native pattern surface | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Empty/non-empty list | Native list | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| Tuple | Native tuple | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| Empty/non-empty record | BEAM map | Proven | `catena_codegen_pure_expr_tests`, `catena_codegen_data_erasure_tests`, `catena_backend_hardening_phase4_tests` |
| `perform Effect.operation(...)` | Resolved Catena effect-runtime call with the current explicit context, effect identity, operation identity, and arguments | Proven | `catena_effect_resolution_tests`, `catena_effect_context_codegen_tests`, `catena_backend_hardening_phase5_tests` |
| `handle ... then` and handler operation cases | Validated Catena effect-runtime handler boundary with lossless parameter patterns and child context | Proven | `catena_handler_runtime_dependency_tests`, `catena_effect_context_codegen_tests`, `catena_backend_hardening_phase5_tests` |
| `do { ... }` with bind, let, action, and return statements | Desugared to `chain`, lambda, and `let` | Proven when `chain` resolves to an accepted local callable; standard-library trait dispatch remains Deferred | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| Unknown normalized expression | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_hardening_phase1_tests` |

`if_expr`, explicit atom/character/boolean literals, `module_call`, and
`try_with_expr` are backend-normalized forms rather than productions in the
current grammar. They remain lowering-only or runtime-lowered and do not add
parser-native surface entries.

## Pattern Inventory

| Parser-native surface | Backend representation | Class | Evidence |
| --- | --- | --- | --- |
| Variable | Core variable | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Wildcard | Core wildcard variable | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Nullary and applied constructor | Arity-validated tagged tuple pattern | Proven for nullary and higher arities | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Integer, float, and string literal | Core literal pattern | Proven | `catena_codegen_pattern_tests`, `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Empty and fixed list | Native list pattern | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Cons | Native cons pattern | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Tuple | Native tuple pattern | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Empty and populated record | Core exact-key map pattern | Proven | `catena_codegen_data_erasure_tests`, `catena_backend_hardening_phase4_tests` |
| As-pattern | Core alias pattern retaining alias and inner bindings | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Or-pattern in nested and multi-position clauses | Recursively expanded into source-ordered alternative Core clauses with identical binding sets | Proven | `catena_codegen_lossless_pattern_tests`, `catena_backend_hardening_phase4_tests` |
| Misplaced normalized or-pattern | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_baseline_tests` |
| Unknown normalized pattern | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_hardening_phase1_tests` |

## Operator Inventory

| Parser token/surface | Normalized behavior | Class | Evidence |
| --- | --- | --- | --- |
| `+`, `-`, `*`, `/` | Explicit Erlang arithmetic call | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `==`, `!=`, `<`, `>`, `<=`, `>=` | Explicit Erlang comparison call | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `&&`, `||` | Explicit Erlang Boolean call | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `++` | Explicit Erlang list append call | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `::` | Core cons | Proven | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `|>` | Resolved Core function application | Proven for accepted local and closure targets | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `===`, `!==` | Desugared to `equals`/`not equals` trait calls | Deferred | `catena_desugar`; Phase 6 roadmap |
| `<$>`, `<*>`, `>>=` | Desugared to `map`, `apply`, and `chain` calls | Deferred | `catena_desugar`; Phase 6 roadmap |
| `>>>`, `<<<`, `***`, `&&&` | Desugared to flow method calls | Deferred | `catena_desugar`; Phase 6 roadmap |
| `<>` | Desugared to `combine` | Proven when `combine` resolves to an accepted local callable; trait dispatch remains Deferred | `catena_codegen_pure_expr_tests`, `catena_backend_hardening_phase4_tests` |
| `>=>` | Desugared to `kleisli` | Deferred | `catena_desugar`; Phase 6 roadmap |
| Unknown normalized binary or unary operator | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_baseline_tests`, `catena_backend_hardening_phase1_tests` |

## Maintenance Rule

Any grammar or canonical AST change must update this ledger in the same
section-level commit. A row moves to Proven only with source-to-BEAM execution
evidence. Deferred lowering must reject artifact generation unless a later
phase promotes it with an accepted representation. Known-failing behavior
that is not a lossy fallback remains visible until its scheduled phase fixes
it.
