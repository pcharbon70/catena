# BEAM Backend Feature Ledger

## Status

Phase 2 ending baseline, derived from the parser grammar, validated compilation
unit, normalized AST, declaration dispositions, and executable backend evidence
on 2026-07-24.

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
`catena_declaration_disposition`. Backend evidence comes from the
`catena_codegen_*` modules and the tests linked below.

## Module And Declaration Inventory

| Parser-native surface | Normalized/backend disposition | Class | Evidence |
| --- | --- | --- | --- |
| Module header and nested module name | Core module identity | Proven | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Exported transform | Core export with source arity | Proven | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Exported type, trait, or effect | Frontend metadata only | Static-erased | `catena_codegen_erase:erase_decl/1`; executable export semantics are deferred |
| Import: open, qualified, aliased, or selective | Explicit unsupported disposition retaining linkage metadata; no executable linkage | Deferred | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests`; Phase 6 roadmap |
| Type declaration and constructors | Static-erased only after constructor representation metadata is retained | Static-erased | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests`, constructor rows below |
| Transform with implementation, including guarded and multi-clause forms | Function plus case clauses | Proven for simple and constructor clauses; otherwise Lowering-only | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Signature-only transform | Static-erased only when not runtime-exported; an exported signature fails with `missing_transform_implementation` | Deferred | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests` |
| Effect declaration and operations | Static-erased after operation metadata is retained | Static-erased | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests` |
| Trait declaration, extends list, signatures, and default members | Explicit unsupported disposition retaining dispatch metadata | Deferred | `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests`; Phase 6 roadmap |
| Instance declaration and methods | Explicit unsupported disposition retaining dictionary metadata | Deferred | `catena_declaration_disposition_tests`; Phase 6 roadmap |
| Test declaration | Explicit unsupported disposition and application-artifact rejection | Deferred | `catena_backend_hardening_phase1_tests`, `catena_backend_hardening_phase2_tests` |
| Property declaration with `forall` bindings | Explicit unsupported disposition and application-artifact rejection | Deferred | `catena_backend_hardening_phase1_tests`, `catena_backend_hardening_phase2_tests` |
| Unknown or unclassified declaration | `invalid_declaration_disposition` before lowering or module success | Deferred | `catena_backend_baseline_tests`, `catena_declaration_disposition_tests`, `catena_backend_hardening_phase2_tests` |

## Expression Inventory

| Parser-native surface | Backend representation | Class | Evidence |
| --- | --- | --- | --- |
| Integer, float, and string literals | Native BEAM terms | Proven for integer; Lowering-only for float/string | `catena_core_pipeline_tests`, `catena_codegen_expr_tests` |
| Lower-case identifier | Core variable or callable value | Lowering-only; top-level named calls are Known-failing | `catena_codegen_expr_tests`, `catena_backend_baseline_tests` |
| Nullary and applied upper-case constructor | Tagged tuple `{Constructor, ...}` | Proven for nullary/unary; Lowering-only for larger arities | `catena_core_pipeline_tests`, `catena_codegen_expr_tests` |
| Function application | Core `apply` or explicit module call | Lowering-only; named local call is Known-failing | `catena_codegen_expr_tests`, `catena_backend_baseline_tests` |
| Field access | `maps:get/2` | Lowering-only | `catena_codegen_expr_tests` |
| `let` with variable binding | Core `let` | Lowering-only | `catena_codegen_expr_tests` |
| `let` with a non-variable binding pattern | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_hardening_phase1_tests` |
| Lambda | Core function | Lowering-only | `catena_codegen_expr_tests` |
| Match function and match with scrutinee | Core case and clauses | Proven for constructor clauses; otherwise Lowering-only | `catena_core_pipeline_tests`, `catena_codegen_pattern_tests` |
| Empty/non-empty list | Native list | Lowering-only | `catena_codegen_expr_tests` |
| Tuple | Native tuple | Lowering-only | `catena_codegen_expr_tests` |
| Empty/non-empty record | BEAM map | Lowering-only | `catena_codegen_expr_tests` |
| `perform Effect.operation(...)` | Catena effect-runtime call | Runtime-lowered | `catena_effect_codegen_tests` |
| `handle ... then` and handler operation cases | Catena effect-runtime handler boundary | Runtime-lowered | `catena_effect_codegen_tests` |
| `do { ... }` with bind, let, action, and return statements | Desugared to `chain`, lambda, and `let` | Deferred | `catena_desugar`; named trait-method resolution is incomplete |
| Unknown normalized expression | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_hardening_phase1_tests` |

`if_expr`, explicit atom/character/boolean literals, `module_call`, and
`try_with_expr` are backend-normalized forms rather than productions in the
current grammar. They remain lowering-only or runtime-lowered and do not add
parser-native surface entries.

## Pattern Inventory

| Parser-native surface | Backend representation | Class | Evidence |
| --- | --- | --- | --- |
| Variable | Core variable | Proven inside transform clauses | `catena_core_pipeline_tests`, `catena_codegen_pattern_tests` |
| Wildcard | Core wildcard variable | Lowering-only | `catena_codegen_pattern_tests` |
| Nullary and applied constructor | Tagged tuple pattern | Proven for nullary/unary | `catena_core_pipeline_tests` |
| Integer, float, and string literal | Core literal pattern | Lowering-only | `catena_codegen_pattern_tests` |
| Empty and fixed list | Native list pattern | Lowering-only | `catena_codegen_pattern_tests` |
| Cons | Native cons pattern | Lowering-only | `catena_codegen_pattern_tests` |
| Tuple | Native tuple pattern | Lowering-only | `catena_codegen_pattern_tests` |
| Empty and populated record | Core map pattern | Lowering-only | `catena_codegen_pattern_tests` |
| As-pattern | Core alias pattern | Lowering-only | `catena_codegen_pattern_tests` |
| Or-pattern in the single clause-pattern position | Expanded into alternative Core clauses | Lowering-only | `catena_codegen_pattern_tests` |
| Misplaced normalized or-pattern | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_baseline_tests` |
| Unknown normalized pattern | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_hardening_phase1_tests` |

## Operator Inventory

| Parser token/surface | Normalized behavior | Class | Evidence |
| --- | --- | --- | --- |
| `+`, `-`, `*`, `/` | Explicit Erlang arithmetic call | Proven for `+`; Lowering-only for the remainder | `catena_core_pipeline_tests`, `catena_codegen_expr_tests` |
| `==`, `!=`, `<`, `>`, `<=`, `>=` | Explicit Erlang comparison call | Lowering-only | `catena_codegen_expr_tests` |
| `and`, `or` | Explicit Erlang boolean call | Lowering-only | `catena_codegen_expr_tests` |
| `++` | Explicit Erlang list append call | Lowering-only | `catena_codegen_expr_tests` |
| `::` | Core cons | Lowering-only | `catena_codegen_expr_tests` |
| `|>` | Core function application | Lowering-only; named target resolution is incomplete | `catena_codegen_expr_tests`, Phase 3 roadmap |
| `===`, `!==` | Desugared to `equals`/`not equals` trait calls | Deferred | `catena_desugar`; Phase 6 roadmap |
| `<$>`, `<*>`, `>>=` | Desugared to `map`, `apply`, and `chain` calls | Deferred | `catena_desugar`; Phase 6 roadmap |
| `>>>`, `<<<`, `***`, `&&&` | Desugared to flow method calls | Deferred | `catena_desugar`; Phase 6 roadmap |
| `<>` | Desugared to `combine` | Deferred | `catena_desugar`; Phase 6 roadmap |
| `>=>` | Desugared to `kleisli` | Deferred | `catena_desugar`; Phase 6 roadmap |
| Unknown normalized binary or unary operator | `unsupported_backend_construct` before Core emission | Deferred | `catena_backend_baseline_tests`, `catena_backend_hardening_phase1_tests` |

## Maintenance Rule

Any grammar or canonical AST change must update this ledger in the same
section-level commit. A row moves to Proven only with source-to-BEAM execution
evidence. Deferred lowering must reject artifact generation unless a later
phase promotes it with an accepted representation. Known-failing behavior
that is not a lossy fallback remains visible until its scheduled phase fixes
it.
