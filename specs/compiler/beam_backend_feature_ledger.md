# BEAM Backend Feature Ledger

## Status

Phase 1 baseline, derived from the parser grammar and the normalized AST on
2026-07-24.

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
`src/compiler/codegen/catena_codegen_lower.erl`. Backend evidence comes from
the `catena_codegen_*` modules and the tests linked below.

## Module And Declaration Inventory

| Parser-native surface | Normalized/backend disposition | Class | Evidence |
| --- | --- | --- | --- |
| Module header and nested module name | Core module identity | Proven | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Exported transform | Core export with source arity | Proven | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Exported type, trait, or effect | Frontend metadata only | Static-erased | `catena_codegen_erase:erase_decl/1`; executable export semantics are deferred |
| Import: open, qualified, aliased, or selective | Frontend environment metadata; no executable linkage | Deferred | `catena_compile`; Phase 6 roadmap |
| Type declaration and constructors | Declaration erased after constructor metadata is consumed | Static-erased | `catena_codegen_erase_tests`, constructor rows below |
| Transform with implementation, including guarded and multi-clause forms | Function plus case clauses | Proven for simple and constructor clauses; otherwise Lowering-only | `catena_codegen_lower_tests`, `catena_core_pipeline_tests` |
| Signature-only transform | Removed by lowering without executable requirement analysis | Deferred | `catena_codegen_lower:lower_decl/1`; Phase 2 roadmap |
| Effect declaration and operations | Declaration metadata erased | Static-erased | `catena_codegen_erase:erase_decl/1` |
| Trait declaration, extends list, signatures, and default members | Declaration erased; dispatch metadata is incomplete | Deferred | `catena_codegen_erase:erase_decl/1`; Phase 6 roadmap |
| Instance declaration and methods | Provisional dictionary transform | Lowering-only | `catena_codegen_erase_tests`; dispatch is not source-to-BEAM proven |
| Test declaration | No application-artifact disposition; can be silently filtered | Known-failing | `catena_backend_baseline_tests`; Phase 2 roadmap |
| Property declaration with `forall` bindings | No application-artifact disposition; can be silently filtered | Known-failing | `catena_backend_baseline_tests`; Phase 2 roadmap |
| Unknown or unclassified declaration | Silently filtered from the generated module | Known-failing | `catena_backend_baseline_tests` |

## Expression Inventory

| Parser-native surface | Backend representation | Class | Evidence |
| --- | --- | --- | --- |
| Integer, float, and string literals | Native BEAM terms | Proven for integer; Lowering-only for float/string | `catena_core_pipeline_tests`, `catena_codegen_expr_tests` |
| Lower-case identifier | Core variable or callable value | Lowering-only; top-level named calls are Known-failing | `catena_codegen_expr_tests`, `catena_backend_baseline_tests` |
| Nullary and applied upper-case constructor | Tagged tuple `{Constructor, ...}` | Proven for nullary/unary; Lowering-only for larger arities | `catena_core_pipeline_tests`, `catena_codegen_expr_tests` |
| Function application | Core `apply` or explicit module call | Lowering-only; named local call is Known-failing | `catena_codegen_expr_tests`, `catena_backend_baseline_tests` |
| Field access | `maps:get/2` | Lowering-only | `catena_codegen_expr_tests` |
| `let` with variable binding | Core `let` | Lowering-only | `catena_codegen_expr_tests` |
| `let` with a non-variable binding pattern | Binding replaced by wildcard | Known-failing | `catena_backend_baseline_tests` |
| Lambda | Core function | Lowering-only | `catena_codegen_expr_tests` |
| Match function and match with scrutinee | Core case and clauses | Proven for constructor clauses; otherwise Lowering-only | `catena_core_pipeline_tests`, `catena_codegen_pattern_tests` |
| Empty/non-empty list | Native list | Lowering-only | `catena_codegen_expr_tests` |
| Tuple | Native tuple | Lowering-only | `catena_codegen_expr_tests` |
| Empty/non-empty record | BEAM map | Lowering-only | `catena_codegen_expr_tests` |
| `perform Effect.operation(...)` | Catena effect-runtime call | Runtime-lowered | `catena_effect_codegen_tests` |
| `handle ... then` and handler operation cases | Catena effect-runtime handler boundary | Runtime-lowered | `catena_effect_codegen_tests` |
| `do { ... }` with bind, let, action, and return statements | Desugared to `chain`, lambda, and `let` | Deferred | `catena_desugar`; named trait-method resolution is incomplete |
| Unknown normalized expression | Attempts to generate `{error, unknown_expression, Term}` through a nonexistent Core constructor and crashes | Known-failing | `catena_backend_baseline_tests` |

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
| Or-pattern | Expanded when it is the single clause pattern; misplaced forms become wildcard | Known-failing | `catena_codegen_pattern_tests`, `catena_backend_baseline_tests` |
| Unknown normalized pattern | Replaced by wildcard | Known-failing | `catena_backend_baseline_tests` |

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
| Unknown normalized binary or unary operator | Arbitrary `erlang:Operator` call | Known-failing | `catena_backend_baseline_tests` |

## Maintenance Rule

Any grammar or canonical AST change must update this ledger in the same
section-level commit. A row moves to Proven only with source-to-BEAM execution
evidence. A Known-failing or Deferred row must reject artifact generation
after Phase 1 unless a later phase promotes it with an accepted
representation.
