# Delimited Resumption Feature Ledger

## Status

Phase 1 implementation baseline for
[ADR-0006](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md).

This ledger prevents internal helper names, marker callbacks, request/response
runtime behavior, and the reference oracle from being mistaken for promoted
Catena source-language delimited control. Later phases must update a row only
when its implementation and required evidence are both present.

## Baseline Snapshot

The pre-implementation source baseline is `origin/main` commit
`088dbb1d277719e9211658574ae8693707dd87b3`, dated 2026-07-24.

| Measurement | Baseline | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,029 pass; 0 failures; 0 skips | Latest full gate recorded before this Phase 1 branch |
| Parser conflicts | 37 shift/reduce; 0 reduce/reduce | Known Yecc baseline; not introduced by resumption work |
| Repository coverage | 27% | Latest complete Phase 7 coverage gate |
| Dialyzer | 822 warnings | Existing repository-wide non-green baseline |
| Resumption source syntax | absent | No `with` binder or `resume` expression token/production |
| Resumption AST/type/lowering | absent | No normalized resumption node, first-class type, or selective-CPS mode |
| Production continuation capture | absent | Current helpers return direct-style values or marker results |

The EUnit, coverage, and Dialyzer values are historical gate measurements
recorded in [Current Status](../planning/current_status.md), not fresh claims
made by adding the oracle. Section 1.4 records the Phase 1 completion test
count. Parser conflicts are checked on every ordinary compilation.

## Classification Vocabulary

| Classification | Meaning |
| --- | --- |
| Request/response | Computes an operation result and returns it to an ordinary direct-style caller; does not capture a language continuation |
| Internal helper | Implements useful handler, kind, depth, type, or state behavior but is not itself a source-level delimited-control implementation |
| Marker-backed | Wraps a function or value that demonstrates an API shape without executing the actual remainder after `perform` |
| Semantic oracle | Independently executes the normative model for comparison and tests; never linked by generated application code |
| Planned source implementation | Accepted by the ADR and architecture but absent from the compiler/runtime path |
| Deferred mode | Has an accepted conceptual meaning but lacks the required syntax, typing, runtime authority, or evidence for promotion |

## Source-To-Runtime Surface Ledger

| Surface | Current implementation | Classification | Promotion target |
| --- | --- | --- | --- |
| `perform` syntax and AST | Parsed and typed as the existing effect expression | Request/response | Preserve syntax; classify resumable suspension points |
| `handle` and operation cases | Parsed value cases with direct-style bodies | Request/response | Normalize value cases to synthetic tail resume |
| `with k` operation binder | No lexer token, grammar production, AST node, scope, or type | Planned source implementation | First-class `Resumption OneShot a b e` binder |
| `resume(k, value)` | No source expression or backend lowering | Planned source implementation | Typed control expression with selective-CPS lowering |
| Value-handler compatibility | Runtime/provider result returns through an Erlang call | Request/response | Proved-equivalent synthetic `resume` in tail position |
| Effect context | Explicit context exists in the compiler runtime | Internal helper | Add same-process handler frame and delimiter entries |
| Core Erlang effect backend | Emits `catena_effect_runtime:perform/4` and `with_handlers/3` calls | Request/response | Direct/resumable mode classification plus explicit bridges |
| Continuation capture | `catena_resumption` and `catena_perform` use supplied functions or `{resumed, Value}` placeholders | Marker-backed | Compiler-reified remainder to the selected delimiter |
| One-shot consumption | ETS-backed wrapper consumption is tested independently | Internal helper | Opaque process-affine runtime authority over a real continuation |
| Deep/shallow selection | Helper modules model scope/depth behavior, often with process-local context | Internal helper | Deep restoration around a captured remainder; shallow remains deferred |
| Multi-shot | State-copy and resume-count helpers exist | Deferred mode | Residual-effect admissibility and independent branch authority |
| Operational semantics | Normative written reductions | Semantic specification | Remains authoritative across later phases |
| `catena_resumption_oracle` | Explicit free-request evaluator with deterministic trace/state | Semantic oracle | Comparison evidence only; never production ABI |

## Relevant Module Inventory

### Request/response production path

| Modules | Honest boundary |
| --- | --- |
| `src/compiler/runtime/catena_effect_runtime.erl` | Explicit-context lookup, provider processes, replies, errors, and timeouts; no continuation capture |
| `src/compiler/codegen/catena_effect_codegen.erl` | Lowers perform/handle to the request/response runtime |
| `src/compiler/effects/catena_effects.erl`, `catena_effect_system.erl` | Public/internal orchestration around current direct-style effect execution |
| `src/compiler/effects/catena_handler.erl`, `catena_handler_stack.erl`, `catena_handler_check.erl` | Handler construction, lookup, execution, and validation helpers |
| `src/compiler/effects/catena_handler_types.erl`, `catena_handler_infer.erl`, `catena_handler_exec.erl` | Internal handler type descriptors and runtime checks, not the accepted first-class source `Resumption` type |

### Marker-backed and control-mode helpers

| Modules | Honest boundary |
| --- | --- |
| `src/compiler/effects/catena_resumption.erl` | Opaque Erlang wrapper around a supplied unary function; capture uses the tagged `{resumed, Value}` placeholder |
| `src/compiler/effects/catena_perform.erl` | Process-dictionary orchestration whose capture helper constructs the same marker continuation |
| `src/compiler/effects/catena_one_shot.erl` | Enforces consumption of stored helper data/functions; does not capture the post-perform remainder |
| `src/compiler/effects/catena_continuation_kind.erl` | Selects/describes helper continuation kinds |
| `src/compiler/effects/catena_deep_handler.erl`, `catena_shallow_handler.erl` | Scope/depth helper behavior, not deep/shallow restoration of a compiler-reified continuation |
| `src/compiler/effects/catena_depth_selection.erl`, `catena_handler_depth.erl` | Internal depth policy and selection |
| `src/compiler/effects/catena_multi_shot.erl`, `catena_state_copy.erl` | Repeated stored-state helper operations without the accepted residual-effect or branch semantics |

### Type and effect infrastructure to be extended

The following implemented families are real prerequisites, but none currently
type or lower the accepted `Resumption k a b e` source construct:

- `src/compiler/types/catena_types.erl`,
  `catena_infer_expr.erl`, `catena_infer_effect.erl`, and
  `catena_infer_unify.erl`;
- `src/compiler/types/catena_effect_constraints.erl`,
  `catena_effect_infer.erl`, `catena_effect_poly.erl`, and row-type modules;
- `src/compiler/effects/catena_op_signatures.erl`,
  `catena_ho_effects.erl`, and higher-order execution helpers;
- `src/compiler/validation/catena_effect_validation.erl`;
- the lexer, parser, AST, semantic normalization, compilation-unit, call
  resolution, erasure, origin, and backend modules named by the phased plan.

### Reference oracle

`src/compiler/effects/catena_resumption_oracle.erl` is the sole Phase 1 target
semantic oracle. It owns:

- explicit `Done`, free `Request`, and failure behavior;
- deterministic delimiter and resumption identities;
- actual Erlang closures representing the small evaluator's remainder;
- deep one-shot handler selection and reinstatement;
- value-case auto-resume;
- retained same-owner invocation across evaluator runs;
- deterministic ownership, lifetime, mode, re-entrancy, and consumption
  failures;
- a stable trace suitable for exact comparison.

The module comment and API forbid generated Catena application code from
calling the oracle. It is intentionally not a replacement production runtime.

## Oracle Trace Vocabulary

| Event | Meaning |
| --- | --- |
| `{delimiter_enter, D}` | A fresh handled boundary was installed |
| `{perform, E, Op, Args}` | Evaluation suspended at an operation request |
| `{propagate, D, E, Op}` | A frame did not match and preserved its context outward |
| `{capture, R, D, Owner, Depth, Kind}` | The real evaluator remainder was registered behind an opaque handle |
| `{handler_select, D, E, Op, Mode}` | The innermost matching case was selected |
| `{auto_resume, R, Value}` | A value case completed and invoked its synthetic tail resume |
| `{resume_begin, R, Value}` | One-shot authority moved from `fresh` to `running` |
| `{delimiter_return, D, Value, Reason}` | Evaluation reached the selected boundary |
| `{resume_return, R, Value}` | The delimiter result became the result of resume |
| `{retain, R, D}` | A fresh resumption escaped as first-class data |
| `{abort, R, D, Result}` | A case returned without invoking or retaining its continuation |
| `{consume, R, Reason}` | One-shot authority became permanently consumed |
| `{failure, Category, Details}` | Evaluation failed in a stable diagnostic family |

Trace values use logical owners and integer identities. They deliberately
exclude PIDs, references, timestamps, stack traces, and function identities.

## Stable Diagnostic Families

### Compile-time categories

| Category | Required use |
| --- | --- |
| `invalid_resumption_binder` | Malformed, duplicate, or otherwise invalid `with` binder |
| `resumption_binder_scope` | Binder referenced outside its operation-case scope before first-class escape is typed |
| `invalid_resume_target` | Resume target does not have a `Resumption` type |
| `resume_value_type_mismatch` | Supplied operation result does not unify with the resumption input |
| `resume_effect_mismatch` | Residual resumed effects are not admitted by the current context |
| `obvious_one_shot_reuse` | Static analysis proves duplicate invocation on a one-shot path |
| `unsupported_resumption_mode` | Source requests a shallow or multi-shot mode not yet promoted |
| `missing_resumption_lowering` | A resumable validated node reaches a backend without a classified lowering |
| `resumption_abi_mismatch` | Direct/resumable bridge or runtime ABI does not match validated metadata |

### Runtime and oracle categories

| Category | Required use |
| --- | --- |
| `invalid_resumption` | Value is malformed, forged, or names no registered authority |
| `invalid_resumption_version` | Opaque representation version is unsupported |
| `expired_resumption_owner` | Capturing owner died and retained resources were released |
| `wrong_resumption_owner` | A live non-owner process attempted invocation |
| `stale_resumption_delimiter` | Delimiter/frame metadata is closed, expired, absent, or incompatible |
| `unsupported_semantic_mode` | Runtime/oracle cannot execute the requested depth or kind |
| `resumption_reentrant` | Invocation observed the same one-shot authority in `running` state |
| `resumption_already_consumed` | Invocation observed the authority in `consumed` state |
| `resumption_value_type_mismatch` | A dynamic boundary receives an incompatible operation result |
| `unhandled_effect` | A request reaches the top of the explicit context |
| `handler_failure` | Production handler/provider execution fails before a valid result |

`oracle_callback_failure` and `invalid_oracle_computation` are test-model
integrity failures. They are not proposed Catena runtime diagnostics.

## Promotion Rule

A helper test proves only its named helper behavior. An oracle test proves
agreement with the Phase 1 model. Neither proves that Catena source parses,
types, lowers, and executes a real delimited continuation.

Only end-to-end evidence through the production source-to-BEAM path may change
`with`, `resume`, first-class retention, selective CPS, shallow, or multi-shot
rows from planned/deferred to implemented.

## Related Material

- [Delimited Resumption Operational Semantics](delimited_resumption_operational_semantics.md)
- [Delimited Resumption Architecture](delimited_resumption_architecture.md)
- [Phase 1 Plan](../planning/delimited-resumptions/phase-01-operational-semantics-feature-ledger-and-reference-oracle.md)
- [Effect Runtime](../runtime/effect_runtime.md)
- [Current Status](../planning/current_status.md)
