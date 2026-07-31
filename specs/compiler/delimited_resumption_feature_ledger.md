# Delimited Resumption Feature Ledger

## Status

Phase 5 executable runtime-ABI boundary for
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

## Phase 2 Snapshot

| Measurement | Phase 2 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,128 pass; 0 failures; 0 skips | Source, AST, normalization, compatibility, and leakage gates are green |
| Parser conflicts | 38 shift/reduce; 0 reduce/reduce | One audited delta: `resume` begins a primary expression in juxtaposition application |
| Resumption source syntax | implemented frontend | `with` binders and binary `resume` expressions parse, round-trip, and carry locations |
| Resumption normalized AST | implemented frontend | Explicit cases are preserved; value cases receive compiler-authored tail auto-resume |
| Resumption typing/CPS/runtime | absent | Explicit control fails closed before type inference/backend execution |

## Phase 3 Snapshot

| Measurement | Phase 3 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,164 pass; 0 failures; 0 skips | Kind, type, effect, first-class flow, compatibility, and fail-closed gates are green |
| Parser conflicts | 38 shift/reduce; 0 reduce/reduce | Unchanged from the audited Phase 2 grammar boundary |
| Resumption typing | implemented frontend | All four parameters, source origins, residual rows, schemes, and first-class flow are retained |
| Resumption CPS/runtime | absent | Explicit control remains rejected before backend success; automatic value handlers retain request/response execution |

## Phase 4 Snapshot

| Measurement | Phase 4 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,192 pass; 0 failures; 0 skips | Control analysis, selective-CPS IR, ABI, validation, imports, and all earlier gates are green |
| Parser conflicts | 38 shift/reduce; 0 reduce/reduce | Unchanged from the audited Phase 2 grammar boundary |
| Control-mode analysis | implemented compiler boundary | Typed effects, handlers, calls, imports, traits, higher-order capabilities, and open rows produce one retained direct/resumable inventory |
| Selective-CPS IR | implemented compiler boundary | Deterministic delimiters, continuations, resumptions, resume/abort nodes, calls, bridges, origins, and dispositions are retained and validated |
| Resumption runtime/Core | absent | Explicit control still fails closed before Core success; Phase 5 runtime authority and Phase 6 Core lowering remain required |

## Phase 5 Snapshot

| Measurement | Phase 5 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,233 pass; 0 failures; 0 skips | Opaque authority, deep handler contexts, lifecycle cleanup, runtime diagnostics, and all earlier gates are green |
| Parser conflicts | 38 shift/reduce; 0 reduce/reduce | Unchanged from the audited Phase 2 grammar boundary |
| Resumption runtime | implemented runtime ABI | Real compiler-shaped closures execute on their capturing process through opaque deep one-shot handles |
| Context and lifecycle | implemented runtime ABI | Local resumable/value and process-provider entries, retained leases, owner/provider monitors, timeouts, and cleanup are explicit |
| Core integration | absent | Validated Phase 4 nodes are not yet emitted as runtime calls; explicit source control continues to fail closed before Core success |

## Classification Vocabulary

| Classification | Meaning |
| --- | --- |
| Request/response | Computes an operation result and returns it to an ordinary direct-style caller; does not capture a language continuation |
| Internal helper | Implements useful handler, kind, depth, type, or state behavior but is not itself a source-level delimited-control implementation |
| Marker-backed | Wraps a function or value that demonstrates an API shape without executing the actual remainder after `perform` |
| Semantic oracle | Independently executes the normative model for comparison and tests; never linked by generated application code |
| Frontend implementation | Source parses, preserves intent, normalizes, and validates structurally, but lacks the typing/lowering/runtime evidence required for promotion |
| Typed frontend implementation | Source reaches kind, type, effect, flow, and typed-artifact validation, but lacks executable continuation lowering/runtime evidence |
| Compiler IR implementation | Typed source reaches authoritative control classification, selective-CPS lowering, and fail-closed graph validation, but lacks production runtime/Core execution |
| Runtime ABI implementation | Real compiler-shaped closures execute through the production deep one-shot runtime, but generated Core does not yet construct or invoke them |
| Planned source implementation | Accepted by the ADR and architecture but absent from the compiler/runtime path |
| Deferred mode | Has an accepted conceptual meaning but lacks the required syntax, typing, runtime authority, or evidence for promotion |

## Source-To-Runtime Surface Ledger

| Surface | Current implementation | Classification | Promotion target |
| --- | --- | --- | --- |
| `perform` syntax and AST | Parsed and typed as the existing effect expression | Request/response | Preserve syntax; classify resumable suspension points |
| `handle` and operation cases | Parsed and typed cases lower to deterministic control nodes; the runtime executes equivalent real-closure fixtures with local deep frames | Compiler IR plus runtime ABI implementation | Phase 6 emits the validated graph through the runtime |
| `with k` operation binder | Binds `Resumption OneShot a b e`; runtime capture constructs opaque process-affine authority for a supplied compiler closure | Compiler IR plus runtime ABI implementation | Phase 6 connects the lowered binder to capture |
| `resume(k, value)` | Typed authority lowers to validated resume nodes; runtime invocation executes the supplied delimited remainder on its owner | Compiler IR plus runtime ABI implementation | Phase 6 emits the runtime invocation |
| Value-handler compatibility | Synthetic tail auto-resume is typed/lowered and runtime value cases execute the same exactly-once deep path | Compiler IR, runtime ABI, and request/response compatibility | Phase 6 emits the normalized form |
| Effect context | Explicit contexts distinguish local resumable frames, local value providers, and process-backed providers with nested lookup | Runtime ABI implementation | Consume these entries from emitted Core |
| Core Erlang effect backend | Direct code still emits `catena_effect_runtime:perform/4` and `with_handlers/3`; validated CPS nodes are not emitted yet | Request/response plus fail-closed compiler IR | Phase 6 Core emission for the Phase 4 ABI |
| Continuation capture | Phase 4 reifies remainder structure; Phase 5 privately registers supplied binary CPS closures and captured contexts behind opaque handles | Compiler IR plus runtime ABI implementation | Phase 6 supplies emitted compiler closures |
| One-shot consumption | A private serialized registry atomically enforces `fresh -> running -> consumed` for every exit | Runtime ABI implementation | Consume from emitted `resume` nodes |
| Deep/shallow selection | Deep restoration is executable through retained contexts; shallow remains rejected | Runtime ABI implementation plus deferred mode | Phase 6 emits deep; Phase 7 owns shallow |
| Multi-shot | State-copy and resume-count helpers exist | Deferred mode | Residual-effect admissibility and independent branch authority |
| Operational semantics | Normative written reductions | Semantic specification | Remains authoritative across later phases |
| `catena_resumption_oracle` | Explicit free-request evaluator with deterministic trace/state | Semantic oracle | Comparison evidence only; never production ABI |

## Relevant Module Inventory

### Request/response production path

| Modules | Honest boundary |
| --- | --- |
| `src/compiler/runtime/catena_effect_runtime.erl` | Promoted request/response operations plus non-promoted local resumable frames, `perform_cps/5`, deep context restoration, and provider separation |
| `src/compiler/runtime/catena_resumption_runtime.erl` | Opaque versioned handles, private closure/context authority, atomic one-shot invocation, retention leases, lifecycle monitors, deadlines, cleanup, and stable failures |
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

### Phase 3 type and effect infrastructure

The following families now type the accepted `Resumption k a b e` source
construct, retain its evidence, and reject unsupported lowering:

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

`catena_resumption_normalize` remains the maintained semantic normalizer.
Phase 3 extends kinds, internal types, substitution, schemes, unification,
handler and resume inference, effect rows, and conservative first-class flow
validation. Compilation units retain normalized nodes and typed origin
evidence. Only the backend disposition path may project the exact
compiler-generated value-handler shape to the legacy request/response
representation; explicit control and other resumption leakage remain
fail-closed.

### Phase 4 control analysis and selective-CPS infrastructure

The compiler now retains and validates the non-executable control graph:

- `catena_control_mode` classifies source regions and solves local call graphs;
- `catena_control_ir` owns the versioned node and graph contracts;
- `catena_selective_cps` lowers resumable regions and preserves direct ones;
- `catena_control_abi` defines entries, closures, continuations, and bridges;
- `catena_control_validate` proves ownership, arity, origin, authority,
  disposition, and bridge invariants;
- `catena_module_interface` publishes validated imported transform modes.

`catena_compilation_unit` retains the mode inventory, control IR, and passing
validation report before declaration disposition. Phase 6 must connect this
graph to the Phase 5 runtime ABI.

### Phase 5 runtime infrastructure

- `catena_effect_runtime` owns typed explicit-context entries, nested lookup,
  local resumable/value execution, process-provider separation, and deep
  `perform_cps/5`;
- `catena_resumption_runtime` owns opaque handles, private continuation and
  context state, atomic authorization, process affinity, leases, lifecycle
  monitors, deadlines, cleanup, and stable failures.

The Phase 5 integration suite executes real compiler-shaped closures through
these modules. Generated application code does not call this path until
Phase 6 lowers the validated control graph.

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
| `resumption_binder_scope` | An unbound binder name is referenced outside its operation-case scope |
| `invalid_resumption_representation` | Source attempts to construct, deconstruct, forge, or shadow opaque representation vocabulary |
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

Only end-to-end evidence through the production source-to-BEAM path may
promote executable `with`, `resume`, first-class retention, shallow, or
multi-shot semantics. Phase 5 may accurately claim an executable production
runtime ABI for real compiler-shaped closures, but not source-to-Core/BEAM
integration or promoted source behavior.

## Related Material

- [Delimited Resumption Operational Semantics](delimited_resumption_operational_semantics.md)
- [Delimited Resumption Architecture](delimited_resumption_architecture.md)
- [Phase 1 Plan](../planning/delimited-resumptions/phase-01-operational-semantics-feature-ledger-and-reference-oracle.md)
- [Phase 2 Plan](../planning/delimited-resumptions/phase-02-with-resume-syntax-ast-and-semantic-normalization.md)
- [Phase 3 Plan](../planning/delimited-resumptions/phase-03-first-class-resumption-kinds-types-and-effects.md)
- [Phase 4 Plan](../planning/delimited-resumptions/phase-04-control-mode-analysis-and-selective-cps-ir.md)
- [Phase 5 Plan](../planning/delimited-resumptions/phase-05-deep-one-shot-runtime-and-resumption-ownership.md)
- [Effect Runtime](../runtime/effect_runtime.md)
- [Current Status](../planning/current_status.md)
