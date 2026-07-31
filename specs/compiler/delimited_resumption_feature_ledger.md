# Delimited Resumption Feature Ledger

## Status

Phase 7 executable mixed-mode Core/BEAM boundary for
[ADR-0006](../adr/ADR-0006-first-class-resumptions-and-selective-cps.md).

This ledger prevents internal helper names, marker callbacks, request/response
runtime behavior, and the reference oracle from being mistaken for promoted
Catena source-language delimited control. Phase 8 must update a row only
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

## Phase 6 Snapshot

| Measurement | Phase 6 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,252 pass; 0 failures; 0 skips | Core lowering, call graphs, artifacts, diagnostics, loaded-BEAM semantics, and all earlier gates are green |
| Parser conflicts | 38 shift/reduce; 0 reduce/reduce | Unchanged from the audited Phase 2 grammar boundary |
| Core and BEAM integration | implemented executable boundary | Validated selective-CPS graphs emit source-arity wrappers, private direct/CPS entries, runtime control calls, and OTP-accepted BEAM |
| Artifact/runtime contract | implemented executable boundary | Artifact format 2 validates exact control/runtime versions, handler features, identities, interface checksums, and dependency checksums before loading |
| Deferred modes | rejected | Deep one-shot is executable; shallow and multi-shot remain fail-closed pending Phase 7 |

## Phase 7 Section 7.2 Snapshot

| Measurement | Section 7.2 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,263 pass; 0 failures; 0 skips | Shallow runtime/Core/artifact integration and all earlier gates are green |
| Parser conflicts | 38 shift/reduce; 0 reduce/reduce | Unchanged from the audited grammar boundary |
| Shallow one-shot | implemented executable boundary | Resumption restores the selected handler's parent context while preserving unrelated frames and process affinity |
| Control/artifact contract | implemented versioned boundary | Control ABI 2, runtime ABI 2, artifact format 3, interface 3, and explicit handler modes fail closed on disagreement |
| Multi-shot | deferred | Static admissibility exists; repeated branch authority and budgets remain owned by Section 7.3 |

## Phase 7 Section 7.3 Snapshot

| Measurement | Section 7.3 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,272 pass; 0 failures; 0 skips | Multi-shot runtime, loaded-BEAM behavior, and all earlier gates are green |
| Focused multi-shot integration | 9 pass; 0 failures; 0 skips | Branch identity, failure isolation, nested one-shot state, ownership, budgets, capture policy, cleanup, artifacts, and loaded-BEAM execution are green |
| Multi-shot | implemented executable boundary | A closed empty residual row may authorize repeated, isolated same-process branches from the same compiler-reified continuation |
| State policy | conservative fail-closed boundary | Process providers, local value-provider state, and direct lexical PIDs, ports, and references are rejected; no external-world cloning is claimed |
| Resource policy | implemented versioned boundary | Runtime ABI 3 bounds invocations, retained words, reductions, cooperative timeout, and nested branch depth |
| Phase integration | pending | Section 7.4 still owns mixed-mode scenarios and the complete phase-ending repository gate |

## Phase 7 Completion Snapshot

| Measurement | Phase 7 result | Interpretation |
| --- | ---: | --- |
| Complete active EUnit suite | 5,280 pass; 0 failures; 0 skips | All Phase 7 and earlier repository behavior is green |
| Manifest conformance | 418 pass; 0 failures; 0 skips | Every currently registered stable scenario remains green |
| Mixed-mode integration | 8 pass; 0 failures; 0 skips | Depth orderings, retention, failure, owner identity, repeated branches, deterministic search, and negative mode/resource paths are executable |
| Artifact mode integrity | implemented fail-closed boundary | The handler-mode inventory embedded in loaded BEAM must exactly match the runtime contract |
| Specs governance | passed | 42 requirements, 11 scenarios, 20 evidence rows, 73 acceptance criteria, seven ADRs, and 301 local links validate |

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
| Runtime ABI implementation | Real compiler-shaped closures execute through the production resumption runtime, but generated Core does not yet construct or invoke them |
| Executable source implementation | Typed source lowers through validated selective CPS to runtime calls and OTP-accepted loaded BEAM with observable continuation semantics |
| Planned source implementation | Accepted by the ADR and architecture but absent from the compiler/runtime path |
| Deferred mode | Has an accepted conceptual meaning but lacks the required syntax, typing, runtime authority, or evidence for promotion |

## Source-To-Runtime Surface Ledger

| Surface | Current implementation | Classification | Promotion target |
| --- | --- | --- | --- |
| `perform` syntax and AST | Parsed and typed as the existing effect expression | Request/response | Preserve syntax; classify resumable suspension points |
| `handle` and operation cases | Parsed and typed cases emit deterministic Core handler frames and execute through selected deep or shallow contexts | Executable source implementation | Preserve the deep default and explicit shallow selection |
| `with k` operation binder | Emitted capture constructs opaque process-affine authority for the compiler-reified remainder | Executable source implementation | Preserve through later optimization and tooling |
| `resume(k, value)` | Emitted runtime invocation executes the delimited source remainder on its owner and returns the delimiter result | Executable source implementation | Preserve through later optimization and tooling |
| Value-handler compatibility | Synthetic tail auto-resume emits and executes the same exactly-once deep path | Executable source implementation plus request/response compatibility | Preserve both specified paths |
| Effect context | Generated wrappers create one context; emitted entries distinguish local resumable/value and process providers with nested lookup | Executable source implementation | Preserve explicit authority |
| Core Erlang effect backend | `catena_control_codegen` emits validated direct/CPS entries, performs, handlers, resumptions, bridges, closures, imports, and dictionaries | Executable source implementation | Phase 8 owns optimization and promotion tooling |
| Continuation capture | Generated binary CPS closures and captured contexts are registered behind opaque handles | Executable source implementation | Never claim ordinary Erlang stack capture |
| One-shot consumption | A private serialized registry atomically enforces `fresh -> running -> consumed` for every exit | Executable source implementation | Preserve deterministic failure behavior |
| Deep/shallow selection | Deep restoration reinstalls the selected frame; shallow restoration resumes from its parent context | Executable source implementation | Phase 8 owns promotion tooling |
| Multi-shot | Closed empty residual rows lower to runtime ABI 3, whose opaque handles execute repeated isolated, bounded same-process branches | Executable source implementation | Phase 8 owns dedicated conformance and public promotion tooling |
| Operational semantics | Normative written reductions | Semantic specification | Remains authoritative across later phases |
| `catena_resumption_oracle` | Explicit free-request evaluator with deterministic trace/state | Semantic oracle | Comparison evidence only; never production ABI |

## Relevant Module Inventory

### Request/response production path

| Modules | Honest boundary |
| --- | --- |
| `src/compiler/runtime/catena_effect_runtime.erl` | Promoted request/response operations plus local resumable frames, `perform_cps/5`, deep/shallow context selection, and provider separation |
| `src/compiler/runtime/catena_resumption_runtime.erl` | Opaque versioned handles, private captured/parent context authority, atomic one-shot or isolated multi-shot invocation, state-admissibility checks, resource budgets, retention leases, lifecycle monitors, deadlines, cleanup, and stable failures |
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
evidence. Only the legacy backend disposition path may project the exact
compiler-generated value-handler shape to the request/response
representation; the selective-CPS backend now consumes explicit control, and
malformed resumption leakage remains fail-closed.

### Phase 4 control analysis and selective-CPS infrastructure

The compiler retains and validates the control graph before executable
lowering:

- `catena_control_mode` classifies source regions and solves local call graphs;
- `catena_control_ir` owns the versioned node and graph contracts;
- `catena_selective_cps` lowers resumable regions and preserves direct ones;
- `catena_control_abi` defines entries, closures, continuations, and bridges;
- `catena_control_validate` proves ownership, arity, origin, authority,
  disposition, and bridge invariants;
- `catena_module_interface` publishes validated imported transform modes.

`catena_compilation_unit` retains the mode inventory, control IR, and passing
validation report before declaration disposition. Phase 6 connects this graph
to the Phase 5 runtime ABI.

### Phase 5 runtime infrastructure

- `catena_effect_runtime` owns typed explicit-context entries, nested lookup,
  local resumable/value execution, process-provider separation, and deep
  `perform_cps/5,6`;
- `catena_resumption_runtime` owns opaque handles, private continuation and
  context state, atomic one-shot and multi-shot branch authorization, process
  affinity, admissibility checks, resource budgets, leases, lifecycle
  monitors, deadlines, cleanup, and stable failures.

The Phase 5 integration suite executes real compiler-shaped closures through
these modules. Phase 6 generated application code now calls this path.

### Phase 6 Core, artifact, and call-graph infrastructure

- `catena_control_codegen` emits source-arity wrappers, private direct/CPS
  entries, control closures, runtime performs, handlers, resume/abort flow,
  imports, dictionaries, and explicit bridges;
- `catena_beam_artifact` format 2 validates and loads only matching BEAM,
  runtime, handler-feature, interface, and dependency contracts;
- `catena_runtime_contract` performs exact runtime version and feature checks;
- `catena_core_origin` publishes generated definitions and source-to-synthetic
  control chains without retaining private callable terms.

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
| `unsupported_resumption_mode` | Source or runtime requests a mode that is not implemented at that boundary |
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
| `inadmissible_multishot_context` | Multi-shot capture contains provider state or direct capabilities without accepted branch semantics |
| `resumption_budget_exceeded` | Multi-shot capture or invocation exceeds a retained-memory, invocation, reduction, timeout, or branch-depth limit |
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
