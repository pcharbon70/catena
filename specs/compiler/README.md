# Compiler Specs Index

This index covers the promoted compiler-domain surfaces.

- [core_compiler_pipeline.md](core_compiler_pipeline.md)
- [core_erlang_and_beam_backend.md](core_erlang_and_beam_backend.md)
- [beam_backend_feature_ledger.md](beam_backend_feature_ledger.md)
- [type_and_effect_system.md](type_and_effect_system.md)
- [pattern_matching_engine.md](pattern_matching_engine.md)
- [delimited_resumption_architecture.md](delimited_resumption_architecture.md)
- [delimited_resumption_operational_semantics.md](delimited_resumption_operational_semantics.md)
- [delimited_resumption_feature_ledger.md](delimited_resumption_feature_ledger.md)
- [delimited_resumption_user_guide.md](delimited_resumption_user_guide.md)

These specs capture the current compiler that exists in `src/compiler/*`,
including the fact that some roadmap items are partially implemented beyond
what the raw planning checklists show. Delimited-resumption Phases 1 through
8 provide the accepted semantics, source and typed frontend, authoritative
control-mode analysis, validated selective-CPS IR, executable deep/shallow and
bounded admissible multi-shot authority, loaded-BEAM lowering, safe
REPL/tooling views, proof-gated optimization, and dedicated conformance. The
final Phase 8 repository promotion gate remains pending.
