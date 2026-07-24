# Specs Planning Index

This directory holds Catena's canonical planning documents.

Current promoted planning view:

- [current_status.md](current_status.md)
- [law_verification_staged_plan.md](law_verification_staged_plan.md)
- [backend-hardening/](backend-hardening/README.md) - seven-phase work to make
  Core Erlang and BEAM generation fail closed, semantics-preserving, and
  executable across the promoted language surface
- [dialyzer-remediation/](dialyzer-remediation/README.md) - phased work to
  reach and enforce a zero-warning static-analysis boundary
- [spec-source-reconciliation/](spec-source-reconciliation/) - phased work to
  align promoted specifications with executable repository evidence
- `algebraic-effects/` - restored Phase 7 through Phase 14 algebraic-effects phase documents
- `proof-of-concept/` - restored proof-of-concept phase documents
- `property-testing/` - restored property-testing phase documents

This directory should hold the reconciled view that contributors need in order to understand what is actually implemented versus still planned.
