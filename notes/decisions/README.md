# Architecture Decision Records (ADRs)

This directory contains Architecture Decision Records (ADRs) documenting significant architectural and design decisions made during the Catena compiler development.

## Purpose

ADRs help us:
- **Document why** decisions were made, not just what was decided
- **Preserve context** for future maintainers
- **Track alternatives** that were considered and rejected
- **Learn from experience** by reviewing past decisions
- **Avoid revisiting** settled questions

## Format

Each ADR follows this structure:

1. **Title**: Short, descriptive name (e.g., "Shared Parser Test Helpers")
2. **Status**: Proposed | Accepted | Rejected | Deprecated | Superseded
3. **Context**: What problem are we solving? What constraints exist?
4. **Decision**: What did we decide to do?
5. **Consequences**: What are the positive and negative outcomes?
6. **Alternatives Considered**: What other options did we evaluate?

## Naming Convention

ADRs are numbered sequentially:
```
0001-short-descriptive-name.md
0002-another-decision.md
0003-yet-another-decision.md
```

Numbers are zero-padded to 4 digits for sorting consistency.

## When to Write an ADR

Write an ADR when making decisions about:
- **Architecture**: Major structural choices (module organization, layer boundaries)
- **Technology**: Language features, libraries, tools, frameworks
- **Patterns**: Standard approaches to common problems
- **Trade-offs**: Choosing between competing concerns (performance vs clarity)
- **Processes**: Development workflows, testing strategies, documentation standards

**Examples**:
- ✅ "Use shared test helper module for parser tests"
- ✅ "Implement UTF-8 validation before lexing"
- ✅ "Choose PropEr over QuickCheck for property-based testing"
- ❌ "Fix typo in comment" (too trivial)
- ❌ "Implement parse_flow function" (implementation detail, not architecture)

## When NOT to Write an ADR

Don't write ADRs for:
- **Trivial changes**: Bug fixes, typos, formatting
- **Implementation details**: How to write a specific function
- **Obvious choices**: Using Erlang for BEAM VM project
- **Temporary experiments**: Spike solutions, prototypes

## Process

### 1. Propose

Create ADR with status "Proposed":
```markdown
**Status**: Proposed
**Decision Maker**: [Your Name]
**Date**: YYYY-MM-DD
```

Discuss with team/reviewers.

### 2. Accept

Update status when decision is made:
```markdown
**Status**: Accepted
**Date**: YYYY-MM-DD
**Implemented**: Commit [hash]
```

### 3. Supersede (if needed)

When a decision is replaced:
```markdown
**Status**: Superseded by ADR-00XX
**Date**: YYYY-MM-DD
**Reason**: [Why this decision was replaced]
```

### 4. Deprecate (if needed)

When a decision is no longer relevant:
```markdown
**Status**: Deprecated
**Date**: YYYY-MM-DD
**Reason**: [Why this is no longer applicable]
```

## Existing ADRs

### Accepted

- **[ADR-0001](0001-shared-parser-test-helpers.md)**: Shared Parser Test Helper Module (2025-11-16)
  - Extracts common test utilities to reduce duplication
  - Establishes pattern for parser test infrastructure

### Proposed

(None currently)

### Superseded

(None currently)

### Deprecated

(None currently)

## References

### ADR Resources

- [Michael Nygard's ADR Template](https://github.com/joelparkerhenderson/architecture-decision-record)
- [ADR Tools](https://github.com/npryce/adr-tools)
- [When to Write an ADR](https://adr.github.io/)

### Catena-Specific Conventions

1. **Link to Code**: Include commit hashes for implemented decisions
2. **Link to Reviews**: Reference code review documents that prompted the decision
3. **Quantify Impact**: Include metrics (lines saved, tests affected, etc.)
4. **Be Specific**: Include code examples, not just prose
5. **Update Status**: Keep status field current (accepted/superseded/deprecated)

## Examples from ADR-0001

### Good Context

✅ Clear problem statement:
> "During code review, we identified 40+ lines of duplicated helper functions across 3 parser test files."

### Good Decision

✅ Specific and actionable:
> "Create `catena_parser_test_helpers.erl` containing parse_flow/1, parse_type_sig/1, parse_single_decl/1, get_patterns/1, get_guards/1, get_body/1"

### Good Consequences

✅ Both positive and negative:
> "✅ Reduced duplication: -42 lines
> ⚠️ Additional module: One more file to maintain"

### Good Alternatives

✅ Explains why each was rejected:
> "Alternative 2: Extract ALL Helpers
> **Rejected**: Over-extraction reduces clarity"

## Questions?

If you're unsure whether to write an ADR, ask:
1. Will this decision affect multiple parts of the codebase?
2. Will future contributors need to understand *why* we made this choice?
3. Are there trade-offs or alternatives worth documenting?
4. Would "why did we do it this way?" be a reasonable question in 6 months?

If you answered "yes" to 2+ questions, write an ADR.

---

**Last Updated**: 2025-11-16
**Maintainer**: Catena Compiler Team
