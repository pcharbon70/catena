# ADR-0004: Flow as Pragmatic Name for Arrow

## Status

Accepted

## Context

Catena uses pragmatic terminology to make category theory concepts accessible to developers. The language has established mappings for many category theory concepts:

- **Object** → Type
- **Morphism** → Transform
- **Category** → System
- **Functor** → Mapper
- **Applicative** → Applicator
- **Monad** → Pipeline
- **Comonad** → Extractor
- **Kleisli Arrow** → EffectfulTransform

However, no pragmatic name was established for **Arrow** — a category theory abstraction that generalizes functions to support parallel composition, fanout, and choice operations. Without a pragmatic name, documentation and planning documents have continued to use the category theory term "Arrow," which violates the language's design principle of accessible terminology.

## Decision

Catena will use **Flow** as the pragmatic name for the Arrow category theory abstraction.

### Mapping

| Category Theory Term | Pragmatic Name |
|---------------------|----------------|
| Arrow | **Flow** |
| Arrow's `arr` operation | `lift` |
| Arrow's `first` operation | `first` |
| Arrow's `>>>` operator | `>>>` (composition) |
| Arrow's `***` operator | `parallel` |
| Arrow's `&&&` operator | `split` |
| Arrow's `+++` operator | `choice` |
| Arrow's `\|\|\|` operator | `merge` |

### Trait Definition

```catena
trait Flow arr where
  -- Lift a pure function into a Flow
  lift : (a -> b) -> arr a b

  -- Compose two Flows (from System trait)
  compose : arr b c -> arr a b -> arr a c

  -- Process first element of a pair
  first : arr a b -> arr (a, c) (b, c)
end
```

### Rationale for "Flow"

1. **Intuitive Meaning**: "Flow" naturally describes data flowing through transformations, which is exactly what Arrow abstracts
2. **Consistent with Existing Naming**: Fits with other pragmatic names that describe *what it does* rather than mathematical jargon
3. **Parallel to "Pipeline"**: Just as Pipeline (Monad) describes sequential computation, Flow describes structured computation patterns
4. **Avoids Confusion**: Unlike "Transform" which could conflict with the existing keyword, "Flow" is unambiguous
5. **Industry Familiarity**: "Data flow," "control flow," and "flow programming" are familiar concepts to developers

## Consequences

### Positive Consequences

1. **Accessible Naming**: Developers can understand "Flow" without category theory background
2. **Consistent Documentation**: All planning and reference materials will use consistent pragmatic terminology
3. **Clear Mental Model**: "Flow" evokes pipelines, streams, and data movement — the core use cases for Arrow
4. **Complementary Naming**: "Flow" (parallel/static structure) complements "Pipeline" (sequential/dependent structure)

### Neutral Consequences

1. **Operator Naming**: Some operators (`first`, `>>>`) remain unchanged as they were already intuitive
2. **Standard Library Location**: Flow will be implemented in `lib/catena/stdlib/prelude.cat` alongside other category theory traits
3. **Effect Integration**: Flow integrates naturally with the existing EffectfulTransform (Kleisli) operations

### Requirements Imposed

1. **Documentation Updates**: All references to "Arrow" in planning documents must be updated to "Flow"
2. **Standard Library**: The Flow trait must be added to the Prelude with appropriate instances
3. **Test Coverage**: Flow laws must be added to the `Laws` module
4. **Type System**: The compiler must support the higher-kinded types required by Flow

## Related Decisions

- [ADR-0002: Minimal Core And Library-First Surface](ADR-0002-minimal-core-and-library-first-surface.md) — Flow will be implemented as a library trait, not a compiler intrinsic
- [ADR-0003: Explicit Effect Context Runtime](ADR-0003-explicit-effect-context-runtime.md) — Flow composition integrates with explicit effect tracking

## References

- [Standard Library Surface](../stdlib/standard_library_surface.md)
- [Flow Implementation Plan](../planning/flow-implementation-plan.md)
- [Category Theory Library Plan](../../notes/planning/category-theory-library/category-library-plan.md)
