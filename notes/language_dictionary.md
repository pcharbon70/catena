# Flowing Shapes Language – Developer Guide

This document presents the core category‑theory abstractions for your language using developer‑friendly naming that aligns with the “flowing shape” aesthetic. Each concept shows: your language name, the canonical category‑theory term in parentheses, a beginner‑friendly explanation, and how it maps into your system.

---

## 1. Core Building Blocks

### Shape (Object)
A **Shape** is a data structure or type. In category theory, an *Object* is a fundamental entity in a category. In your language, Shapes are the foundational types that Flows transform.

### Flow (Morphism)
A **Flow** describes how one Shape transforms into another. In Category Theory, a *Morphism* (or arrow) is a structural transformation between objects. Flows correspond to functions or transformations in your language.

### FlowSpace (Category)
A **FlowSpace** is a space which contains Shapes and Flows, and defines how Flows compose and what identities exist. In Category Theory, a *Category* is a collection of objects and morphisms with composition and identity.

### Comparable (Setoid)
A **Comparable** Shape has an equality or equivalence relation defined. A *Setoid* is a set equipped with an equivalence relation. Use this when you need custom equality semantics in your language.

### Combiner (Semigroup)
A **Combiner** is a Shape that supports an associative binary combine operation. A *Semigroup* is a set with one associative operation. E.g., string concatenation, list append.

### Accumulator (Monoid)
An **Accumulator** is a Combiner that also includes a default or “empty” element (identity). A *Monoid* is a semigroup with an identity element. Useful for folding and reduction.

### Reversible Accumulator (Group)
A **Reversible Accumulator** is an Accumulator where each element has an inverse. A *Group* is a monoid where every element is invertible. Example: integers under addition.

---

## 2. Functorial Structure (Mappers & Contexts)

### Mapper (Functor)
A **Mapper** lifts Flows so they operate inside a container or context (Shape) rather than only on bare Shapes. A *Functor* is a structure‑preserving mapping between categories (or an endofunctor within one category). In your language it means applying a Flow to a Shape inside a context.

### Context Shift (Natural Transformation)
A **Context Shift** transforms one Mapper into another while preserving the structure of flows. A *Natural Transformation* is a mapping between functors that respects their action on objects and morphisms.

### Structured Mapper (Applicative Functor)
A **Structured Mapper** is a Mapper with the ability to apply Shapes of Flows to Shapes of Values, enabling parallel or independent effects. An *Applicative Functor* has both mapping and application inside a context.

---

## 3. Workflow Structures (Monads & Related)

### Workflow (Monad)
A **Workflow** is a context that supports chaining of Flows: you can lift values into the context, apply effectful Flows, and sequence operations. A *Monad* is a functor plus structure for sequencing computations.

### Effectful Flow (Kleisli Arrow)
An **Effectful Flow** is a Flow whose result is a Workflow. In Category Theory, a *Kleisli Arrow* is a morphism into the monadic context.

### Workflow Layer (Monad Transformer)
A **Workflow Layer** augments an existing Workflow with additional capabilities. A *Monad Transformer* is a construction that stacks monadic behavior.

### Context Reader (Comonad)
A **Context Reader** is a structure from which you can extract values or navigate contextual data. A *Comonad* is dual to a monad.

---

## 4. Universal Constructions

### Bundle (Product)
A **Bundle** is a composite Shape that holds two (or more) Shapes together. A *Product* is the categorical “and‑type” of two objects.

### Choice (Coproduct)
A **Choice** is a Shape representing “one of many” possibilities. A *Coproduct* is the dual of product.

### Unit (Terminal Object)
A **Unit** Shape has exactly one possible value. A *Terminal Object* has a unique morphism from every object.

### Never (Initial Object)
A **Never** Shape has no possible values. An *Initial Object* is the dual of terminal.

### SyncJoin (Pullback)
A **SyncJoin** combines Shapes that must agree on a shared flow or constraint. A *Pullback* models this in Category Theory.

### Merge (Pushout)
A **Merge** attaches two Shapes along shared structure. A *Pushout* is the dual of pullback.

---

## 5. Structured FlowSpaces

### Composable FlowSpace (Monoidal Category)
Supports combining Shapes in parallel. A *Monoidal Category* formalizes parallel composition.

### Structured FlowSpace (Cartesian Category)
Supports Bundles and duplication. A *Cartesian Category* has product structure.

### Function FlowSpace (Cartesian Closed Category)
A FlowSpace where Flows themselves are Shapes. A *Cartesian Closed Category* supports exponentials (function types).

### Shape Mapper (Endofunctor)
A Mapper mapping inside the same FlowSpace. An *Endofunctor* is a functor from a category to itself.

### Dual Mapper (Bifunctor)
A Mapper operating on two Shapes. A *Bifunctor* takes two inputs.

---

## 6. Overview Diagram

```mermaid
flowchart TD

    subgraph Core["Core Structures"]
        SH[Shape (Object)]
        FL[Flow (Morphism)]
        FS[FlowSpace (Category)]
    end

    subgraph Algebra["Algebraic Structures"]
        CM[Combiner (Semigroup)]
        AC[Accumulator (Monoid)]
        RA[Reversible Accumulator (Group)]
    end

    subgraph Functorial["Functorial Layer"]
        MP[Mapper (Functor)]
        SM[Structured Mapper (Applicative)]
        CS[Context Shift (Natural Transformation)]
    end

    subgraph WorkflowLayer["Workflow Structures"]
        WF[Workflow (Monad)]
        EF[Effectful Flow (Kleisli Arrow)]
        WL[Workflow Layer (Monad Transformer)]
        CR[Context Reader (Comonad)]
    end

    subgraph Constructions["Universal Constructions"]
        BD[Bundle (Product)]
        CH[Choice (Coproduct)]
        UT[Unit (Terminal Object)]
        NV[Never (Initial Object)]
        SJ[SyncJoin (Pullback)]
        MG[Merge (Pushout)]
    end

    SH --> FL
    FL --> FS

    AC --> CM
    RA --> AC

    MP --> SM
    MP --> CS
    SM --> WF

    WF --> EF
    WF --> WL
    CR --> MP

    BD --> FS
    CH --> FS
    UT --> FS
    NV --> FS
    SJ --> FS
    MG --> FS
```

---

**End of Developer Guide**

