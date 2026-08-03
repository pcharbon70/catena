# Specifications

Catena specifications attach typed, executable statements to exact language
subjects. They are optional to adopt, mandatory to interpret once declared,
and completely erased from runtime BEAM code in version 0.6.

This guide teaches the semantic feature. The JSON AST is normative for 0.6;
the source-shaped notation is illustrative because public punctuation remains
open.

## Start with a rule and an example

Imagine an exported function that returns a retry count:

```catena
retry_count : Job -> Int

verification positive_retry_count(value : Int) : Bool =
  value >= 0

specification RetryCountRules {
  rule retry_count_is_nonnegative
    subject retry_count
    checker positive_retry_count

    example queued_job
      arguments (retry_count(queued_job),)
      expected true
}
```

This notation expresses the selected model but does not propose final
keywords. Semantically:

- the **subject** resolves to a real exported language entity;
- the **rule** names a typed Boolean checker;
- the checker is **verification-only**;
- an **example** supplies exact literal arguments and an expected Boolean; and
- the compiler records outcomes as evidence tied to exact artifacts.

## The normative JSON shape

Inside a complete JSON AST 0.6 module, the corresponding sections resemble:

```json
{
  "specifications": [
    {
      "name": "retry_count_contract",
      "claims": [
        {
          "name": "retry_count_is_nonnegative",
          "kind": "rule",
          "subject": { "kind": "value", "name": "retry_count" },
          "checker": "positive_retry_count",
          "examples": [
            {
              "name": "zero_is_valid",
              "arguments": [0],
              "expected": true
            }
          ]
        }
      ]
    }
  ]
}
```

The named checker is an ordinary definition in the same module with
`"verification_only": true`, an explicit signature ending in `Bool`, and a
pure body. See
[`c006_specification_governance_test.exs`](../../test/catena/c006_specification_governance_test.exs)
for complete executable AST examples.

## What a rule can describe

A rule subject has one of these closed kinds:

- exported value;
- datatype;
- trait;
- instance;
- effect family;
- handler;
- module;
- package output;
- module interface;
- governed action; or
- named assurance profile.

The compiler resolves the name against the typed module and package graph.
Unknown names, private values where an export is required, kind mismatches,
and future subject kinds fail as `SPC001`. A claim is never accepted as opaque
metadata.

## Rule checkers are deliberately restricted

A 0.6 checker must:

- have an explicit function signature ending in `Bool`;
- accept every parameter declared by the rule;
- infer an empty effect row;
- use only pure 0.6 expressions and pure helper definitions;
- exclude `request`, `handle`, and `resume`; and
- remain unreachable from every runtime definition.

The ordinary type checker establishes the signature and purity boundary. An
independent dependency pass rejects any runtime call into verification-only
code before lowering.

Why require a fixed fragment? A specification result must not depend on a
clock, network service, random source, mutable process, ambient handler, or
which machine happens to compile the package.

## Examples are exact witnesses

An example calls the checker with a finite input and expected Boolean. Version
0.6 admits JSON integers, Booleans, and recursively nested tuples compatible
with the checker's parameters.

It excludes floats, constructors, opaque host terms, functions, processes,
references, ports, and binaries. Those values require a later canonical
encoding contract rather than host-dependent comparison.

Each example receives a deterministic 20,000 semantic-step budget and one
outcome:

| Outcome | Meaning |
| --- | --- |
| `supported` | the checker completed with the expected Boolean |
| `counterexample` | it completed with the opposite Boolean |
| `runtime_error` | the checked expression reached an evaluation fault |
| `budget_exhausted` | another step would exceed the fixed limit |

Only `supported` satisfies the example. An example is evidence for one exact
invocation, not proof of a universal rule.

## Stable identity separates names from meaning

Every claim receives a stable identifier from its package origin, module,
specification name, and claim name. It also receives a semantic digest covering
the elaborated subject, checker type and core, examples, assumptions, and
dependencies.

```mermaid
flowchart LR
    Location[Origin + module + local names] --> ID[Stable claim ID]
    Meaning[Typed subject + checker + examples + dependencies] --> Digest[Semantic digest]
    Formatting[Whitespace, comments, display labels] -. excluded .-> Digest
```

Formatting or moving source without changing those meanings preserves the
semantic digest. A meaning change invalidates evidence and approvals bound to
the previous digest.

## Keep evidence kinds honest

Version 0.6 distinguishes:

- **conformance evidence**, emitted by the compiler for a named successful
  checker or artifact audit;
- **example evidence**, recording the exact example and outcome;
- **attestation**, an external statement signed by an authorized principal;
  and
- **assumption**, an explicitly unverified premise admitted by policy.

These categories are not interchangeable. A signature proves who signed the
canonical bytes, not that the statement is true. An approval permits an
action, not a technical conclusion. An unavailable checker does not become an
assumption automatically.

## Assumptions remain visible

An assumption binds the exact claim digest, subject, reason, and logical
validity window. It counts only when every matching policy explicitly permits
that assumption kind and an authorized assumption role approves the exact
record.

Assumptions remain labelled in diagnostics and the assurance manifest. This
lets a downstream reviewer distinguish “checked,” “externally attested,” and
“accepted as a trust boundary.”

## Build-time adoption, not runtime monitoring

Declaring rules adopts typed checking and an assurance sidecar for the package
build. It does not require an organizational governance bundle.

```mermaid
flowchart LR
    Rules[Rules and examples] --> Check[Type check and evaluate]
    Check --> Evidence[Compiler evidence]
    Evidence --> Sidecar[Assurance manifest]
    Check --> Erase[Erase verification-only definitions]
    Erase --> Beam[Runtime-only BEAM]
```

The checker, rules, examples, evidence, and digests are absent from executable
functions, exports, literals, custom chunks, attributes, compile information,
and companion specialization functions. Fully discharged specifications must
leave identical runtime input byte-identical at the BEAM level.

Version 0.6 has no monitor-retaining profile. A declaration that genuinely
needs runtime enforcement belongs to future work with an explicit semantics,
effect, and cost contract.

## Use the assurance manifest

A 0.6 package build produces ordinary runtime artifacts plus a canonical
`catena-assurance-manifest`. The sidecar records claims, evidence, assumptions,
artifact hashes, compiler identity, dependencies, and the erasure audit.

Removing the sidecar after an admitted build does not change program
execution. Changing a bound BEAM or interface byte makes later assurance
verification fail.

For a package that uses specifications but no governance bundle:

```bash
./catena compile-package-ir package.catena-package.json
```

Such a package can build and emit assurance evidence. It cannot claim a
governed `publish` or `activate` result.

## Diagnose specification failures

| Family | Repair direction |
| --- | --- |
| `SPC001` | correct the subject name, visibility, or kind |
| `SPC002` | remove malformed or duplicate claim identity |
| `SPC003` | provide a typed, pure, verification-only checker |
| `SPC004` | make example arguments and expectation compatible |
| `EVD002` | inspect the counterexample or runtime fault |
| `EVD003` | simplify the checker; the fixed budget is part of 0.6 |
| `ERS001` | remove runtime reachability or retained assurance material |

Continue with [Governance](governance.md) when checked evidence must gate an
organizational action. Exact rules are in the
[normative 0.6 specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/specifications-and-governance).
