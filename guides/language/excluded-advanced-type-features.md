# Excluded Advanced Type Features

Revision `0.1.44` closes D140: the advanced type features C001
excluded at `0.1.1` stay excluded, and the **seven-point arrival
gate** — previously prose in the research inbox — is now
normative text.

## The exclusion table

| Form | What it would admit |
| --- | --- |
| Impredicative instantiation | quantification over arbitrary types |
| Inferred higher rank | rank-2+ without the annotation boundary |
| First-class existential packages beyond declared constructors | existentials outside C002's rigid surface |
| General linear types | usage-counted values beyond C005's affine resumptions |
| Dependent types | types indexing on values |
| Unrestricted type families | computation at the type level |
| Higher-kinded polymorphism over arbitrary kinds | abstraction over type constructors |
| Unrestricted GADT inference | equalities without signature direction |

Rejections identify the profile boundary (`T009`, `T010`, the
frozen grammar's `T012`) rather than an unrelated unification
failure. C068's checked profile — predicative explicit higher
rank, signature-directed GADTs, branch-local equalities, explicit
rigid existentials — is the positive complement and stays
unchanged.

## The seven-point gate

A slice admitting any form must state, for that form: an
independent problem statement; evidence of repeated use; an
interaction audit (principality, coherence, erasure, resolution,
failure taxonomy); a formal semantics; an operational contract; a
diagnostic story; and a comparison with an ordinary library or
explicit core mechanism. Forms arrive independently or not at all
— no omnibus advanced-features revision.

## Witnesses

```elixir
# The gate's live diagnostics:
{:error, %{id: "T010"}} = Catena.check_json(gadt_without_signature)
{:error, %{id: "T012"}} = Catena.check_json(quantifier_in_parameter)

# The checked profile unchanged:
{:ok, core} = Catena.check_json(signature_directed_gadt)
{:ok, 41} = Catena.Reference.Evaluator.run(core, "main")
```

The normative contract is the research repository's
[Excluded Advanced Type Features Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/excluded-advanced-type-features).
