# Vocabulary-neutral semantic task book

These prompts prepare the semantic foundation condition. They intentionally use
task IDs and diagrams or retained values instead of selecting Catena keywords or
surface grammar. Each final public-language counterpart remains held for P107
and P109.

## Task families

| ID | Prompt shape | Prediction key | Transfer or repair key |
| --- | --- | --- | --- |
| S1 | A context contains one value and a pure arrow changes the value. Choose the resulting context. | Structure is preserved; the arrow changes only the contained value. | Apply the same rule to a different context shape without adding effects. |
| S2 | Two independent contexts contain inputs for a binary pure arrow. Choose whether either input depends on the other. | Neither branch can observe the other's produced value. | Distinguish independent combination from dependent sequencing. |
| S3 | The second computation is selected from the first computation's value. Order four trace events. | The first result is available before the second computation is selected. | Repair an attempted independent combination that requires the first value. |
| S4 | A finite structure contains several effectful element computations. Predict the output shape and effect order declared by the fixture. | Shape is preserved and every admitted element computation is sequenced by the declared traversal rule. | Transfer from a list-shaped fixture to another finite traversable shape. |
| S5 | An operation request crosses a nearest matching handler and can resume once. Predict the trace. | The nearest matching handler receives the request; unrelated handlers do not. | Repair an unhandled or multiply resumed request using the supplied diagnostic. |
| S6 | Ordered clauses contain safe Boolean conditions. Select the first applicable clause. | Conditions refine matching but do not reorder clauses or perform effects. | Repair an unsafe condition without changing clause order. |
| S7 | A finite generator and filters construct an output collection. Predict order and multiplicity. | Generator order and repeated values are preserved unless an explicit rule removes them. | Transfer the construction to an independently typed finite source. |
| S8 | A structured diagnostic names a cause, semantic location, and machine applicability. Choose a safe repair. | Only an exact machine-applicable repair may be applied without a human semantic choice. | Reject a stale, speculative, or snapshot-mismatched repair. |

## Outcome codes

For each task record `correct-unaided`, `correct-after-procedural-restate`,
`incorrect`, `abandoned`, or `protocol-failure`. Record duration only as
`under-2m`, `2m-to-5m`, `over-5m`, or `not-completed`. Do not retain prose
answers. The future public-language condition must preserve the same semantic
keys and vary only the approved notation and diagnostic presentation.
