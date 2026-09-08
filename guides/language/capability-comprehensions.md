# Capability Comprehensions

Revision `0.1.50` adds an explicit compiler-internal capability-tree boundary.
It does not change standalone `0.1.8` source or select final public vocabulary.

Build the existing `Catena.Comprehension` tree with `uses` on each effectful
qualifier, `yield_uses` on the yield, and an aggregate `uses` equal to their
union. Context entries optionally add a fourth field for their evaluated row.
The new boundary checks context entries in order and rejects forward or
recursive context dependencies. Each fragment is checked independently before
recursive worker signatures are built.

Call `Catena.Comprehension.Capability.check(tree, bindings, handlers: names)`.
The bindings map associates every decoded effect-slot name with its nominal
family identity. Distinct names can belong to the same family, provided their
operation signatures agree. The handler names are ordered outermost first and
wrap the whole comprehension. The result is `{:ok, core, advisories}` or a
structured diagnostic. The old `Comprehension.elaborate/1` path remains intact.

A false filter retains its already performed effects. An enclosing handler
that declines resumption aborts the whole remaining traversal. The source,
filter, binding and yield run in the same depth-first order on the reference
stepper and BEAM, including immediate failure.

Use `Catena.Kernel.Stepper.run(core, "main")` to evaluate the checked core, or
`Catena.Kernel.Backend.compile(core)` to produce a verified deterministic BEAM
artifact. Compilation requires a zero-argument main with no unhandled or
escaping capabilities. Artifact metadata identifies `0.1.50`; no importable
interface or signed format is produced by this new boundary.

The implementation supports closed, explicitly selected slots. It does not
provide a general open-row solver, a new public parser or a package-linking
interface. The governing rules are in the research archive's
[closed capability-kernel specification](https://github.com/pcharbon70/catena-research/blob/main/60-specification/closed-capability-kernel/identity-rows-and-comprehension-target.md).

Effectful context definitions execute at each reached reference, including
earlier context dependencies; they are not implicitly memoized or eagerly
forced. Dependent case patterns preserve source effects before selecting
elements. Unnecessary case-marker advisories use the selected 0.1.50 checker.
