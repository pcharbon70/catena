# Progress and Preservation

Revision `0.1.45` closes P132: the remaining metatheory targets
are stated with executable evidence, and the integrated theorem
arrives as a **composed statement** — the honest middle between
"each part works" and "the whole is proven."

## The effects and failure targets

For the shipped handler calculus (C005 unchanged):

1. **Installation preservation** — handling a request-performing
   computation preserves types modulo the handler's rows.
2. **Resume-once preservation** — the affine discipline holds
   statically and at runtime (consumed-token check).
3. **Return-clause preservation** — normal completion produces
   the output type.
4. **Effect progress** — a closed well-typed handled term is a
   value, performs an operation, or returns; never stuck.
5. **Trap terminality** — `trap(reason)` is the failure terminal:
   kinded reason preserved kernel-verbatim, no further step, other
   processes' worlds untouched.

```elixir
{:ok, core} = Catena.check_kernel(handled_program)
{:ok, 4, %{root_status: :terminated}} =
  Catena.Kernel.Stepper.run(core, "main")   # install + resume + return

assert {:trap, 100, _} = Catena.Kernel.Stepper.run(trap_core, "main")
```

## The composed theorem

**If** each component target holds (C002 data, C003 conditions,
C010 kernel, 0.1.45 effects) **and** the composition lemma is
discharged, **then** every closed well-typed program reaches the
three-way partition (value, trap, divergence) with types preserved
at each step and traces agreeing across targets. The lemma — that
the components combine without interference — is a **named proof
obligation** owned by the formal-validation program, never a
claim.

## The conditionals

Public processes extend the mailbox targets **iff** G084/G085 ship
with their own preservation statement. Foreign values preserve the
theorem **by construction** of C067's visible typed boundary — the
condition being that no other entry path exists, which the C067
exclusions enforce.

The normative contract is the research repository's
[Progress and Preservation Specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification/progress-and-preservation).
