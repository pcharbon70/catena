# Owned tasks and managed relationships

`Catena.Task.Kernel.check_selected/3` checks the exact `0.1.52` compound target.
Supply a decoded retained module, explicit capability-family bindings and
internal task/managed-process nodes. The programmatic roles are not final
source keywords. `Catena.Kernel.Backend.compile/1` independently verifies the
result and emits a deterministic task artifact with a closed main entry.

Raw spawn remains isolated. An explicit task scope registers children before
starting them and joins them before returning. The first observed child failure
cancels live siblings and waits for bounded cleanup. Scope, child and monitor
handles cannot escape or cross processes. Child callbacks have no residual
latent effects and inherit no surrounding parent handler.

Resource releases and task joins follow dynamic lifetime order. Actual handler
abandonment closes captured scopes; an original trap keeps its reason and
later cleanup failures remain evidence. Cancellation is cooperative at function
entry, recursive calls and blocking waits. Each child has explicit shutdown
grace. Forced expiry is distinct from successful cleanup. External process/VM
loss and noncooperative foreign execution have no cleanup guarantee.

Managed process handles carry the mailbox type and remain distinct from raw
Process handles. Typed monitor outcomes use caller-chosen labels for fixed
closed roles. Monitoring does not own the target. Managed links propagate
failure symmetrically; explicit trapping regions observe it instead. Private
lifecycle protocols never enter the user mailbox. Unlink invalidates the
caller's generation; it does not manufacture a terminal notification for the
other endpoint. General supervision and distributed transport remain separate.

`Catena.Task.Kernel.check/2` remains an unselected experimental entry. General
time is admitted separately through `Catena.Task.TimeKernel.check/3` at exact
0.1.53; those nodes remain rejected by the selected 0.1.52 entry. Historical
interfaces and signed formats are unchanged. See [cancellation and time](cancellation-and-time.md).

The [lifetime contract](https://github.com/pcharbon70/catena-research/blob/main/60-specification/process-lifetimes/owned-tasks-and-managed-relationships.md)
defines the admitted behavior. The `task_*_test.exs` behavioral
witnesses cover the reference model and real BEAM actors; the tag inventory is
navigation rather than semantic proof.
