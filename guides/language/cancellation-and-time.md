# Cancellation and time

`Catena.Task.TimeKernel.check/3` checks the exact `0.1.53` compound target.
It extends owned task lifetimes with relative sleep, opaque absolute deadlines,
absolute waiting, and relative/absolute timed receive. Supply retained decoded
module structure, explicit capability families and internal semantic nodes.
These adapter roles do not choose final source or library vocabulary.

Durations are exact nonnegative integer nanoseconds. A deadline belongs to one
live scope and cannot escape, cross a message boundary or acquire meaning in
another owner. Repeated waits preserve its original budget. Large values are
not silently clamped: bounded host wait intervals reuse the same monotonic
deadline and round remaining waits upward to host granularity.

Timeout expressions execute once before scanning. Queued matches remain
eligible at zero timeout; fallback preserves rejected messages. A selected
branch cannot be replaced by a late reply. Cancellation can interrupt later
computation at an admitted continuation safe point without selecting a second
receive branch. User messages and private control stay separate, including in
nested receives and raw actors with explicit owned scopes.

Mandatory cleanup masks cooperative cancellation but retains finite release
and shutdown bounds. Expiry never claims successful cleanup. External process
loss, VM loss and noncooperative foreign code have no prompt-unwind promise.
The reference clock is explicit virtual time; evaluator fuel is not time.

The production backend verifies a closed main entry and emits deterministic
0.1.53 artifacts without an interface or signed-format extension. Exact 0.1.52
continues to reject general time nodes. Behavioral witnesses are in the
`task_time_*_test.exs` tests, supplemented by the existing owned-lifetime tests.

The [normative time contract](https://github.com/pcharbon70/catena-research/blob/main/60-specification/cancellation-and-time/deadlines-waits-and-cancellation.md)
defines these rules and their eight conformance obligations.
