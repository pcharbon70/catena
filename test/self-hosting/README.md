# G141 self-hosting preflight

This directory contains the grammar-independent preflight for Catena's late-0.x
self-hosting milestone. It does not contain Catena compiler source, bootstrap
stages, or evidence that the compiler is self-hosted.

`preflight.json` fixes the BEAM through OTP 29 Erlang Abstract Format target,
retained Elixir recovery root, pure-pass-first port order, permitted residual
host services, required dual-implementation suites, stage evidence, exact or
declared semantic comparison, offline reproducibility, and rollback drill.

Run `mix test test/catena/self_hosting_test.exs` from the repository root. The
expected result validates the package and reports G141 blocked. The future
`bootstrap/` directory remains held until P109 is adopted and actual compiler
passes can be authored in Catena. A wrapper around Elixir compiler passes does
not count as self-hosting, and a fixed point alone does not prove the bootstrap
was trustworthy.
