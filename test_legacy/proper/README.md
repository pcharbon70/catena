# Legacy PropEr Artifacts

This directory is now historical/documentary only.

There are currently no runnable PropEr suites left here. The maintained property/law workflow has converged on Catena's internal engine in `src/proptest/` with `src/testing/` serving as the compatibility/front-end layer.

This placeholder remains so the repository can record that:

- PropEr is no longer part of the active default workflow
- any future rediscovered PropEr-era artifacts should be treated as migration-or-retirement decisions, not revived implicitly
- the default `rebar3 eunit` / `make test` path should stay clear of legacy external-framework coupling
