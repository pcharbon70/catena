# Catena Unicode 17 tables

These files are the pinned Unicode 17.0.0 inputs for Catena 0.1.10 identifier
validation. The upstream text files retain their Unicode copyright and terms
of-use headers. `catena-unicode.etf` is the compact deterministic table read by
the compiler; its embedded `sources` map records every canonical URL and
SHA-256 digest.

Regenerate the table, without changing the pinned inputs, with:

```sh
elixir scripts/build_unicode_tables.exs
```

Use `--download` only when deliberately refreshing the vendored copies from
their recorded Unicode URLs. Any data-version change requires a new Catena
language revision and corresponding specification update.
