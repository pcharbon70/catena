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

## Text operations at semantic revision 0.1.66

The separate `catena-text.etf` table binds Unicode 17.0.0, UAX #29 revision 47
and UAX #15 revision 57. Its embedded source manifest records the exact URL and
SHA-256 of `UnicodeData.txt`, `DerivedNormalizationProps.txt`,
`NormalizationTest.txt`, `DerivedCoreProperties.txt`, `GraphemeBreakProperty.txt`,
`GraphemeBreakTest.txt` and `emoji-data.txt`. The three added upstream files retain
their copyright and license headers. Regenerate only this table using
`elixir scripts/build_text_tables.exs`; the script performs no network requests.
The identifier table and its generator are unchanged.

The text runtime implements default extended grapheme boundaries and explicit
NFC/NFD/NFKC/NFKD. Official vector tests and the supplementary normalization
scalar invariant run in `test/catena/text_unicode_contract_test.exs`.
