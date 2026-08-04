# Contributing to Catena

Catena is an executable language-design prototype for the BEAM VM. Changes
should preserve the distinction between normative language decisions in
`catena-research` and their executable implementation in this repository.

## Start with the documentation

Before changing code, read:

1. [Catena Language Tour](LANGUAGE-TOUR.md);
2. [Compiler Architecture](guides/development/compiler-architecture.md);
3. [Intermediate Representations](guides/development/intermediate-representations.md);
4. [Diagnostics and Testing](guides/development/diagnostics-and-testing.md); and
5. the applicable
   [normative specification](https://github.com/pcharbon70/catena-research/tree/main/60-specification).

The research repository's
[Specification Authority](https://github.com/pcharbon70/catena-research/blob/main/SPECIFICATION-AUTHORITY.md)
defines document status, rendered labels, exact rule citations, and conflict
handling.

For a new semantic feature, follow
[Adding a Language Feature](guides/development/adding-a-language-feature.md).

## Install the toolchain

The repository pins Erlang/OTP and Elixir with asdf:

```bash
asdf install
asdf exec mix compile --warnings-as-errors
asdf exec mix test
```

The project deliberately has no runtime dependencies. `:logger` and `:crypto`
are OTP applications.

## Choose the correct change category

### Compiler bug

The implementation disagrees with an applicable normative chapter's stated
rule. Cite the document and heading, fix the implementation, and add the
smallest regression test that demonstrates the discrepancy. Conformance
evidence helps expose the bug but does not define a fallback rule.

### Documentation bug

The README, tour, or guide misstates current semantics or status. Correct the
documentation without changing language behavior. If the same misunderstanding
could recur in code, add or improve a test.

### Language change

The requested behavior is not settled by the specification. Develop and
approve the versioned rule in `catena-research` before treating compiler code
as normative. Do not use a pull request to smuggle an unresolved syntax or
semantic decision into the executable model.

New prototype slices use the next unused `0.1.n` patch in
`Catena.LanguageVersion`. Update every AST, interface, package, artifact,
signature-domain, guide, and conformance boundary that the slice changes.
Do not infer ordering or replacement from the number alone; the normative
specification must still state applicability explicitly.

### Internal refactor

Runtime output, diagnostics, typed-core evidence, interfaces, and assurance
artifacts should remain unchanged unless the change explicitly says otherwise.
Use byte and canonical-artifact comparisons where relevant.

## Resolve disagreements explicitly

When the compiler, a test, an executable reference, or a guide disagrees:

1. identify the applicable normative document and heading;
2. suspend the affected conformance claim;
3. compare each artifact with that rule rather than ranking the executable
   artifacts against one another;
4. repair the non-normative artifact and add regression evidence; or
5. if normative chapters conflict or remain silent, resolve the language text
   before implementing a new behavior.

A larger specification version does not win by itself. Normative text must
explicitly state an applicability or replacement relationship. Compiler
behavior, even when tested and released, never silently amends Catena.

## Keep changes focused

- Work on a topic branch based on the repository's current default development
  branch.
- Do not mix speculative language design, generated artifacts, and unrelated
  cleanup into one commit.
- Preserve user changes already present in the worktree.
- Do not rewrite or amend an immutable conformance commit recorded by the
  research archive.
- Keep follow-up documentation or maintenance changes in descendant commits.

## Elixir style

- Run `mix format`; do not hand-format around it.
- Prefer explicit small modules aligned with one semantic responsibility.
- Return `{:ok, value}` or `{:error, %Catena.Diagnostic{}}` at public compiler
  boundaries.
- Use `Catena.TypeError` only inside protected semantic phases that translate
  it back to a diagnostic.
- Avoid process-global state, ambient configuration, network access, and
  nondeterministic iteration in compiler semantics.
- Sort records before canonical encoding or output whenever order is not
  semantically source-defined.
- Keep deterministic step budgets explicit and test exhaustion separately.
- Never accept private signing keys as compiler, package, environment, or CLI
  input.

## Preserve architectural boundaries

1. Decode protocol shape in `Catena.AST.Decoder`.
2. Resolve types, effects, identities, and selections during elaboration.
3. Recheck proof-relevant facts independently in typed core or the dedicated
   verifier.
4. Lower only verified core in `Catena.Backend.ErlangAbstract`.
5. Generate BEAM only through `Catena.OTP.Compiler` and OTP 29 Abstract Format.
6. Put separate-compilation facts in digest-bound interfaces, not runtime
   layout assumptions.
7. Keep governance authority outside source modules and private keys outside
   the compiler.
8. Remove verification-only and governance material before BEAM generation.

## Tests

During development, run the focused test file:

```bash
asdf exec mix test test/catena/c003_clause_condition_test.exs --trace
```

Before committing or requesting review, run:

```bash
asdf exec mix format --check-formatted
asdf exec mix clean
asdf exec mix compile --warnings-as-errors
asdf exec mix test
asdf exec mix escript.build
git diff --check
```

Test accepted and rejected behavior. For a new core invariant, include forged
typed-core evidence. For runtime behavior, load and execute the generated
module and clean it from the VM. For security-sensitive code, include replay,
substitution, duplicate, malformed, and path attacks.

Reference evaluators must remain structurally independent from the production
path they check.

## Diagnostics

- Preserve existing IDs when meaning and repair remain the same.
- Introduce a new ID only through the applicable normative diagnostic family.
- Assert structured details that matter to repair.
- Treat post-inference verifier failures as compiler defects, not source errors.
- Keep budget exhaustion distinct from a semantic falsehood or denial.

## Documentation

Update documentation in the same change when behavior or navigation changes:

- `README.md` for repository status, commands, and architecture summary;
- `LANGUAGE-TOUR.md` for the programmer's compact model;
- `guides/` for task and developer explanations; and
- `catena-research` for normative or research changes.

Label Catena source examples as illustrative until the source parser and
surface grammar are normative. Executable examples should use current JSON AST
or tests.

## Pull-request description

Explain:

- the problem and applicable specification/checklist item;
- the semantic and implementation boundary;
- representations and modules changed;
- diagnostics added or preserved;
- reference, differential, adversarial, and runtime evidence;
- compatibility and erasure impact;
- commands run and results; and
- documentation updated.

Call out deliberately excluded work so reviewers do not infer a larger feature
than the change implements.

## Security-sensitive contributions

Use generated ephemeral keys and temporary directories in tests. Never commit
real keys, signing payloads tied to private deployments, organization trust
roots, or production assurance records.

This repository does not currently publish a dedicated security-reporting
channel. Avoid placing sensitive exploit details or credentials in a public
issue; contact the repository owner through an established private channel
before disclosure.

## Review checklist

- [ ] Behavior matches the explicitly applicable normative chapter and the PR
      cites its governing heading.
- [ ] Older JSON AST and interface versions remain deliberately supported.
- [ ] Every implicit semantic choice is explicit in typed core.
- [ ] Independent verification rejects forged evidence.
- [ ] Backend output preserves order, identity, and effects.
- [ ] OTP 29 Abstract Format remains the sole BEAM path.
- [ ] Diagnostics have stable IDs, paths, and repair details.
- [ ] Deterministic limits and outputs are tested.
- [ ] Runtime/reference or policy/reference paths remain independent.
- [ ] Interfaces, package artifacts, and assurance binding are tested when affected.
- [ ] Compile-time evidence is erased unless runtime semantics explicitly require it.
- [ ] Formatting, warning-free compilation, full tests, escript build, and diff checks pass.
- [ ] README, tour, guides, specification links, and status wording are current.
