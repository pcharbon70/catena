# Phase 2: `with`/`resume` Syntax, AST, And Semantic Normalization

**Description:** This phase introduces the accepted source vocabulary without
inventing backend behavior prematurely. It parses explicit resumption binders
and resume expressions, preserves their source identity, and normalizes
existing value handlers into explicit tail-resume semantics.

**Status:** In progress (Section 2.1 complete).

**Dependencies:** Phase 1 complete.

## Section 2.1: Lexer And Grammar Surface

**Description:** Add `with` and `resume` to the maintained lexer/parser
boundary with precise identifier handling, recovery, and conflict accounting.

- [x] **Section 2.1 Complete**

### Task 2.1.1: Add Language Tokens

**Description:** Recognize `with` and `resume` as context-sensitive language
words with stable locations and identifier-boundary behavior.

- [x] **Task 2.1.1 Complete**

#### Subtask 2.1.1.1: Extend The Lexer Source

**Description:** Add rules to `catena_lexer.xrl`, update token inventories and
generators, and regenerate outputs only through the canonical build.

- [x] **Subtask 2.1.1.1 Complete**

#### Subtask 2.1.1.2: Test Keyword Boundaries

**Description:** Verify `with`, `resume`, identifiers such as `within` and
`resumed`, comments, whitespace, malformed boundaries, and source locations.

- [x] **Subtask 2.1.1.2 Complete**

### Task 2.1.2: Extend Handler And Expression Grammar

**Description:** Parse `operation(patterns) with k -> body` and
`resume(resumption, value)` without weakening existing handler syntax.

- [x] **Task 2.1.2 Complete**

#### Subtask 2.1.2.1: Parse Resumption Binders

**Description:** Add the optional `with` binder after operation parameters,
preserve its name and location, and reject missing, uppercase, duplicated, or
malformed binders with focused recovery.

- [x] **Subtask 2.1.2.1 Complete**

#### Subtask 2.1.2.2: Parse Resume Expressions

**Description:** Add a dedicated resume expression with exactly two operands,
well-defined precedence, nested-expression support, and targeted arity
diagnostics.

- [x] **Subtask 2.1.2.2 Complete**

**Implementation evidence:** The maintained lexer recognizes `with` and
`resume` without splitting longer identifiers. The grammar accepts both
nullary and parameterized explicit resumption binders, represents
`resume(resumption, value)` directly, and reports a stable error for every
non-binary resume arity. Focused lexer/parser regressions cover comments,
locations, boundaries, precedence, nesting, malformed binders, and legacy
value-handler syntax. The parser conflict audit moved from the Phase 1
baseline of 37 shift/reduce and 0 reduce/reduce conflicts to 39 shift/reduce
and 0 reduce/reduce conflicts. The two-conflict delta is recorded for the
focused attribution audit in Section 2.4.

## Section 2.2: Parsed AST, Utilities, And Pretty Printing

**Description:** Make the new source forms first-class parsed AST nodes rather
than punctuation reconstructed by later compiler passes.

- [ ] **Section 2.2 Complete**

### Task 2.2.1: Define Canonical Parsed Shapes

**Description:** Extend operation cases with optional resumption-binder
metadata and add a dedicated `resume_expr` carrying both operands and source
origin.

- [ ] **Task 2.2.1 Complete**

#### Subtask 2.2.1.1: Update AST Contracts

**Description:** Update AST unions, type documentation, location extraction,
depth, traversal, mapping, folding, equality, and validation utilities.

- [ ] **Subtask 2.2.1.1 Complete**

#### Subtask 2.2.1.2: Preserve Explicit Source Intent

**Description:** Retain whether a case used `with` explicitly so diagnostics
and formatting do not confuse user-written control handlers with generated
value-handler sugar.

- [ ] **Subtask 2.2.1.2 Complete**

### Task 2.2.2: Extend Pretty Printing And Round Trips

**Description:** Print valid canonical source for explicit binders and resume
expressions while keeping value-handler source concise.

- [ ] **Task 2.2.2 Complete**

#### Subtask 2.2.2.1: Print New Forms

**Description:** Emit `with k` only for explicit control handlers and print
`resume(k, value)` with stable precedence and layout.

- [ ] **Subtask 2.2.2.1 Complete**

#### Subtask 2.2.2.2: Add Parse/Print Round Trips

**Description:** Cover nested handlers, patterns, multiline bodies, resume in
larger expressions, comments, and source forms that resemble ordinary calls.

- [ ] **Subtask 2.2.2.2 Complete**

## Section 2.3: Semantic Normalization And Early Validation

**Description:** Normalize both handler forms into a common semantic shape and
reject structurally invalid control usage before type inference.

- [ ] **Section 2.3 Complete**

### Task 2.3.1: Normalize Value And Control Handlers

**Description:** Give every normalized operation case explicit resumption
metadata without changing existing value-handler evaluation order.

- [ ] **Task 2.3.1 Complete**

#### Subtask 2.3.1.1: Generate Tail Auto-Resume

**Description:** Introduce a collision-free synthetic binder and tail
`resume_expr`, preserve the original case origin, and ensure the operation
result expression is evaluated exactly once.

- [ ] **Subtask 2.3.1.1 Complete**

#### Subtask 2.3.1.2: Preserve Explicit Control Cases

**Description:** Carry the user's binder and body unchanged and mark the case
as requiring explicit control so no later pass inserts an implicit resume.

- [ ] **Subtask 2.3.1.2 Complete**

### Task 2.3.2: Add Structural Control Diagnostics

**Description:** Reject resume syntax that is invalid independently of its
eventual type and establish fail-closed dispositions for later phases.

- [ ] **Task 2.3.2 Complete**

#### Subtask 2.3.2.1: Validate Binder Scope And Shadowing

**Description:** Apply ordinary lexical rules, reject duplicate pattern/binder
names where ambiguous, and preserve deliberate nested shadowing with clear
locations.

- [ ] **Subtask 2.3.2.1 Complete**

#### Subtask 2.3.2.2: Reject Unsupported Backend Leakage

**Description:** Mark normalized resumption nodes unsupported at the current
backend boundary until later phases supply typing and CPS dispositions; never
lower them as ordinary calls or marker closures.

- [ ] **Subtask 2.3.2.2 Complete**

## Section 2.4: Phase 2 Integration Tests

**Description:** Prove that the complete source-to-normalized-AST path
understands `with` and `resume`, preserves existing handlers, and still fails
closed before unsupported backend execution.

- [ ] **Section 2.4 Complete**

### Task 2.4.1: Exercise Source And Normalization Paths

**Description:** Run lexer, parser, AST utility, pretty-printer, semantic, and
diagnostic paths on representative control-handler programs.

- [ ] **Task 2.4.1 Complete**

#### Subtask 2.4.1.1: Test Positive Syntax And Round Trips

**Description:** Cover nullary and parameterized operations, explicit
resumption binders, nested resume expressions, value-handler compatibility,
and parse/normalize/print/parse stability.

- [ ] **Subtask 2.4.1.1 Complete**

#### Subtask 2.4.1.2: Test Negative Syntax And Leakage

**Description:** Cover malformed binders, invalid resume arity, keyword
boundaries, invalid scope, parser recovery, and explicit rejection at typed
and backend boundaries that are not implemented yet.

- [ ] **Subtask 2.4.1.2 Complete**

### Task 2.4.2: Run Phase Completion Gates

**Description:** Validate generated frontend artifacts, parser-conflict
accounting, backward compatibility, and repository health before typing work.

- [ ] **Task 2.4.2 Complete**

#### Subtask 2.4.2.1: Audit Parser And Compatibility Deltas

**Description:** Publish the conflict count, prove any new conflict is
understood or removed, and run the maintained lexer, parser, semantic, AST,
stdlib-source, and handler regression suites.

- [ ] **Subtask 2.4.2.1 Complete**

#### Subtask 2.4.2.2: Run Repository Gates

**Description:** Run Phase 2 integration tests, `make check-specs`, and the
complete active EUnit suite and record the exact phase-ending evidence.

- [ ] **Subtask 2.4.2.2 Complete**
