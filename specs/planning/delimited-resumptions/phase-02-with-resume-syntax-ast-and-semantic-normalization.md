# Phase 2: `with`/`resume` Syntax, AST, And Semantic Normalization

**Description:** This phase introduces the accepted source vocabulary without
inventing backend behavior prematurely. It parses explicit resumption binders
and resume expressions, preserves their source identity, and normalizes
existing value handlers into explicit tail-resume semantics.

**Status:** Complete.

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
baseline of 37 shift/reduce and 0 reduce/reduce conflicts to an interim 39
shift/reduce and 0 reduce/reduce conflicts. Section 2.4 removed two redundant
nullary operation-case productions and records the final 38/0 count and its
single remaining attribution.

## Section 2.2: Parsed AST, Utilities, And Pretty Printing

**Description:** Make the new source forms first-class parsed AST nodes rather
than punctuation reconstructed by later compiler passes.

- [x] **Section 2.2 Complete**

### Task 2.2.1: Define Canonical Parsed Shapes

**Description:** Extend operation cases with optional resumption-binder
metadata and add a dedicated `resume_expr` carrying both operands and source
origin.

- [x] **Task 2.2.1 Complete**

#### Subtask 2.2.1.1: Update AST Contracts

**Description:** Update AST unions, type documentation, location extraction,
depth, traversal, mapping, folding, equality, and validation utilities.

- [x] **Subtask 2.2.1.1 Complete**

#### Subtask 2.2.1.2: Preserve Explicit Source Intent

**Description:** Retain whether a case used `with` explicitly so diagnostics
and formatting do not confuse user-written control handlers with generated
value-handler sugar.

- [x] **Subtask 2.2.1.2 Complete**

### Task 2.2.2: Extend Pretty Printing And Round Trips

**Description:** Print valid canonical source for explicit binders and resume
expressions while keeping value-handler source concise.

- [x] **Task 2.2.2 Complete**

#### Subtask 2.2.2.1: Print New Forms

**Description:** Emit `with k` only for explicit control handlers and print
`resume(k, value)` with stable precedence and layout.

- [x] **Subtask 2.2.2.1 Complete**

#### Subtask 2.2.2.2: Add Parse/Print Round Trips

**Description:** Cover nested handlers, patterns, multiline bodies, resume in
larger expressions, comments, and source forms that resemble ordinary calls.

- [x] **Subtask 2.2.2.2 Complete**

**Implementation evidence:** The AST contract now includes first-class
`resume_expr` and optional resumption-binder metadata while retaining the
legacy value-case tuple as an explicit compatibility member. Smart
constructors, location extraction, depth limits, bottom-up map/fold/walk,
location-insensitive equality, and structural validation preserve the new
nodes. Pretty printing emits `with k` only when explicit metadata is present,
keeps value cases compact, maps operator tags back to source spellings, and
round-trips nested handlers, patterns, multiline bodies, comments, ordinary
call lookalikes, and resume expressions in larger expressions.

## Section 2.3: Semantic Normalization And Early Validation

**Description:** Normalize both handler forms into a common semantic shape and
reject structurally invalid control usage before type inference.

- [x] **Section 2.3 Complete**

### Task 2.3.1: Normalize Value And Control Handlers

**Description:** Give every normalized operation case explicit resumption
metadata without changing existing value-handler evaluation order.

- [x] **Task 2.3.1 Complete**

#### Subtask 2.3.1.1: Generate Tail Auto-Resume

**Description:** Introduce a collision-free synthetic binder and tail
`resume_expr`, preserve the original case origin, and ensure the operation
result expression is evaluated exactly once.

- [x] **Subtask 2.3.1.1 Complete**

#### Subtask 2.3.1.2: Preserve Explicit Control Cases

**Description:** Carry the user's binder and body unchanged and mark the case
as requiring explicit control so no later pass inserts an implicit resume.

- [x] **Subtask 2.3.1.2 Complete**

### Task 2.3.2: Add Structural Control Diagnostics

**Description:** Reject resume syntax that is invalid independently of its
eventual type and establish fail-closed dispositions for later phases.

- [x] **Task 2.3.2 Complete**

#### Subtask 2.3.2.1: Validate Binder Scope And Shadowing

**Description:** Apply ordinary lexical rules, reject duplicate pattern/binder
names where ambiguous, and preserve deliberate nested shadowing with clear
locations.

- [x] **Subtask 2.3.2.1 Complete**

#### Subtask 2.3.2.2: Reject Unsupported Backend Leakage

**Description:** Mark normalized resumption nodes unsupported at the current
backend boundary until later phases supply typing and CPS dispositions; never
lower them as ordinary calls or marker closures.

- [x] **Subtask 2.3.2.2 Complete**

**Implementation evidence:** Semantic analysis now gives every operation case
the canonical six-element form. Value cases receive a collision-free
`__catena_resumption_N` binder and one synthetic tail `resume_expr` whose
origin points back to the source case; explicit cases retain their binder and
body. Normalization is recursive and idempotent, including handler bodies,
resume operands, perform arguments, record bases, and desugared lets.

The early validator enforces active lexical binders, deliberate nested
shadowing, and distinct operation-pattern and resumption names. Type and
backend entry points reject explicit or malformed normalized resumptions with
`missing_resumption_lowering`. To preserve the already promoted value-handler
behavior, legacy consumers receive a compatibility view only when the
synthetic binder, origin, resume target, and tail shape exactly match compiler
output. The normalized AST remains authoritative in semantic results and
compilation units, and direct backend leakage still fails closed rather than
becoming an ordinary call or marker closure.

## Section 2.4: Phase 2 Integration Tests

**Description:** Prove that the complete source-to-normalized-AST path
understands `with` and `resume`, preserves existing handlers, and still fails
closed before unsupported backend execution.

- [x] **Section 2.4 Complete**

### Task 2.4.1: Exercise Source And Normalization Paths

**Description:** Run lexer, parser, AST utility, pretty-printer, semantic, and
diagnostic paths on representative control-handler programs.

- [x] **Task 2.4.1 Complete**

#### Subtask 2.4.1.1: Test Positive Syntax And Round Trips

**Description:** Cover nullary and parameterized operations, explicit
resumption binders, nested resume expressions, value-handler compatibility,
and parse/normalize/print/parse stability.

- [x] **Subtask 2.4.1.1 Complete**

#### Subtask 2.4.1.2: Test Negative Syntax And Leakage

**Description:** Cover malformed binders, invalid resume arity, keyword
boundaries, invalid scope, parser recovery, and explicit rejection at typed
and backend boundaries that are not implemented yet.

- [x] **Subtask 2.4.1.2 Complete**

### Task 2.4.2: Run Phase Completion Gates

**Description:** Validate generated frontend artifacts, parser-conflict
accounting, backward compatibility, and repository health before typing work.

- [x] **Task 2.4.2 Complete**

#### Subtask 2.4.2.1: Audit Parser And Compatibility Deltas

**Description:** Publish the conflict count, prove any new conflict is
understood or removed, and run the maintained lexer, parser, semantic, AST,
stdlib-source, and handler regression suites.

- [x] **Subtask 2.4.2.1 Complete**

#### Subtask 2.4.2.2: Run Repository Gates

**Description:** Run Phase 2 integration tests, `make check-specs`, and the
complete active EUnit suite and record the exact phase-ending evidence.

- [x] **Subtask 2.4.2.2 Complete**

**Implementation evidence:** The dedicated
`catena_delimited_resumption_phase2_tests` integration contract covers
nullary and parameterized control cases, nested handlers, explicit and
automatic normalization, parse/print/parse and normalization stability,
lexical shadowing, keyword boundaries, malformed binders, resume arity,
panic-mode diagnostic preservation, invalid scope, typed/backend leakage, and
loaded-BEAM compatibility for existing value handlers.

The parser-conflict audit removed the explicit nullary value/control
productions added during Section 2.1 because the maintained
`pattern_list_comma` production already accepts an empty list. The final count
is 38 shift/reduce and 0 reduce/reduce conflicts. The one-conflict delta from
the Phase 1 baseline is the expected application-grammar choice on lookahead
`resume`: as with every other primary-expression starter, Yecc shifts so
`f resume(k, value)` remains juxtaposition application. Focused suites,
`make check-specs`, and the complete active EUnit suite pass; the phase-ending
suite contains 5,128 passing tests with no failures or skips.
