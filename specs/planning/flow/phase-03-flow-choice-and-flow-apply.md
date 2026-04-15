# Flow Phase 3: FlowChoice and FlowApply

**Duration:** 4 days

**Description:** This phase implements FlowChoice (ArrowChoice) for sum type branching and FlowApply (ArrowApply) for dynamic Flow application.

---

## Section 3.1: FlowChoice Trait

**Description:** Define FlowChoice trait for branching on sum types.

### Task 3.1.1: FlowChoice Type Definition
**Description:** Define FlowChoice trait in `lib/catena/stdlib/prelude.cat`.

**Subtasks:**
- Add FlowChoice trait definition extending Flow
- Define `left : arr a b -> arr (Either a c) (Either b c)` method
- Define `right : arr a b -> arr (Either c a) (Either c b)` method
- Add FlowChoice trait documentation
- Export FlowChoice trait from Prelude

### Task 3.1.2: FlowChoice Derived Operations
**Description:** Implement derived FlowChoice operations.

**Subtasks:**
- Define `+++` (choice) operator: `choice : arr a b -> arr c d -> arr (Either a c) (Either b d)`
- Define `|||` (merge) operator: `merge : arr a c -> arr b c -> arr (Either a b) c`
- Implement `fanin` helper for merge patterns
- Add derived operation documentation

### Task 3.1.3: FlowChoice Laws
**Description:** Add FlowChoice law definitions to `lib/catena/stdlib/laws.cat`.

**Subtasks:**
- Add `flowChoiceLeftLaw`: `compose (left f) (left g) === left (compose f g)`
- Add `flowChoiceRightLaw`: `compose (right f) (right g) === right (compose f g)`
- Add `flowChoiceExchangeLaw`: exchange law for choice operations
- Document FlowChoice laws with examples
- Add FlowChoice law verification helpers

### Task 3.1.4: FlowChoice Instances
**Description:** Implement FlowChoice instances for core types.

**Subtasks:**
- Implement FlowChoice for functions
- Implement FlowChoice for Maybe
- Implement FlowChoice for Either
- Implement FlowChoice for Result
- Verify FlowChoice laws for all instances

---

## Section 3.2: FlowApply Trait

**Description:** Define FlowApply trait for dynamic Flow application (equivalent in power to Monad).

### Task 3.2.1: FlowApply Type Definition
**Description:** Define FlowApply trait in `lib/catena/stdlib/prelude.cat`.

**Subtasks:**
- Add FlowApply trait definition extending Flow
- Define `app : arr (arr a b, a) b` method
- Add FlowApply trait documentation
- Document relationship to Pipeline (Monad)
- Export FlowApply trait from Prelude

### Task 3.2.2: FlowApply Laws
**Description:** Add FlowApply law definitions to `lib/catena/stdlib/laws.cat`.

**Subtasks:**
- Add `flowApplyAppLaw`: application law
- Add `flowApplyCompositionLaw`: composition with app
- Document FlowApply laws
- Add FlowApply law verification helpers

### Task 3.2.3: FlowApply Instances
**Description:** Implement FlowApply instances for applicable types.

**Subtasks:**
- Implement FlowApply for functions
- Implement FlowApply for State (when State Flow exists)
- Verify FlowApply laws for all instances
- Document FlowApply use cases

---

## Section 3.3: Parser Combinator Examples

**Description:** Build parser combinators using Flow to demonstrate practical usage.

### Task 3.3.1: Basic Parser Flow
**Description:** Implement basic parsers using Flow.

**Subtasks:**
- Define Parser type as Flow from String
- Implement `char` parser using Flow
- Implement `string` parser using Flow
- Implement `many` parser using FlowChoice
- Implement `optional` parser using FlowChoice

### Task 3.3.2: Parser Examples
**Description:** Create example parsers using Flow.

**Subtasks:**
- Write JSON parser example using Flow
- Write CSV parser example using Flow
- Write expression parser example using Flow
- Document parser patterns with Flow
- Benchmark Flow-based parsers

---

## Section 3.4: Integration Tests

**Description:** Integration tests for FlowChoice and FlowApply.

### Task 3.4.1: FlowChoice Tests
**Description:** Test FlowChoice functionality comprehensively.

**Subtasks:**
- Test FlowChoice `left` operation
- Test FlowChoice `right` operation
- Test FlowChoice `+++` (choice) operator
- Test FlowChoice `|||` (merge) operator
- Test FlowChoice laws for all instances
- Test FlowChoice with Either and Result types

### Task 3.4.2: FlowApply Tests
**Description:** Test FlowApply functionality.

**Subtasks:**
- Test FlowApply `app` operation
- Test FlowApply laws for all instances
- Test FlowApply relationship to Pipeline
- Test FlowApply dynamic computation
- Test FlowApply error cases

### Task 3.4.3: Parser Tests
**Description:** Test Flow-based parser combinators.

**Subtasks:**
- Test basic parsers parse correctly
- Test parser composition with Flow
- Test parser error handling
- Test parser backtracking with FlowChoice
- Test complex parsers (JSON, CSV)

---

## Deliverables

### New Modules
- `lib/catena/stdlib/prelude.cat` — Updated with FlowChoice and FlowApply traits
- `lib/catena/stdlib/parser.cat` — Parser combinators using Flow

### Modified Modules
- `lib/catena/stdlib/laws.cat` — Updated with FlowChoice and FlowApply laws

### Test Modules
- `test/compiler/stdlib/catena_flow_choice_tests.erl`
- `test/compiler/stdlib/catena_flow_apply_tests.erl`
- `test/compiler/stdlib/catena_parser_tests.erl`

### Documentation
- FlowChoice and FlowApply trait documentation
- Parser combinator examples
