# Core Erlang: Complete Language Specification Guide

Core Erlang is a **strict functional intermediate representation** that serves as the critical bridge between Erlang source code and BEAM bytecode in the Erlang/OTP compiler pipeline. Developed by Uppsala University's HiPE project in collaboration with Ericsson, Core Erlang simplifies Erlang's complex surface syntax into approximately 15 orthogonal constructs while maintaining complete expressive power. This makes it the preferred target for compiler optimizations, static analysis tools like Dialyzer, and alternative language implementations for the BEAM virtual machine. Unlike BEAM bytecode, Core Erlang remains human-readable and writable, enabling program transformation, analysis, and debugging at an intermediate level. The official specification (version 1.0.3, released November 2004) defines Core Erlang as functional, strict, dynamically typed, supporting pattern matching, higher-order functions, and concurrent actor-based computation.

Core Erlang matters because it represents the canonical, normalized form of Erlang programs before final compilation. Every Erlang program passes through Core Erlang during compilation, where multiple optimization passes operate on this simplified representation. Understanding Core Erlang reveals how Erlang's syntactic sugar desugars, how pattern matching actually works, and how the compiler transforms high-level constructs into efficient BEAM code. For language implementers, Core Erlang provides a stable, well-documented target that's far simpler than targeting BEAM bytecode directly.

## Complete syntax and grammar specification

Core Erlang's grammar follows a strict hierarchical structure starting with module definitions and descending through function definitions, expressions, patterns, and literals. The language uses **15 primary keywords** and enforces explicit, fully-qualified syntax for all operations.

### Reserved keywords and their exact usage

The complete set of Core Erlang keywords includes: **module** (module declaration), **attributes** (module attributes section), **fun** (function/lambda expressions), **let** (variable binding), **in** (used with let/letrec), **case** (pattern matching expressions), **of** (introduces case clauses), **when** (guard expressions), **end** (terminates case/receive), **receive** (message handling), **after** (timeout clauses), **catch** (exception handling), **try** (try-catch blocks), **primop** (primitive operations), **call** (external function calls), **apply** (local function applications), and **letrec** (recursive let bindings).

Core Erlang eliminated many Erlang keywords. The language does NOT use: `if`, `cond`, `andalso`, `orelse`, `begin`, or any operator keywords. These constructs are compiled away during translation.

**Operator delimiters and special syntax** include: `<` and `>` (value list delimiters), `{` and `}` (tuple constructors), `[` and `]` (list constructors), `|` (list cons separator), `->` (clause separator), `/` (function arity separator as in 'function'/2), `=` (binding operator), single quotes `'` (mandatory atom quoting), and `-|` (annotation attachment operator).

### Formal grammar in BNF notation

The Core Erlang grammar specification defines modules as the top-level construct:

```
module ::= 'module' atom '[' function_name_list ']'
           'attributes' '[' attribute_list ']'
           function_definition_list 'end'

function_name ::= atom '/' integer

function_definition ::= function_name '=' fun

attribute ::= atom '=' literal
```

Expression grammar encompasses all computational constructs:

```
expression ::= variable
             | literal
             | cons | tuple | binary | map
             | 'let' '<' variable_list '>' '=' expression 'in' expression
             | 'letrec' function_definition_list 'in' expression
             | 'case' expression 'of' clause_list 'end'
             | 'receive' clause_list timeout 'end'
             | 'apply' expression '(' expression_list ')'
             | 'call' expression ':' expression '(' expression_list ')'
             | 'primop' atom '(' expression_list ')'
             | 'try' expression 'of' variable_list '->' expression
                      'catch' variable_list '->' expression
             | 'catch' expression

clause ::= '<' pattern_list '>' 'when' guard '->' expression

pattern ::= variable | literal | cons_pattern | tuple_pattern
          | binary_pattern | map_pattern | alias_pattern

guard ::= expression  % must evaluate to 'true' or 'false'
```

### Value lists and their significance

Value lists, denoted by angle brackets `< >`, represent **ordered sequences of values** that differ fundamentally from tuples. Empty value lists `<>` contain no values, single-element lists `<Expr>` wrap one expression, and multi-element lists `<Expr1, Expr2, Expr3>` contain multiple expressions. Value lists appear in case patterns, function return values, and let bindings. Crucially, **value lists are not tuples**: `<1,2>` differs semantically from `{1,2}`. Value lists represent multiple simultaneous values (like multiple return values), while tuples are single composite data structures.

## Core Erlang language constructs

### Module definitions and structure

Every Core Erlang program begins with a module definition that explicitly declares its name, exports, and attributes:

```erlang
module 'example_module' ['public_function'/2, 'main'/0]
    attributes [
        'author' = "Developer Name",
        'vsn' = [234857293847]
    ]
    'public_function'/2 = fun (_@c1, _@c0) -> ...
    'main'/0 = fun () -> ...
end
```

Module names must be quoted atoms. The export list contains function names with **explicit arity** (never omitted). Attributes are key-value pairs where keys are atoms and values are any Erlang terms. Functions are defined as top-level bindings using the pattern `'function_name'/arity = fun (params) -> body`.

### Function definitions and single-clause requirement

Core Erlang functions differ critically from Erlang functions: **a fun can have only ONE clause**. Multi-clause Erlang functions transform into single-clause funs containing case expressions. Function definitions follow this structure:

```erlang
'factorial'/1 = 
    fun (_@c0) -> 
        case _@c0 of
            <0> when 'true' -> 1
            <N> when 'true' ->
                let <_@c1> = call 'erlang':'-'(N, 1)
                in let <_@c2> = apply 'factorial'/1(_@c1)
                in call 'erlang':'*'(N, _@c2)
        end
```

All parameters must be variables (never patterns). Pattern matching occurs exclusively in case statements within the function body. The compiler generates variable names like `_@c0`, `_@c1` for parameters and intermediate values.

### Let expressions for explicit scoping

Let expressions provide **explicit variable binding** with clear scoping rules:

```erlang
let <Var1, Var2, Var3> = Expression
in BodyExpression
```

Variables bound in let expressions exist only within the scope following `in`. Multiple variables can be bound simultaneously from expressions returning value lists. Let expressions make all binding explicit—unlike Erlang where variables can be introduced implicitly in many contexts.

### Letrec for recursive bindings

The `letrec` construct creates **recursive function bindings**, essential for defining local helper functions and implementing language constructs like list comprehensions:

```erlang
'function'/1 = 
    fun (_@c0) ->
        letrec
            'helper'/0 = 
                fun () -> 
                    ... recursive calls to 'helper'/0 ...
        in
            apply 'helper'/0()
```

Starting in OTP 23, `letrec` implements receive expressions. The compiler generates a recursive loop function that peeks at messages, pattern matches them, and either processes matches or continues looping. List comprehensions also desugar to letrec with recursive accumulator functions.

### Case expressions as the sole pattern matching construct

**All pattern matching in Core Erlang occurs in case expressions**. This is the most fundamental difference from Erlang, which allows patterns in function heads, let bindings, receive expressions, and more.

```erlang
case <Expr1, Expr2, Expr3> of
    <Pattern1, Pattern2, Pattern3> when Guard1 -> Body1
    <PatternA, PatternB, PatternC> when Guard2 -> Body2
end
```

Every clause **requires a guard** (use `'true'` for unconditional clauses). Case expressions **must not fall through**—they must include a catch-all clause that matches all remaining cases, typically raising a `match_fail` primop for unmatched patterns.

### Apply versus call for function invocation

Core Erlang distinguishes between two types of function invocation:

**Apply** invokes functions within the same module:
```erlang
apply 'local_function'/2(Arg1, Arg2)
```

**Call** invokes external functions with full module qualification:
```erlang
call 'module':'function'(Arg1, Arg2)
```

All built-in functions (BIFs) require call with the `'erlang'` module name: `call 'erlang':'+'(X, Y)`. This explicit distinction enables compiler optimizations and makes module boundaries clear.

### Receive expressions and message handling

Modern Core Erlang (OTP 23+) implements receive expressions using **letrec and primitive operations**:

```erlang
letrec
    'recv$^0'/0 =
        fun () ->
            let <PeekSucceeded, Message> = primop 'recv_peek_message'()
            in case PeekSucceeded of
                <'true'> when 'true' ->
                    case Message of
                        <{tag, Value}> when 'true' ->
                            do primop 'remove_message'()
                               Value
                        <_Other> when 'true' ->
                            do primop 'recv_next'()
                               apply 'recv$^0'/0()
                    end
                <'false'> when 'true' ->
                    % timeout handling
            end
in apply 'recv$^0'/0()
```

Three primitive operations manage message queues: **recv_peek_message** examines the next message without removing it, **remove_message** removes the current message when a pattern matches, and **recv_next** advances to the next message when the current message doesn't match. Timeout clauses evaluate timeout expressions in outer let bindings if they're not safe to evaluate repeatedly.

### Try-catch for exception handling

Core Erlang provides structured exception handling through try expressions:

```erlang
try RiskyExpression
of <SuccessVar1, SuccessVar2> -> SuccessBody
catch <ExceptionClass, ExceptionReason, StackTrace> -> ExceptionHandler
```

The `of` clause handles successful evaluation, binding result values to variables. The `catch` clause handles exceptions, binding three variables: exception class (error, throw, exit), the reason term, and the stack trace. Simple catch expressions (`catch Body`) exist but are deprecated—they can be rewritten as try expressions and will eventually be removed.

### Primitive operations

Primitive operations (primops) provide low-level functionality not expressible in pure Core Erlang:

```erlang
primop 'match_fail'({'function_clause', Value})
primop 'match_fail'({'case_clause', Value})
primop 'match_fail'({'badmatch', Value})
```

The **match_fail** primop raises exceptions for pattern matching failures. Other primops handle binary operations, message queue manipulation, and compiler-internal operations. Primop availability and semantics are implementation-specific.

### Literals and data constructors

**Atoms** must always be quoted with single quotes: `'ok'`, `'true'`, `'false'`, `'undefined'`. This differs from Erlang where simple atoms don't require quotes.

**Numbers** use standard notation: `42` (integer), `3.14159` (float).

**Strings** become lists of integers: `"hello"` compiles to `[104, 101, 108, 108, 111]`.

**Lists** use bracket notation: `[]` (empty list), `[Head | Tail]` (cons cell), `[1, 2, 3]` (list literal).

**Tuples** use curly braces: `{}` (empty tuple), `{Element1, Element2}` (tuple with elements).

**Binaries** use special syntax: `#{<bitstring_elements>}#` where bitstring elements specify size, type, and endianness.

**Maps** use tilde notation: `~{Key1=>Value1, Key2=>Value2}~` for map literals.

### Variables and naming conventions

Variables must begin with uppercase letters or underscores, following standard Erlang conventions. However, **the bare underscore `_` is not valid** in Core Erlang—the compiler generates explicit names like `_@c0`, `_@c1`, `_@c2` for unused values and pattern variables.

The compiler systematically renames variables to ensure uniqueness. Variables cannot repeat in patterns—repeated variables transform into equality guards. For example, `function(Same, Same)` becomes `function(_@c0, _@c1) when call 'erlang':'=:='(_@c0, _@c1)`.

## How Core Erlang differs from regular Erlang

Core Erlang implements **twelve fundamental restrictions** that distinguish it from full Erlang.

**Pattern matching location**: Erlang allows patterns in function heads, case, receive, try, let, and the `=` operator. Core Erlang restricts all pattern matching to case expressions exclusively.

**Function clauses**: Erlang functions can have multiple clauses with patterns in the function head. Core Erlang functions have a single clause—multiple Erlang clauses transform into a single fun containing a case expression.

**Mandatory guards**: Erlang allows clauses without guards. Core Erlang requires every case clause to have a guard, using `'true'` for unconditional clauses.

**Call qualification**: Erlang uses the same syntax for local and external calls. Core Erlang distinguishes them: `apply` for same-module calls, `call 'module':'function'` for external calls and BIFs.

**Operator desugaring**: Erlang has built-in operators like `+`, `-`, `*`, `>`, `==`. Core Erlang treats all operators as BIF calls: `X + Y` becomes `call 'erlang':'+'(X, Y)`.

**Atom quoting**: Erlang requires quotes only for atoms with special characters. Core Erlang requires single quotes around **all atoms**: `ok` becomes `'ok'`.

**Underscore handling**: Erlang uses `_` as a wildcard variable. Core Erlang doesn't support bare underscore—the compiler generates names like `_@c0`.

**Variable repetition**: Erlang allows repeated variables in patterns to express equality. Core Erlang converts these to separate variables with equality guards.

**Scoping rules**: Erlang has implicit variable scoping with variables persisting through function execution. Core Erlang has explicit scoping—let expressions and case expressions don't export variables.

**No fall-through**: Case expressions must be exhaustive with a catch-all clause that raises match_fail for unmatched patterns.

**Value lists**: Core Erlang exposes value lists `<expr1, expr2>` for multiple values, which differ from tuples. Erlang doesn't expose this concept to programmers.

**Expression nesting**: Core Erlang restricts where expressions can nest. Complex expressions must be bound to variables with let before use as arguments or patterns.

## Examples demonstrating each language feature

### Simple factorial showing fundamental constructs

```erlang
% Erlang
factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

% Core Erlang
'factorial'/1 =
    fun (_@c0) ->
        case _@c0 of
            <0> when 'true' ->
                1
            <N> when 'true' ->
                let <_@c1> = call 'erlang':'-'(N, 1)
                in let <_@c2> = apply 'factorial'/1(_@c1)
                in call 'erlang':'*'(N, _@c2)
        end
```

This demonstrates: single-clause fun, case for pattern matching, mandatory guards, operators as BIF calls, let for intermediate bindings, apply for recursion.

### Multiple function definitions in a module

```erlang
module 'arithmetic' ['add'/2, 'multiply'/2, 'main'/0]
    attributes []
    
'add'/2 =
    fun (_@c1, _@c0) ->
        call 'erlang':'+'(_@c1, _@c0)
        
'multiply'/2 =
    fun (_@c1, _@c0) ->
        call 'erlang':'*'(_@c1, _@c0)
        
'main'/0 =
    fun () ->
        let <Sum> = apply 'add'/2(10, 32)
        in apply 'multiply'/2(Sum, 2)
        
end
```

This shows: module structure, export list with explicit arities, multiple top-level functions, variable binding with let, sequential operations.

### Pattern matching with guards

```erlang
'classify'/1 =
    fun (_@c0) ->
        case _@c0 of
            <N> when call 'erlang':'<'(N, 0) ->
                'negative'
            <0> when 'true' ->
                'zero'
            <N> when call 'erlang':'>'(N, 0) ->
                'positive'
            <_@c1> when 'true' ->
                primop 'match_fail'({'function_clause', _@c1})
        end
```

This demonstrates: comparison operators as guards, multiple clauses with different guards, catch-all clause with match_fail, quoted atoms as return values.

### If statements transformed to case expressions

```erlang
% Erlang
compare(A, B) ->
    if
        A > B -> greater;
        A < B -> less;
        true -> equal
    end.

% Core Erlang
'compare'/2 =
    fun (_@c1, _@c0) ->
        case <> of
            <> when call 'erlang':'>'(_@c1, _@c0) ->
                'greater'
            <> when call 'erlang':'<'(_@c1, _@c0) ->
                'less'
            <> when 'true' ->
                'equal'
        end
```

If-expressions desugar to **case with empty value lists** `<>`, with all logic in guard expressions.

### List comprehensions desugared to letrec

```erlang
% Erlang
double_list(L) -> [X*2 || X <- L].

% Core Erlang (conceptual)
'double_list'/1 =
    fun (_@c0) ->
        letrec
            'lc$^0'/1 =
                fun (_@c1) ->
                    case _@c1 of
                        <[_@c2|_@c3]> when 'true' ->
                            let <_@c4> = call 'erlang':'*'(_@c2, 2)
                            in let <_@c5> = apply 'lc$^0'/1(_@c3)
                            in [_@c4|_@c5]
                        <[]> when 'true' ->
                            []
                    end
        in apply 'lc$^0'/1(_@c0)
```

List comprehensions become recursive functions defined with letrec that pattern match on list structure and build result lists incrementally.

### Record updates showing tuple operations

```erlang
% Erlang
-record(person, {name, age, city}).
update_age(Person, NewAge) -> Person#person{age=NewAge}.

% Core Erlang
'update_age'/2 =
    fun (_@c1, _@c0) ->
        case <_@c1, _@c0> of
            <{'person', Name, _OldAge, City}, NewAge> when 'true' ->
                call 'erlang':'setelement'(3, _@c1, NewAge)
            <_@c2, _@c3> when 'true' ->
                call 'erlang':'error'({'badrecord', 'person'})
        end
```

Records become tuples with the record name as the first element. Updates use `setelement/3` BIF calls.

## Core Erlang abstract syntax and program structure

### Abstract syntax tree representation

Core Erlang uses record-based AST nodes defined in the `cerl` module. Primary node types include:

**Module structure**: `#c_module{}` represents complete modules with name, exports, attributes, and function definitions.

**Functions and variables**: `#c_fun{}` for function expressions, `#c_var{}` for variables, `#c_literal{}` for constant values.

**Data constructors**: `#c_cons{}` for list cons cells, `#c_tuple{}` for tuples, `#c_binary{}` for binaries, `#c_map{}` and `#c_map_pair{}` for maps.

**Control flow**: `#c_let{}` for let expressions, `#c_letrec{}` for recursive let, `#c_case{}` for case expressions, `#c_clause{}` for pattern matching clauses, `#c_receive{}` for receive expressions.

**Function calls**: `#c_apply{}` for local applications, `#c_call{}` for external calls, `#c_primop{}` for primitive operations.

**Exception handling**: `#c_try{}` for try-catch, `#c_catch{}` for catch expressions.

**Other constructs**: `#c_alias{}` for pattern aliases, `#c_values{}` for value lists, `#c_seq{}` for sequential expressions, `#c_bitstr{}` for bitstring segments.

### Working with the cerl module

The `cerl` module provides a complete API for constructing, querying, and manipulating Core Erlang ASTs:

**Constructor functions** (prefixed with `c_`): `c_module/3,4` creates modules, `c_fun/2` creates functions, `c_var/1` creates variables, `c_let/3` creates let expressions, `c_case/2` creates case expressions, `c_clause/3` creates clauses, `c_apply/2` creates applications, `c_call/3` creates calls, `c_primop/2` creates primops.

**Accessor functions** extract components: `module_name/1`, `module_exports/1`, `module_defs/1` for modules; `fun_vars/1`, `fun_body/1` for functions; `let_vars/1`, `let_arg/1`, `let_body/1` for let expressions; `case_arg/1`, `case_clauses/1` for cases; `clause_pats/1`, `clause_guard/1`, `clause_body/1` for clauses.

**Type checking predicates** (prefixed with `is_c_`): `is_c_var/1`, `is_c_fun/1`, `is_c_call/1`, etc.

**Utility functions**: `concrete/1` converts literal AST nodes to Erlang terms, `abstract/1` converts Erlang terms to literal AST nodes, `type/1` returns the type tag of any node, `get_ann/1` retrieves annotations.

### Tree traversal with cerl_trees

The `cerl_trees` module provides utilities for traversing and transforming syntax trees:

```erlang
% Map a function over all nodes
TransformedTree = cerl_trees:map(TransformFun, Tree),

% Count nodes in tree
Size = cerl_trees:size(Tree),

% Find free variables
FreeVars = cerl_trees:free_variables(Tree),

% Combined map and fold
{Result, Accumulator} = cerl_trees:mapfold(Fun, Acc, Tree)
```

These functions enable systematic AST transformation for compiler passes and program analysis.

## Type system and annotations

Core Erlang is **dynamically typed** with no static type system or compile-time type checking. However, it uses **annotations** to attach metadata to AST nodes.

### Annotation syntax and usage

Annotations attach to expressions using the `-|` operator:

```erlang
Expression -| [annotation1, annotation2, annotation3]
```

**Line number annotations** track source file locations for error messages, rendered as comments in pretty-printed output.

**compiler_generated annotation** marks code inserted by the compiler that shouldn't generate warnings about unused variables or unreachable code.

**function_name annotation** provides function name and arity for exception messages: `{'function_name', {'factorial', 1}}`.

**letrec_goto annotation** marks letrec constructs used for goto-like control flow to avoid code duplication.

**dialyzer_ignore annotation** suppresses specific Dialyzer warnings for intentionally unusual code.

### Relationship with Erlang type specifications

Erlang's `-spec` and `-type` declarations exist only in source code. They're **not present** in Core Erlang—the compiler strips them before Core Erlang generation. Dialyzer analyzes Core Erlang using **success typings**, a form of type inference that doesn't rely on programmer annotations. Success typings work bottom-up, inferring the most general types that allow functions to succeed without runtime errors.

## Compiling to Core Erlang and working with .core files

### Generating Core Erlang from Erlang source

**Command line compilation**:
```bash
erlc +to_core module.erl
```
This creates `module.core` in the current directory.

**With timing information**:
```bash
erlc +time +to_core module.erl
```
This shows each compiler pass with timing and intermediate size:
```
Compiling "module"
parse_module     : 0.000 s   10.8 kB
transform_module : 0.000 s   10.8 kB
lint_module      : 0.003 s   10.8 kB
expand_records   : 0.000 s   10.8 kB
core             : 0.000 s   89.9 kB
sys_core_fold    : 0.000 s   58.6 kB
```

**From Erlang shell**:
```erlang
c('module.erl', [to_core]).
```

**Programmatically obtaining Core Erlang forms**:
```erlang
{ok, _, CoreForms} = compile:file("module.erl", [to_core, binary, no_copt])
```

The `binary` option returns forms in memory without creating a file. The `no_copt` option disables Core Erlang optimizations for clearer, more readable output.

### Compiling Core Erlang to BEAM

**Command line**:
```bash
erlc module.core
```
This produces `module.beam`.

**From Erlang shell**:
```erlang
c('module.core', [from_core]).
```

**Programmatically**:
```erlang
compile:file("module.core", [from_core])
```

### The compilation pipeline

Core Erlang appears in the middle of a multi-stage compilation process:

```
Erlang Source (.erl)
    ↓ parse_module       (parsing)
    ↓ transform_module   (parse transforms)
    ↓ lint_module        (syntax checking)
    ↓ expand_records     (record expansion)
    ↓ core               (Core Erlang generation)
    ↓ sys_core_fold      (optimizations)
    ↓ sys_core_alias     (alias optimization)
    ↓ core_transforms    (custom transformations)
    ↓ sys_core_bsm       (binary matching optimization)
    ↓ sys_core_dsetel    (tuple update optimization)
    ↓ v3_kernel          (Kernel Erlang)
    ↓ ... (further passes)
    ↓ BEAM Bytecode (.beam)
```

**sys_core_fold** performs constant folding, dead code elimination, and clause reduction. **sys_core_alias** (OTP 21+) avoids rebuilding already-matched terms. **sys_core_bsm** optimizes binary matching patterns. **sys_core_dsetel** optimizes multiple `setelement/3` calls into destructive updates where safe.

### Creating custom Core Erlang transformations

Core transforms enable custom compiler plugins that operate on Core Erlang:

```erlang
-module(my_transform).
-export([core_transform/2]).

core_transform(Core, Options) ->
    Module = cerl:concrete(cerl:module_name(Core)),
    io:format("Transforming module: ~p~n", [Module]),
    % Manipulate Core using cerl and cerl_trees
    TransformedCore = cerl_trees:map(fun transform_node/1, Core),
    TransformedCore.

transform_node(Node) ->
    case cerl:type(Node) of
        call -> 
            % Transform calls
            modify_call(Node);
        _ -> 
            Node
    end.
```

**Using the transform**:
```bash
erlc -pa . '+{core_transform,my_transform}' module.erl
```

## Official documentation and academic resources

### Primary specification document

The **Core Erlang 1.0.3 Language Specification** (November 2004) is the definitive reference. Authors: Richard Carlsson, Björn Gustavsson, Erik Johansson, Thomas Lindgren, Sven-Olof Nyström, Mikael Pettersson, and Robert Virding. Available at: https://www.it.uu.se/research/group/hipe/cerl/doc/core_erlang-1.0.3.pdf

This 27-page specification provides formal grammar, operational semantics, and design rationale. It defines Core Erlang as functional, strict, dynamically typed, with pattern matching, higher-order functions, and concurrency. The document includes complete BNF grammar and examples of translation from Erlang to Core Erlang.

### Official Erlang/OTP documentation

**compile module documentation** (https://www.erlang.org/doc/apps/compiler/compile.html): Documents `to_core`, `from_core`, `binary`, and `no_copt` compiler options. Important note: "The format of core files is not documented and can change between releases."

**cerl module documentation** (https://www.erlang.org/doc/apps/compiler/cerl.html): Complete API reference for constructing and manipulating Core Erlang ASTs. Note: "This module is an internal part of the compiler. Its API is not guaranteed to remain compatible between releases."

**cerl_trees module** (https://www.erlang.org/doc/apps/compiler/cerl_trees.html): Tree traversal utilities including fold, map, mapfold operations.

**cerl_clauses module** (https://www.erlang.org/doc/apps/compiler/cerl_clauses.html): Clause manipulation utilities for pattern matching and guard evaluation.

### Key academic papers

**"An Introduction to Core Erlang"** by Richard Carlsson (Erlang Workshop 2001, PLI Florence): The foundational paper introducing Core Erlang as the official intermediate language, describing the HiPE collaboration and translation examples.

**"Practical Type Inference Based on Success Typings"** by Tobias Lindahl and Konstantinos Sagonas (PPDP 2006): Introduces success typings operating on Core Erlang, foundation for Dialyzer.

**"Machine-Checked Natural Semantics for Core Erlang"** by Péter Bereczky, Dániel Horpácsi, and Simon Thompson (Erlang Workshop 2020): Formal semantics verified in Coq, handling exceptions and side effects.

**"A Formalisation of Core Erlang, a Concurrent Actor Language"** by Bereczky, Horpácsi, and Thompson (Acta Cybernetica 2024): Comprehensive Coq formalization modeling concurrent actor-based computation.

**"A Core Erlang Semantics for Declarative Debugging"** by Caballero, Martin-Martin, Riesco, and Tamarit (JLAMP 2019): Defines medium-step calculus for sequential Core Erlang with proof trees.

**"The Development of the HiPE System"** by Johansson, Pettersson, Sagonas, and Lindgren (STTT 2003): Details Core Erlang's role in native code compilation and design decisions.

### Official blog posts and tutorials

**"Core Erlang by Example"** (https://www.erlang.org/blog/core-erlang-by-example/): Official Erlang blog introduction with practical examples and compilation commands.

**"Core Erlang Wrap Up"** (https://www.erlang.org/blog/core-erlang-wrapup/): Covers AST representation and compiler passes in detail.

**"A Gentle Introduction to Core Erlang"** by Sasha Fonseca (https://baha.github.io/intro-core-erlang-1/): Two-part community tutorial with clear explanations and examples.

**"The Core of Erlang"** by Kofi Gumbs (https://8thlight.com/insights/the-core-of-erlang): Practical perspective on implementing compilers targeting Core Erlang.

### Research institutions and key contributors

**Uppsala University, Sweden** developed Core Erlang through the HiPE (High Performance Erlang) project. Key researchers: Richard Carlsson (co-designer, cerl module author), Konstantinos Sagonas (HiPE team leader, Dialyzer), Mikael Pettersson (backend development), Tobias Lindahl (success typings), Thomas Lindgren (compiler development).

**Ericsson** collaborated on integrating Core Erlang into OTP. Contributors: Björn Gustavsson (OTP compiler team), Robert Virding (original Erlang designer).

**University of Kent, UK** conducted formal verification work. Simon Thompson leads research on machine-checked Core Erlang semantics.

**Eötvös Loránd University, Hungary** developed Coq formalizations. Dániel Horpácsi and Péter Bereczky created verified Core Erlang semantics.

## Conclusion and practical applications

Core Erlang succeeds as an intermediate language by ruthlessly simplifying Erlang's syntax while preserving complete expressiveness. Its **15 keywords and explicit semantics** make program analysis tractable for tools like Dialyzer, which infers types from Core Erlang representations. Compiler optimization passes operate almost entirely on Core Erlang, where normalized control flow and explicit function calls enable transformations difficult or impossible on the Erlang AST. Alternative language implementations (Elixir, LFE, Alpaca) can target Core Erlang instead of BEAM bytecode, gaining access to all OTP compiler optimizations.

For language implementers, Core Erlang provides a **stable compilation target** with human-readable semantics and well-documented grammar. Unlike BEAM bytecode, which changes frequently and has complex semantics, Core Erlang has remained largely stable since 2004. The specification clearly defines how to compile high-level constructs (pattern matching, guards, receive) into primitive operations. This makes Core Erlang ideal for research languages, DSLs for the BEAM, and educational projects.

Understanding Core Erlang reveals how Erlang really works beneath its syntactic sugar. Pattern matching in function heads is just case expressions. Guards are BIF calls that must return booleans. Records are tuples with atoms as tags. List comprehensions are recursive functions. This knowledge helps debug complex issues, optimize performance-critical code, and understand compiler error messages that reference Core Erlang constructs. Core Erlang represents the essence of Erlang—functional, explicit, and elegantly simple.
