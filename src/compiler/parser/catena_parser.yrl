%% Catena Parser - Yecc Grammar Definition
%% Phase 1, Task 1.1.2: Grammar Implementation
%%
%% This parser transforms Catena token streams into Abstract Syntax Trees (ASTs).
%% Built using yecc (Erlang's LALR parser generator).

%%============================================================================
%% Header
%%============================================================================

Header
"%% This file is generated from catena_parser.yrl. Do not edit directly."
.

%%============================================================================
%% Nonterminals
%%============================================================================

Nonterminals
  catena_module
  module_header module_decl export_decl export_list export_item
  declarations declaration
  type_decl transform_decl effect_decl trait_decl instance_decl
  type_params type_params_nonempty constructors constructor constructor_fields
  effect_operations effect_operation
  trait_extends trait_extends_list trait_constraint
  maybe_trait_extends
  trait_members trait_member
  instance_type_args
  instance_constraints instance_methods instance_method
  transform_signature transform_clauses transform_clause
  match_clauses match_clause
  pattern_list pattern_list_nonempty pattern pattern_primary pattern_list_comma tuple_pattern_list
  guards guard
  expr expr_primary expr_app expr_list expr_list_opt tuple_expr_list
  record_fields record_field
  record_pattern_fields record_pattern_field
  literal
  perform_expr try_with_expr
  handler_clauses handler_clause operation_cases operation_case
  effect_list effect_list_nonempty
  type_expr type_expr_primary type_expr_app type_expr_primary_list
  type_list type_expr_list type_record_fields type_record_field
  type_constraints
  do_expr do_statements do_statement
  .

%%============================================================================
%% Terminals
%%============================================================================

Terminals
  %% Core Keywords (14 keywords requiring compiler support)
  type transform match 'let' 'in' 'end'
  trait instance where then
  effect operation perform handle
  actor process module 'do'

  %% Syntax Keywords (supplementary keywords)
  'case' 'of' 'when' as forall fn

  %% Trait system keywords
  extend constrain

  %% Module Keywords
  import export exports qualified private

  %% Operators
  pipe_right arrow double_arrow
  setoid_eq setoid_neq eq neq lte gte lt gt
  'or' 'and' cons left_arrow range
  colon equals pipe
  plus minus star slash dot plus_plus

  %% Delimiters
  lbrace rbrace lbracket rbracket lparen rparen
  comma semicolon underscore

  %% Literals and identifiers
  integer float string
  lower_ident upper_ident

  %% Error token for error recovery
  error
  .

%%============================================================================
%% Rootsymbol
%%============================================================================

Rootsymbol catena_module.

%%============================================================================
%% Precedence and Associativity
%%============================================================================

Right    100 arrow.           %% Type-level function arrow (right-assoc)

Right    160 pipe_right.      %% |> pipe operator

%% Logical operators (low precedence)
Right    200 'or'.            %% ||
Right    250 'and'.           %% &&

%% Equality operators (non-associative)
Nonassoc 300 eq neq setoid_eq setoid_neq.  %% == /= === !==
Nonassoc 310 lt gt lte gte.   %% < > <= >=

%% Arithmetic operators
Right    350 plus_plus.       %% ++ list concatenation
Left     400 plus minus.      %% + -
Left     500 star slash.      %% * /

%% Record field access (highest precedence)
Left     600 dot.             %% .

%%============================================================================
%% Parser Conflicts Documentation
%%============================================================================
%%
%% This grammar has 17 shift/reduce conflicts, all of which are resolved
%% correctly by yecc's default behavior (shift on conflict).
%%
%% CONFLICT CATEGORIES:
%%
%% 1. FUNCTION APPLICATION (Juxtaposition)
%%    Location: expr_app productions
%%    Cause: When parsing "f x y", the parser must decide whether to:
%%           - Shift: Continue building application (f x) y
%%           - Reduce: Complete current application f x
%%    Resolution: SHIFT (yields left-associative application) ✓
%%    Impact: Most of the 17 conflicts (~12-14 conflicts)
%%    Example: "map f xs" parses as (map f) xs, not map (f xs)
%%
%% 2. FLOW DECLARATIONS (Signature vs. Implementation)
%%    Location: transform_decl productions
%%    Cause: After "transform name : Type", parser must decide:
%%           - Shift: Parse implementation body
%%           - Reduce: Accept signature-only declaration
%%    Resolution: SHIFT (allows implementation if present) ✓
%%    Impact: ~1-2 conflicts
%%    Example: "transform id : a -> a\n  id x = x" parses correctly
%%
%% 3. TYPE EXPRESSIONS (Parenthesized vs. Tuple Types)
%%    Location: type_expr productions
%%    Cause: "(Int, String)" could be:
%%           - Tuple type (Int, String)
%%           - Parenthesized type list for function args
%%    Resolution: SHIFT (favors tuple interpretation) ✓
%%    Impact: ~1-2 conflicts
%%    Example: "(Int, String)" is tuple type
%%
%% 4. EFFECT ANNOTATIONS (Slash Operator Ambiguity)
%%    Location: type_expr -> type_expr_app slash ...
%%    Cause: "Int / Maybe" could be:
%%           - Division operator: Int / Maybe
%%           - Effect annotation: Int / {Maybe}
%%    Resolution: Context-dependent (slash requires {Effects}) ✓
%%    Impact: ~1 conflict
%%    Note: Effect annotations require braces, so no actual ambiguity
%%    Example: "Int / {IO}" is effect type, "x / y" is division
%%
%% 5. RECORD ACCESS (Dot Operator)
%%    Location: expr -> expr_primary dot lower_ident
%%    Cause: "a.b.c" parsing with left recursion
%%    Resolution: SHIFT (left-associative chaining) ✓
%%    Impact: ~1 conflict
%%    Example: "user.address.street" parses as ((user.address).street)
%%
%% WHY THESE CONFLICTS ARE ACCEPTABLE:
%%
%% - All conflicts resolve via SHIFT, which is the desired behavior
%% - No reduce/reduce conflicts (which would indicate grammar ambiguity)
%% - Each conflict has a clear "correct" resolution
%% - Alternative would require extensive grammar restructuring with
%%   complex precedence rules, reducing readability
%% - These are standard conflicts in ML-family language parsers
%%
%% VERIFICATION:
%%
%% Run: erlc -o src/compiler/parser src/compiler/parser/catena_parser.yrl
%% Expected: "conflicts: 17 shift/reduce, 0 reduce/reduce"
%% Any change in conflict count indicates grammar modification
%%
%% REFERENCES:
%%
%% - Yecc User's Guide: Shift/Reduce Conflicts
%% - "Compiling with Continuations" (Appel) - Chapter on parsing
%% - OCaml parser conflicts: Similar patterns in OCaml's yacc grammar
%%

%%============================================================================
%% Grammar Rules
%%============================================================================

%% Module structure
catena_module -> module_header declarations :
    {module,
        element(2, '$1'),   %% Module name
        element(3, '$1'),   %% Exports
        [],                 %% Imports (future)
        '$2',               %% Declarations
        element(4, '$1')}.  %% Location

catena_module -> declarations :
    {module, undefined, [], [], '$1', {line, 1}}.

%% Module header (module declaration + optional exports)
module_header -> module_decl :
    {module_header, element(2, '$1'), element(3, '$1'), element(4, '$1')}.
module_header -> module_decl export_decl :
    {module_header, element(2, '$1'), '$2', element(4, '$1')}.

%% Module declaration: module Name
module_decl -> module upper_ident :
    {module_decl, extract_atom('$2'), [], extract_location('$1')}.

%% Dotted module name: module Effect.IO
module_decl -> module upper_ident dot upper_ident :
    {module_decl,
        list_to_atom(atom_to_list(extract_atom('$2')) ++ "." ++ atom_to_list(extract_atom('$4'))),
        [],
        extract_location('$1')}.

%% Export declaration
export_decl -> export_list :
    '$1'.

export_list -> export_item :
    ['$1'].
export_list -> export_item export_list :
    ['$1' | '$2'].

%% Export items
export_item -> export trait upper_ident :
    {export_trait, extract_atom('$3')}.
export_item -> export type upper_ident :
    {export_type, extract_atom('$3')}.
export_item -> export transform lower_ident :
    {export_transform, extract_atom('$3')}.
export_item -> export effect upper_ident :
    {export_effect, extract_atom('$3')}.

declarations -> declaration :
    ['$1'].
declarations -> declaration declarations :
    ['$1' | '$2'].

declaration -> type_decl : '$1'.
declaration -> transform_decl : '$1'.
declaration -> effect_decl : '$1'.
declaration -> trait_decl : '$1'.
declaration -> instance_decl : '$1'.

%% Error recovery: skip malformed declaration and continue with next
declaration -> error type :
    make_error_declaration(extract_location('$2'), "Malformed declaration before 'type'", '$1').
declaration -> error transform :
    make_error_declaration(extract_location('$2'), "Malformed declaration before 'transform'", '$1').
declaration -> error effect :
    make_error_declaration(extract_location('$2'), "Malformed declaration before 'effect'", '$1').
declaration -> error trait :
    make_error_declaration(extract_location('$2'), "Malformed declaration before 'trait'", '$1').
declaration -> error instance :
    make_error_declaration(extract_location('$2'), "Malformed declaration before 'instance'", '$1').

%%----------------------------------------------------------------------------
%% Type Declarations (Algebraic Data Types)
%%----------------------------------------------------------------------------

type_decl -> type upper_ident type_params equals constructors :
    {type_decl,
        extract_atom('$2'),
        '$3',
        '$5',
        [],
        extract_location('$1')}.

%% Error recovery for incomplete type declarations
type_decl -> type error equals constructors :
    make_error_declaration(extract_location('$1'), "Invalid type name", '$2').
type_decl -> type upper_ident type_params error :
    make_error_declaration(extract_location('$1'), "Missing '=' or constructors in type declaration", '$4').
type_decl -> type error :
    make_error_declaration(extract_location('$1'), "Incomplete type declaration", '$2').

type_params -> '$empty' :
    [].
type_params -> type_params_nonempty :
    '$1'.

type_params_nonempty -> lower_ident :
    [extract_atom('$1')].
type_params_nonempty -> lower_ident type_params_nonempty :
    [extract_atom('$1') | '$2'].

constructors -> constructor :
    ['$1'].
constructors -> constructor pipe constructors :
    ['$1' | '$3'].

constructor -> upper_ident :
    {constructor,
        extract_atom('$1'),
        [],
        extract_location('$1')}.

constructor -> upper_ident constructor_fields :
    {constructor,
        extract_atom('$1'),
        '$2',
        extract_location('$1')}.

constructor_fields -> type_expr_primary :
    ['$1'].
constructor_fields -> type_expr_primary constructor_fields :
    ['$1' | '$2'].

%%----------------------------------------------------------------------------
%% Effect Declarations (Algebraic Effects)
%%----------------------------------------------------------------------------

effect_decl -> effect upper_ident effect_operations 'end' :
    {effect_decl,
        extract_atom('$2'),
        '$3',
        extract_location('$1')}.

%% Error recovery for incomplete effect declarations
effect_decl -> effect error :
    make_error_declaration(extract_location('$1'), "Incomplete effect declaration", '$2').

effect_operations -> '$empty' :
    [].
effect_operations -> effect_operation effect_operations :
    ['$1' | '$2'].

effect_operation -> operation lower_ident :
    {effect_operation,
        extract_atom('$2'),
        undefined,
        extract_location('$1')}.

effect_operation -> operation lower_ident colon type_expr :
    {effect_operation,
        extract_atom('$2'),
        '$4',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Trait Declarations (Type Classes)
%%----------------------------------------------------------------------------

%% Consolidated trait declaration with optional inheritance
%% Uses extend keyword for inheritance: trait Orderable a extend Comparable a where ... end
trait_decl -> trait upper_ident type_params maybe_trait_extends where trait_members 'end' :
    {trait_decl,
        extract_atom('$2'),
        '$3',
        '$4',
        '$6',
        extract_location('$1')}.

%% Error recovery for incomplete trait declarations
trait_decl -> trait error :
    make_error_declaration(extract_location('$1'), "Incomplete trait declaration", '$2').

%% Optional trait extends clause using extend keyword
maybe_trait_extends -> extend trait_extends_list : '$2'.
maybe_trait_extends -> '$empty' : undefined.

%% Trait extends list (e.g., "Applicative m" or "Eq a, Show a")
trait_extends_list -> trait_constraint :
    ['$1'].
trait_extends_list -> trait_constraint comma trait_extends_list :
    ['$1' | '$3'].

%% Trait constraint (e.g., "Applicative m" or "Eq a")
%% Note: We parse as type_expr and extract trait name and args during semantic analysis
trait_constraint -> type_expr_app :
    extract_trait_constraint('$1').

%% Trait members (method signatures and default implementations)
%% Members can be separated by commas or just newlines
trait_members -> trait_member :
    ['$1'].
trait_members -> trait_member comma trait_members :
    ['$1' | '$3'].
trait_members -> trait_member comma :
    ['$1'].

%% Trait member: either a signature or a default implementation
%% Signature: name : type
%% With constraint: name : type constrain Constraint
trait_member -> lower_ident colon type_expr :
    {trait_sig, extract_atom('$1'), '$3', extract_location('$1')}.

%% Default implementation: name params = expr
trait_member -> lower_ident pattern_list_nonempty equals expr :
    {trait_default, extract_atom('$1'), '$2', '$4', extract_location('$1')}.

%% Trait method with type parsing error
trait_member -> lower_ident colon error :
    make_error_declaration(extract_location('$2'),
        "Invalid method signature. " ++
        "Common issues and solutions:\n" ++
        "  • Cannot use simple tuples as parameters: '(a, b) -> ...'\n" ++
        "  • Try: 'Pair a b -> ...' or '((a -> b), c) -> ...'\n" ++
        "  • See trait signatures documentation for examples", '$3').

%%----------------------------------------------------------------------------
%% Instance Declarations (Trait Implementations)
%%----------------------------------------------------------------------------

%% Instance with constraints - flexible type args
instance_decl -> instance instance_constraints double_arrow upper_ident instance_type_args where instance_methods 'end' :
    {instance_decl,
        extract_atom('$4'),
        '$5',
        '$2',
        '$7',
        extract_location('$1')}.

%% Instance without constraints - flexible type args
instance_decl -> instance upper_ident instance_type_args where instance_methods 'end' :
    {instance_decl,
        extract_atom('$2'),
        '$3',
        undefined,
        '$5',
        extract_location('$1')}.

%% Instance with empty body (no methods to override)
instance_decl -> instance upper_ident instance_type_args where 'end' :
    {instance_decl,
        extract_atom('$2'),
        '$3',
        undefined,
        [],
        extract_location('$1')}.

%% Instance with constraints and empty body
instance_decl -> instance instance_constraints double_arrow upper_ident instance_type_args where 'end' :
    {instance_decl,
        extract_atom('$4'),
        '$5',
        '$2',
        [],
        extract_location('$1')}.

%% Error recovery for incomplete instance declarations
instance_decl -> instance error :
    make_error_declaration(extract_location('$1'), "Incomplete instance declaration", '$2').

%% Instance type arguments (supports unlimited arguments via recursion)
instance_type_args -> type_expr_primary : ['$1'].
instance_type_args -> type_expr_primary instance_type_args : ['$1' | '$2'].

%% Instance constraints (e.g., "Eq a, Show a")
instance_constraints -> trait_constraint :
    ['$1'].
instance_constraints -> trait_constraint comma instance_constraints :
    ['$1' | '$3'].

%% Instance methods (method implementations)
%% Methods can be separated by commas
instance_methods -> instance_method :
    ['$1'].
instance_methods -> instance_method comma instance_methods :
    ['$1' | '$3'].
instance_methods -> instance_method comma :
    ['$1'].

%% Instance method implementation (e.g., "fmap f = match | None -> None | Some x -> Some (f x) end")
instance_method -> transform lower_ident pattern_list equals expr :
    {extract_atom('$2'), {lambda, '$3', '$5', extract_location('$1')}}.

%%----------------------------------------------------------------------------
%% Transform Declarations (Function Definitions)
%%----------------------------------------------------------------------------

transform_decl -> transform_signature transform_clauses :
    {transform_decl,
        extract_transform_name('$1'),
        extract_transform_type('$1'),
        '$2',
        extract_location('$1')}.

%% Flow with only a type signature (no implementation)
transform_decl -> transform_signature :
    {transform_decl,
        extract_transform_name('$1'),
        extract_transform_type('$1'),
        [],
        extract_location('$1')}.

%% Simple transform without type signature (like minimal parser)
transform_decl -> transform lower_ident pattern_list equals expr :
    {transform_decl,
        extract_atom('$2'),
        undefined,
        [{transform_clause, '$3', undefined, '$5', extract_location('$1')}],
        extract_location('$1')}.

transform_decl -> transform lower_ident pattern_list 'when' guards equals expr :
    {transform_decl,
        extract_atom('$2'),
        undefined,
        [{transform_clause, '$3', '$5', '$7', extract_location('$1')}],
        extract_location('$1')}.

%% Error recovery for incomplete transform declarations
transform_decl -> transform error equals expr :
    make_error_declaration(extract_location('$1'), "Invalid transform name", '$2').
transform_decl -> transform lower_ident pattern_list error :
    make_error_declaration(extract_location('$1'), "Missing '=' or expression in transform declaration", '$4').
transform_decl -> transform error :
    make_error_declaration(extract_location('$1'), "Incomplete transform declaration", '$2').

%% Transform signature: transform name : type
%% With constraint: transform name : type constrain Constraint
transform_signature -> transform lower_ident colon type_expr :
    {transform_sig, extract_atom('$2'), '$4', extract_location('$1')}.

transform_clauses -> transform_clause :
    ['$1'].
transform_clauses -> transform_clause transform_clauses :
    ['$1' | '$2'].

transform_clause -> transform lower_ident pattern_list equals expr :
    {transform_clause,
        '$3',
        undefined,
        '$5',
        extract_location('$1')}.

transform_clause -> transform lower_ident pattern_list 'when' guards equals expr :
    {transform_clause,
        '$3',
        '$5',
        '$7',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Match Expressions
%%----------------------------------------------------------------------------

match_clauses -> match_clause :
    ['$1'].
match_clauses -> match_clause match_clauses :
    ['$1' | '$2'].

match_clause -> pipe pattern arrow expr :
    {match_clause,
        '$2',
        undefined,
        '$4',
        extract_location('$1')}.

match_clause -> pipe pattern 'when' guards arrow expr :
    {match_clause,
        '$2',
        '$4',
        '$6',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Patterns
%%----------------------------------------------------------------------------

pattern_list -> '$empty' :
    [].
pattern_list -> pattern_list_nonempty :
    '$1'.

pattern_list_nonempty -> pattern :
    ['$1'].
pattern_list_nonempty -> pattern pattern_list_nonempty :
    ['$1' | '$2'].

%% Comma-separated pattern list (for operation parameters, tuples, etc.)
pattern_list_comma -> '$empty' :
    [].
pattern_list_comma -> pattern :
    ['$1'].
pattern_list_comma -> pattern comma pattern_list_comma :
    ['$1' | '$3'].

pattern -> lower_ident :
    {pat_var, extract_atom('$1'), extract_location('$1')}.

pattern -> underscore :
    {pat_wildcard, extract_location('$1')}.

pattern -> upper_ident :
    {pat_constructor, extract_atom('$1'), [], extract_location('$1')}.

pattern -> upper_ident lparen pattern_list rparen :
    {pat_constructor, extract_atom('$1'), '$3', extract_location('$1')}.

%% Constructor pattern with juxtaposition (Haskell-style): Some x
pattern -> upper_ident pattern_primary :
    {pat_constructor, extract_atom('$1'), ['$2'], extract_location('$1')}.

%% Constructor pattern with multiple arguments: Triple a b c
pattern -> upper_ident pattern_primary pattern_primary :
    {pat_constructor, extract_atom('$1'), ['$2', '$3'], extract_location('$1')}.

%% Primary patterns (atomic, unambiguous)
pattern_primary -> lower_ident :
    {pat_var, extract_atom('$1'), extract_location('$1')}.

pattern_primary -> underscore :
    {pat_wildcard, extract_location('$1')}.

pattern_primary -> integer :
    {pat_literal, extract_value('$1'), integer, extract_location('$1')}.

pattern_primary -> float :
    {pat_literal, extract_value('$1'), float, extract_location('$1')}.

pattern_primary -> string :
    {pat_literal, extract_value('$1'), string, extract_location('$1')}.

pattern_primary -> lparen pattern rparen : '$2'.

pattern_primary -> lbracket rbracket :
    {pat_list, [], extract_location('$1')}.

pattern_primary -> lbracket pattern_list rbracket :
    {pat_list, '$2', extract_location('$1')}.

pattern -> integer :
    {pat_literal, extract_value('$1'), integer, extract_location('$1')}.

pattern -> float :
    {pat_literal, extract_value('$1'), float, extract_location('$1')}.

pattern -> string :
    {pat_literal, extract_value('$1'), string, extract_location('$1')}.

pattern -> lbracket rbracket :
    {pat_list, [], extract_location('$1')}.

pattern -> lbracket pattern_list rbracket :
    {pat_list, '$2', extract_location('$1')}.

pattern -> lparen tuple_pattern_list rparen :
    {pat_tuple, '$2', extract_location('$1')}.

pattern -> lbrace rbrace :
    {pat_record, [], extract_location('$1')}.

pattern -> lbrace record_pattern_fields rbrace :
    {pat_record, '$2', extract_location('$1')}.

%% Cons pattern (h :: t)
pattern -> pattern cons pattern :
    {pat_cons, '$1', '$3', extract_location('$2')}.

%% Tuple pattern lists (comma-separated patterns)
tuple_pattern_list -> pattern comma pattern :
    ['$1', '$3'].
tuple_pattern_list -> pattern comma tuple_pattern_list :
    ['$1' | '$3'].

%% Record pattern fields (field: pattern, field: pattern, ...)
record_pattern_fields -> record_pattern_field :
    ['$1'].
record_pattern_fields -> record_pattern_field comma record_pattern_fields :
    ['$1' | '$3'].

record_pattern_field -> lower_ident colon pattern :
    {extract_atom('$1'), '$3'}.

%%----------------------------------------------------------------------------
%% Guards
%%----------------------------------------------------------------------------

guards -> guard :
    ['$1'].
guards -> guard comma guards :
    ['$1' | '$3'].

guard -> expr : '$1'.

%%----------------------------------------------------------------------------
%% Expressions
%%----------------------------------------------------------------------------

%% Expression hierarchy: expr > expr_app > expr_primary

expr -> expr pipe_right expr :
    {binary_op, pipe_right, '$1', '$3', extract_location('$2')}.

expr -> expr plus expr :
    {binary_op, plus, '$1', '$3', extract_location('$2')}.

expr -> expr minus expr :
    {binary_op, minus, '$1', '$3', extract_location('$2')}.

expr -> expr star expr :
    {binary_op, star, '$1', '$3', extract_location('$2')}.

expr -> expr slash expr :
    {binary_op, slash, '$1', '$3', extract_location('$2')}.

expr -> expr eq expr :
    {binary_op, eq, '$1', '$3', extract_location('$2')}.

expr -> expr neq expr :
    {binary_op, neq, '$1', '$3', extract_location('$2')}.

expr -> expr setoid_eq expr :
    {binary_op, setoid_eq, '$1', '$3', extract_location('$2')}.

expr -> expr setoid_neq expr :
    {binary_op, setoid_neq, '$1', '$3', extract_location('$2')}.

expr -> expr lt expr :
    {binary_op, lt, '$1', '$3', extract_location('$2')}.

expr -> expr gt expr :
    {binary_op, gt, '$1', '$3', extract_location('$2')}.

expr -> expr lte expr :
    {binary_op, lte, '$1', '$3', extract_location('$2')}.

expr -> expr gte expr :
    {binary_op, gte, '$1', '$3', extract_location('$2')}.

%% Logical operators
expr -> expr 'and' expr :
    {binary_op, 'and', '$1', '$3', extract_location('$2')}.

expr -> expr 'or' expr :
    {binary_op, 'or', '$1', '$3', extract_location('$2')}.

%% List concatenation operator (right-associative)
expr -> expr plus_plus expr :
    {binary_op, plus_plus, '$1', '$3', extract_location('$2')}.

%% List cons operator (right-associative)
expr -> expr cons expr :
    {cons_expr, '$1', '$3', extract_location('$2')}.

expr -> expr_app : '$1'.

%% Function application (left-associative, juxtaposition)
expr_app -> expr_app expr_primary :
    {app, '$1', ['$2'], extract_location('$1')}.

expr_app -> expr_app dot lower_ident :
    {record_access, '$1', extract_atom('$3'), extract_location('$2')}.

expr_app -> expr_primary : '$1'.

%% Primary expressions (highest precedence, atomic)
expr_primary -> literal : '$1'.

expr_primary -> lower_ident :
    {var, extract_atom('$1'), extract_location('$1')}.

expr_primary -> upper_ident :
    {var, extract_atom('$1'), extract_location('$1')}.

expr_primary -> lparen expr rparen :
    '$2'.

expr_primary -> lparen tuple_expr_list rparen :
    {tuple_expr, '$2', extract_location('$1')}.

%% Tuple expression lists (comma-separated expressions)
tuple_expr_list -> expr comma expr :
    ['$1', '$3'].
tuple_expr_list -> expr comma tuple_expr_list :
    ['$1' | '$3'].

expr_primary -> 'let' lower_ident equals expr 'in' expr :
    {let_expr,
        [{pat_var, extract_atom('$2'), extract_location('$2')}, '$4'],
        '$6',
        extract_location('$1')}.

%% Lambda expressions: fn param -> body
expr_primary -> fn lower_ident arrow expr :
    {lambda, [{pat_var, extract_atom('$2'), extract_location('$2')}], '$4', extract_location('$1')}.

%% Note: if/then/else removed - use match on Bool in library code

expr_primary -> perform_expr : '$1'.

expr_primary -> try_with_expr : '$1'.

expr_primary -> do_expr : '$1'.

%% Match expressions
%% Pattern-only: match | pat -> expr | ... end
expr_primary -> match match_clauses 'end' :
    {match_expr, undefined, '$2', extract_location('$1')}.

%% With scrutinee: match scrutinee of | pat -> expr | ... end
expr_primary -> match expr 'of' match_clauses 'end' :
    {match_expr, '$2', '$4', extract_location('$1')}.

expr_primary -> lbracket rbracket :
    {list_expr, [], extract_location('$1')}.

expr_primary -> lbracket expr_list rbracket :
    {list_expr, '$2', extract_location('$1')}.

expr_primary -> lbrace rbrace :
    {record_expr, [], undefined, extract_location('$1')}.

expr_primary -> lbrace record_fields rbrace :
    {record_expr, '$2', undefined, extract_location('$1')}.

%% Record fields (field: expr, field: expr, ...)
record_fields -> record_field :
    ['$1'].
record_fields -> record_field comma record_fields :
    ['$1' | '$3'].

record_field -> lower_ident colon expr :
    {extract_atom('$1'), '$3'}.

%% Expression lists (for list literals, function arguments, etc.)
expr_list -> expr :
    ['$1'].
expr_list -> expr comma expr_list :
    ['$1' | '$3'].

%% Optional expression list (can be empty)
expr_list_opt -> '$empty' :
    [].
expr_list_opt -> expr_list :
    '$1'.

%%----------------------------------------------------------------------------
%% Literals
%%----------------------------------------------------------------------------

literal -> integer :
    {literal, extract_value('$1'), integer, extract_location('$1')}.

literal -> float :
    {literal, extract_value('$1'), float, extract_location('$1')}.

literal -> string :
    {literal, extract_value('$1'), string, extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Do-Notation (Monadic Composition)
%%----------------------------------------------------------------------------

%% Do block: do { stmt; stmt; expr }
do_expr -> 'do' lbrace do_statements rbrace :
    {do_expr, '$3', extract_location('$1')}.

%% Do statements (semicolon-separated)
do_statements -> expr :
    [{do_return, '$1', extract_location('$1')}].
do_statements -> do_statement semicolon do_statements :
    ['$1' | '$3'].

%% Do statement types:
%% - Bind: x <- ma
%% - Let: let x = e
%% - Action: expr (for effects without binding)

%% Bind statement: x <- ma
do_statement -> lower_ident left_arrow expr :
    {do_bind, extract_atom('$1'), '$3', extract_location('$1')}.

%% Let statement in do: let x = e
do_statement -> 'let' lower_ident equals expr :
    {do_let, extract_atom('$2'), '$4', extract_location('$1')}.

%% Action statement: just an expression (for effects that don't return useful values)
do_statement -> expr :
    {do_action, '$1', extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Effect Expressions
%%----------------------------------------------------------------------------

%% Perform expression: perform Effect.operation(args)
%% Parentheses are required for now to avoid shift/reduce conflicts
perform_expr -> perform upper_ident dot lower_ident lparen expr_list_opt rparen :
    {perform_expr,
        extract_atom('$2'),
        extract_atom('$4'),
        '$6',
        extract_location('$1')}.

%% Handle expression: handle expr then { handlers }
%% Using 'then' keyword to separate expr from handler block
try_with_expr -> handle expr then lbrace handler_clauses rbrace :
    {handle_expr,
        '$2',
        '$5',
        extract_location('$1')}.

%% Handler clauses: Effect { operation cases }
handler_clauses -> handler_clause :
    ['$1'].
handler_clauses -> handler_clause handler_clauses :
    ['$1' | '$2'].

handler_clause -> upper_ident lbrace operation_cases rbrace :
    {handler_clause,
        extract_atom('$1'),
        '$3',
        extract_location('$1')}.

%% Operation cases: operation(params) -> expr
%% Note: Left-recursive to avoid issues with right-associative arrow
operation_cases -> operation_cases operation_case :
    '$1' ++ ['$2'].
operation_cases -> operation_case :
    ['$1'].

operation_case -> lower_ident lparen pattern_list_comma rparen arrow expr_primary :
    {operation_case,
        extract_atom('$1'),
        '$3',
        '$6',
        extract_location('$1')}.

operation_case -> lower_ident arrow expr_primary :
    {operation_case,
        extract_atom('$1'),
        [],
        '$3',
        extract_location('$1')}.

%%----------------------------------------------------------------------------
%% Type Expressions
%%----------------------------------------------------------------------------

type_expr -> type_expr arrow type_expr :
    {type_fun, '$1', '$3', extract_location('$2')}.

type_expr -> forall type_params dot type_expr :
    {type_forall, '$2', '$4', extract_location('$1')}.

type_expr -> type_expr_app slash lbrace rbrace :
    {type_effect, '$1', [], extract_location('$2')}.

type_expr -> type_expr_app slash lbrace effect_list_nonempty rbrace :
    {type_effect, '$1', '$4', extract_location('$2')}.

%% Constrained type: Type constrain Constraint1, Constraint2
type_expr -> type_expr constrain type_constraints :
    {constrained_type, '$3', '$1', extract_location('$2')}.

type_expr -> type_expr_app : '$1'.

%% Type constraints (comma-separated list of trait constraints)
type_constraints -> trait_constraint :
    ['$1'].
type_constraints -> trait_constraint comma type_constraints :
    ['$1' | '$3'].



%% Type application (higher precedence than function arrows)
%% Supports both type constructors (Maybe a, Either a b, Triple a b c)
%% and type variables (f a, c a a) for higher-kinded types
type_expr_app -> upper_ident type_expr_primary_list :
    {type_app,
        {type_con, extract_atom('$1'), extract_location('$1')},
        '$2',
        extract_location('$1')}.

%% Type application with type variable (for higher-kinded types like f a)
type_expr_app -> lower_ident type_expr_primary_list :
    {type_app,
        {type_var, extract_atom('$1'), extract_location('$1')},
        '$2',
        extract_location('$1')}.

type_expr_app -> type_expr_primary :
    '$1'.

%% Multiple type arguments (one or more, space-separated)
%% Examples: a, Int, (a -> b), a b, a b c
type_expr_primary_list -> type_expr_primary :
    ['$1'].
type_expr_primary_list -> type_expr_primary type_expr_primary_list :
    ['$1' | '$2'].

%% Primary type expressions (atomic)
type_expr_primary -> lower_ident :
    {type_var, extract_atom('$1'), extract_location('$1')}.

type_expr_primary -> upper_ident :
    {type_con, extract_atom('$1'), extract_location('$1')}.

%% Parenthesized type expression (no comma)
type_expr_primary -> lparen type_expr rparen :
    '$2'.

%% Tuple type (requires comma - minimum 2 elements)
%% This eliminates ambiguity with parenthesized types
type_expr_primary -> lparen type_expr comma type_expr_list rparen :
    {type_tuple, ['$2' | '$4'], extract_location('$1')}.

type_expr_primary -> lbrace rbrace :
    {type_record, [], undefined, extract_location('$1')}.

type_expr_primary -> lbrace type_record_fields rbrace :
    {type_record, '$2', undefined, extract_location('$1')}.

%% Type expression list (for tuples - at least one element after the first)
type_expr_list -> type_expr :
    ['$1'].
type_expr_list -> type_expr comma type_expr_list :
    ['$1' | '$3'].

%% Legacy type_list for backwards compatibility (used in other contexts)
type_list -> type_expr :
    ['$1'].
type_list -> type_expr comma type_list :
    ['$1' | '$3'].

%% Record type fields (field: Type, field: Type, ...)
type_record_fields -> type_record_field :
    ['$1'].
type_record_fields -> type_record_field comma type_record_fields :
    ['$1' | '$3'].

type_record_field -> lower_ident colon type_expr :
    {extract_atom('$1'), '$3'}.

%% Effect lists (for effect annotations)
effect_list -> '$empty' :
    [].
effect_list -> effect_list_nonempty :
    '$1'.

effect_list_nonempty -> upper_ident :
    [extract_atom('$1')].
effect_list_nonempty -> upper_ident comma effect_list_nonempty :
    [extract_atom('$1') | '$3'].

%%============================================================================
%% Erlang Code - Helper Functions
%%============================================================================

Erlang code.

%% @doc Extract atom from token
%% Delegates to catena_compiler_utils to avoid code duplication
extract_atom(Token) -> catena_compiler_utils:extract_atom(Token).

%% @doc Extract value from token
%% Delegates to catena_compiler_utils to avoid code duplication
extract_value(Token) -> catena_compiler_utils:extract_value(Token).

%% @doc Extract location from token or AST node
%% Delegates to catena_compiler_utils to avoid code duplication
%% Supports both legacy {line, N} format and enhanced {location, ...} format
extract_location(Node) -> catena_compiler_utils:extract_location(Node).

%% @doc Extract transform name from transform signature
extract_transform_name({transform_sig, Name, _Type, _Loc}) -> Name.

%% @doc Extract transform type from transform signature
extract_transform_type({transform_sig, _Name, Type, _Loc}) -> Type.

%% @doc Create an error declaration node for error recovery
%% Returns a special AST node that marks a parsing error
make_error_declaration(Location, Message, _ErrorInfo) ->
    {error_decl,
        Message,
        Location}.

%% @doc Extract trait constraint from type expression
%% Delegates to catena_compiler_utils for centralized implementation
%% This helper is shared between parser and type checker
extract_trait_constraint(TypeExpr) ->
    catena_compiler_utils:extract_trait_constraint(TypeExpr).


