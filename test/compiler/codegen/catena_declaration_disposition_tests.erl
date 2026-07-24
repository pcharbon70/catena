-module(catena_declaration_disposition_tests).

-include_lib("eunit/include/eunit.hrl").

static_and_executable_declarations_are_classified_test() ->
    Source =
        "module Dispositions\n"
        "export transform run\n"
        "type Maybe a = None | Some a\n"
        "effect Console\n"
        "operation print : String -> Unit\n"
        "end\n"
        "transform run = 42\n"
        "transform hidden : Int\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Dispositions = declaration_dispositions(Unit),
    ?assertEqual(
        lowered,
        disposition_for(Dispositions, transform, run)
    ),
    ?assertEqual(
        erased_static,
        disposition_for(Dispositions, transform, hidden)
    ),
    ?assertEqual(
        erased_static,
        disposition_for(Dispositions, type, 'Maybe')
    ),
    ?assertEqual(
        erased_static,
        disposition_for(Dispositions, effect, 'Console')
    ).

representation_is_selected_before_static_erasure_test() ->
    Source =
        "module Representations\n"
        "export transform run\n"
        "type Maybe a = None | Some a\n"
        "effect Console\n"
        "operation print : String -> Unit\n"
        "end\n"
        "transform run = 42\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    TypeDisposition = find_disposition(
        declaration_dispositions(Unit),
        type,
        'Maybe'
    ),
    TypeRepresentation = maps:get(
        representation,
        TypeDisposition
    ),
    ?assertEqual(
        [
            #{name => 'None', arity => 0},
            #{name => 'Some', arity => 1}
        ],
        [
            maps:with([name, arity], Constructor)
            || Constructor <-
                maps:get(constructors, TypeRepresentation)
        ]
    ),
    EffectDisposition = find_disposition(
        declaration_dispositions(Unit),
        effect,
        'Console'
    ),
    [Operation] = maps:get(
        operations,
        maps:get(representation, EffectDisposition)
    ),
    ?assertEqual(print, maps:get(name, Operation)),
    {ok, BackendAST} =
        catena_declaration_disposition:prepare_for_codegen(Unit),
    {module, _, _, _, ActiveDeclarations, _} = BackendAST,
    ?assertMatch(
        [{transform_decl, run, _, [_ | _], _}],
        ActiveDeclarations
    ).

imports_and_exports_receive_explicit_dispositions_test() ->
    Source =
        "module Linked\n"
        "export transform run\n"
        "import Prelude\n"
        "transform run = 42\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(
        Source,
        #{process_imports => false}
    ),
    Dispositions = catena_compilation_unit:dispositions(Unit),
    [ImportDisposition] = [
        Disposition
        || Disposition <- Dispositions,
           maps:get(subject, Disposition) =:= import
    ],
    ?assertEqual(unsupported, maps:get(disposition, ImportDisposition)),
    ?assertEqual(
        executable_import_linkage_deferred,
        maps:get(reason, ImportDisposition)
    ),
    [ExportDisposition] = [
        Disposition
        || Disposition <- Dispositions,
           maps:get(subject, Disposition) =:= export
    ],
    ?assertEqual(lowered, maps:get(disposition, ExportDisposition)),
    ?assertEqual(export_transform, maps:get(kind, ExportDisposition)).

exported_signature_without_implementation_is_rejected_test() ->
    Source =
        "module Missing\n"
        "export transform required\n"
        "transform required : Int\n",
    Result = catena_compile:compile_string_to_core(Source),
    ?assertMatch(
        {error, {backend_error, missing_transform_implementation, #{}}},
        Result
    ),
    {error, Diagnostic} = Result,
    Details = catena_backend_error:details(Diagnostic),
    ?assertEqual(required, maps:get(source_identity, Details)),
    ?assertEqual({location, 3, 0}, maps:get(location, Details)).

nonexported_signature_can_be_erased_test() ->
    Source =
        "module OptionalSignature\n"
        "export transform run\n"
        "transform run = 42\n"
        "transform helper : Int\n",
    {ok, CoreModule} = catena_compile:compile_string_to_core(Source),
    ?assertEqual(1, length(cerl:module_defs(CoreModule))),
    [Export] = cerl:module_exports(CoreModule),
    ?assertEqual(run, cerl:fname_id(Export)).

runtime_bearing_declarations_are_not_silently_erased_test() ->
    Location = {location, 9, 3},
    RuntimeDeclarations = [
        {test_decl, "deferred", {literal, integer, 1, Location}, Location},
        {property_decl, "deferred",
            {property_forall, [{x, 'Int'}],
                {var, x, Location}, Location},
            Location}
    ],
    lists:foreach(
        fun(Declaration) ->
            Unit = unit_with_declaration(Declaration),
            {error, Diagnostic} =
                catena_declaration_disposition:prepare_for_codegen(Unit),
            ?assertEqual(
                invalid_declaration_disposition,
                catena_backend_error:category(Diagnostic)
            ),
            ?assertEqual(
                Location,
                maps:get(
                    location,
                    catena_backend_error:details(Diagnostic)
                )
            )
        end,
        RuntimeDeclarations
    ).

traits_and_instances_have_promoted_dispositions_test() ->
    Source =
        "module PromotedTraits\n"
        "type Flag = On | Off\n"
        "trait Comparable a where\n"
        "  equals : a -> a -> Bool\n"
        "end\n"
        "instance Comparable Flag where\n"
        "  transform equals left right = true\n"
        "end\n",
    {ok, Unit} = catena_compile:compile_string_to_unit(Source),
    Dispositions = declaration_dispositions(Unit),
    ?assertEqual(
        erased_static,
        disposition_for(Dispositions, trait, 'Comparable')
    ),
    ?assertEqual(
        runtime_lowered,
        disposition_for(Dispositions, instance, 'Comparable')
    ),
    {ok, _BackendAST} =
        catena_declaration_disposition:prepare_for_codegen(Unit).

unknown_declaration_is_explicitly_unsupported_test() ->
    Location = {location, 12, 4},
    Unit = unit_with_declaration(
        {future_actor_decl, worker, Location}
    ),
    [Disposition] = declaration_dispositions(Unit),
    ?assertEqual(unsupported, maps:get(disposition, Disposition)),
    ?assertEqual(unknown_declaration, maps:get(reason, Disposition)),
    ?assertMatch(
        {error,
            {backend_error, invalid_declaration_disposition, #{}}},
        catena_declaration_disposition:prepare_for_codegen(Unit)
    ).

unit_with_declaration(Declaration) ->
    Module = disposition_fixture,
    AST = {module, Module, [], [], [Declaration], {location, 1, 1}},
    Typed = {typed_module, Module, [Declaration], #{}},
    ValidationState = maps:from_list([
        {Stage, passed}
        || Stage <- catena_compilation_unit:validated_stages()
    ]),
    {ok, Unit0} = catena_compilation_unit:new(
        AST,
        Typed,
        #{
            validation_state => ValidationState,
            options => #{},
            source_identity => #{kind => test}
        }
    ),
    {ok, Unit} = catena_declaration_disposition:classify(Unit0),
    Unit.

declaration_dispositions(Unit) ->
    [
        Disposition
        || Disposition <- catena_compilation_unit:dispositions(Unit),
           maps:get(subject, Disposition) =:= declaration
    ].

disposition_for(Dispositions, Kind, Name) ->
    maps:get(disposition, find_disposition(Dispositions, Kind, Name)).

find_disposition(Dispositions, Kind, Name) ->
    hd([
        Disposition
        || Disposition <- Dispositions,
           maps:get(kind, Disposition) =:= Kind,
           declaration_name(maps:get(declaration, Disposition)) =:= Name
    ]).

declaration_name({transform_decl, Name, _, _, _}) -> Name;
declaration_name({type_decl, Name, _, _, _, _}) -> Name;
declaration_name({effect_decl, Name, _, _}) -> Name;
declaration_name({trait_decl, Name, _, _, _, _}) -> Name;
declaration_name({instance_decl, Name, _, _, _, _}) -> Name;
declaration_name({test_decl, Name, _, _}) -> Name;
declaration_name({property_decl, Name, _, _}) -> Name;
declaration_name(Declaration) when is_tuple(Declaration) ->
    element(2, Declaration).
