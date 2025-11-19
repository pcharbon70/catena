%%%
%%% @doc Type Pretty-Printing
%%%
%%% Converts internal type representations to human-readable strings
%%% for error messages, REPL output, and debugging. Uses Catena-friendly
%%% terminology and syntax.
%%%
%%% @end
%%%
-module(catena_type_pp).

-export([
    pp_type/1,
    pp_scheme/1,
    pp_effects/1,
    format/1
]).

%%====================================================================
%% Pretty-Printing Functions
%%====================================================================

%% @doc Pretty-print a type to a human-readable string.
%%
%% Converts an internal type representation to a readable string using
%% mathematical notation and Catena-friendly syntax. Optimized using iolists
%% internally to minimize string copying.
%%
%% Type syntax:
%% - Type variables: α1, α2, α3, ...
%% - Type constructors: integer, string, List, Maybe, ...
%% - Type application: List<Int>, Maybe<String>
%% - Function types: Int -> String (pure), Int -> String / {io} (impure)
%% - Records: {x: Int, y: Int} (closed), {x: Int | ρ1} (open)
%% - Tuples: (Int, String, Bool)
%% - Variants: Red | Green | Blue, Some a | None
%%
%% @param Type The type to pretty-print
%% @returns A flattened string representation
%%
%% @see pp_scheme/1
%% @see pp_effects/1
%%
%% @example
%% ```
%% %% Type variable
%% Str1 = catena_type_pp:pp_type({tvar, alpha}).
%% %% → "alpha"
%%
%% %% Function type with effects
%% FuncType = catena_types:tfun(
%%     catena_types:tcon(string),
%%     catena_types:tcon(unit),
%%     catena_types:singleton_effect(io)
%% ),
%% Str2 = catena_type_pp:pp_type(FuncType).
%% %% → "string -> unit / {io}"
%%
%% %% Record type
%% Point = catena_types:trecord([{x, {tcon, integer}}, {y, {tcon, integer}}], closed),
%% Str3 = catena_type_pp:pp_type(Point).
%% %% → "{x: integer, y: integer}"
%% '''
-spec pp_type(catena_types:ty()) -> string().
pp_type(Type) ->
    lists:flatten(pp_type_iolist(Type)).

%% @doc Format a type to a string (alias for pp_type/1)
-spec format(catena_types:ty()) -> string().
format(Type) ->
    pp_type(Type).

%% Internal helper that builds iolists for efficiency
pp_type_iolist({tvar, Id}) when is_atom(Id) ->
    atom_to_list(Id);
pp_type_iolist({tvar, Id}) when is_list(Id) ->
    Id;
pp_type_iolist({tvar, Id}) when is_integer(Id) ->
    ["α", integer_to_list(Id)];

pp_type_iolist({tcon, Name}) ->
    % Handle special case for maybe_type -> maybe (user-friendly naming)
    % Note: 'maybe' is a reserved word in Erlang/OTP 25+, so we use maybe_type internally
    case Name of
        maybe_type -> "maybe";
        _ -> atom_to_list(Name)
    end;

pp_type_iolist({tapp, Constructor, Args}) ->
    case Args of
        [] ->
            pp_type_iolist(Constructor);
        _ ->
            ArgStrs = [pp_type_iolist(Arg) || Arg <- Args],
            [pp_type_iolist(Constructor), "<", join(ArgStrs, ", "), ">"]
    end;

pp_type_iolist({tfun, From, To, Effects}) ->
    FromStr = pp_type_with_parens_iolist(From),
    ToStr = pp_type_iolist(To),
    EffStr = pp_effects_iolist(Effects),

    case EffStr of
        [] ->
            % Pure function: α -> β
            [FromStr, " -> ", ToStr];
        _ ->
            % Effectful function: α -> β / {Effects}
            [FromStr, " -> ", ToStr, " / ", EffStr]
    end;

pp_type_iolist({trecord, Fields, RowVar}) ->
    FieldStrs = [[atom_to_list(Name), ": ", pp_type_iolist(Type)]
                || {Name, Type} <- Fields],
    FieldsStr = join(FieldStrs, ", "),

    case RowVar of
        closed ->
            ["{", FieldsStr, "}"];
        VarId when is_integer(VarId) ->
            case FieldStrs of
                [] -> ["{| ρ", integer_to_list(VarId), "}"];
                _ -> ["{", FieldsStr, " | ρ", integer_to_list(VarId), "}"]
            end;
        VarId when is_atom(VarId) ->
            case FieldStrs of
                [] -> ["{| ", atom_to_list(VarId), "}"];
                _ -> ["{", FieldsStr, " | ", atom_to_list(VarId), "}"]
            end;
        VarId when is_list(VarId) ->
            case FieldStrs of
                [] -> ["{| ", VarId, "}"];
                _ -> ["{", FieldsStr, " | ", VarId, "}"]
            end
    end;

pp_type_iolist({ttuple, Elements}) ->
    case Elements of
        [] -> "()";
        _ ->
            ElemStrs = [pp_type_iolist(Elem) || Elem <- Elements],
            ["(", join(ElemStrs, ", "), ")"]
    end;

pp_type_iolist({tvariant, Constructors}) ->
    ConStrs = [pp_variant_constructor_iolist(Name, Args)
              || {Name, Args} <- Constructors],
    join(ConStrs, " | ").

%% Internal helpers for iolists
pp_variant_constructor_iolist(Name, []) ->
    atom_to_list(Name);
pp_variant_constructor_iolist(Name, Args) ->
    ArgStrs = [pp_type_with_parens_iolist(Arg) || Arg <- Args],
    [atom_to_list(Name), " ", join(ArgStrs, " ")].

pp_type_with_parens_iolist(Type) ->
    case needs_parens(Type) of
        true -> ["(", pp_type_iolist(Type), ")"];
        false -> pp_type_iolist(Type)
    end.

-spec needs_parens(catena_types:ty()) -> boolean().
needs_parens({tfun, _, _, _}) -> true;
needs_parens({tvariant, _}) -> true;
needs_parens(_) -> false.

%% @doc Pretty-print an effect set to a human-readable string.
%%
%% Converts an effect set to string notation. Empty effect sets (pure
%% functions) render as an empty string. Non-empty effect sets render
%% as comma-separated effects in braces.
%%
%% Effect set syntax:
%% - Pure (empty): "" (no output)
%% - Single effect: "{io}"
%% - Multiple effects: "{error, io, state}"  (alphabetically sorted)
%%
%% @param Effects The effect set to pretty-print
%% @returns String representation of the effect set
%%
%% @see pp_type/1
%%
%% @example
%% ```
%% %% Pure function (empty effect set)
%% Pure = catena_types:empty_effects(),
%% Str1 = catena_type_pp:pp_effects(Pure).
%% %% → ""
%%
%% %% Single effect
%% IoEffect = catena_types:singleton_effect(io),
%% Str2 = catena_type_pp:pp_effects(IoEffect).
%% %% → "{io}"
%%
%% %% Multiple effects (sorted)
%% Combined = catena_types:normalize_effects([state, io, error]),
%% Str3 = catena_type_pp:pp_effects(Combined).
%% %% → "{error, io, state}"
%% '''
-spec pp_effects(catena_types:effect_set()) -> string().
pp_effects(Effects) ->
    lists:flatten(pp_effects_iolist(Effects)).

pp_effects_iolist({effect_set, []}) ->
    [];  % Empty effect set (pure)
pp_effects_iolist({effect_set, Effects}) ->
    EffStrs = [atom_to_list(Eff) || Eff <- Effects],
    ["{", join(EffStrs, ", "), "}"].

%% @doc Pretty-print a type scheme to a human-readable string.
%%
%% Converts a type scheme to string notation using forall (∀) for
%% quantified variables. Monomorphic schemes render as plain types.
%% Polymorphic schemes show quantified variables before the type.
%%
%% Scheme syntax:
%% - Monomorphic: "Int -> String"
%% - Polymorphic: "∀α. α -> α"
%% - Multiple variables: "∀α β. (α -> β) -> List<α> -> List<β>"
%%
%% @param Scheme The type scheme to pretty-print
%% @returns String representation of the scheme
%%
%% @see pp_type/1
%% @see catena_type_scheme:mono/1
%% @see catena_type_scheme:poly/2
%%
%% @example
%% ```
%% %% Monomorphic scheme
%% IntType = catena_types:tcon(integer),
%% MonoScheme = catena_type_scheme:mono(IntType),
%% Str1 = catena_type_pp:pp_scheme(MonoScheme).
%% %% → "integer"
%%
%% %% Polymorphic identity function: ∀α. α -> α
%% IdScheme = catena_type_scheme:poly(
%%     [1],
%%     catena_types:tfun({tvar, 1}, {tvar, 1}, catena_types:empty_effects())
%% ),
%% Str2 = catena_type_pp:pp_scheme(IdScheme).
%% %% → "∀α1. α1 -> α1"
%%
%% %% Polymorphic map function: ∀α β. (α -> β) -> List<α> -> List<β>
%% MapScheme = catena_type_scheme:poly([1, 2], MapType),
%% Str3 = catena_type_pp:pp_scheme(MapScheme).
%% %% → "∀α1 α2. (α1 -> α2) -> List<α1> -> List<α2>"
%% '''
-spec pp_scheme(catena_type_scheme:scheme()) -> string().
pp_scheme({mono, Type}) ->
    pp_type(Type);
pp_scheme({poly, Vars, Type}) ->
    VarStrs = [format_type_var(V) || V <- Vars],
    lists:flatten(["∀", join(VarStrs, " "), ". ", pp_type_iolist(Type)]).

%% Helper to format type variables consistently
format_type_var(V) when is_integer(V) ->
    ["α", integer_to_list(V)];
format_type_var(V) when is_atom(V) ->
    atom_to_list(V);
format_type_var(V) when is_list(V) ->
    V.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @doc Join iolist elements with a separator (like string:join but for iolists)
join([], _Sep) -> [];
join([H], _Sep) -> H;
join([H|T], Sep) ->
    [H, Sep | join(T, Sep)].
