defmodule Catena.Data do
  @moduledoc "Nominal datatype declaration elaboration for the C002 compiler slice."

  alias Catena.{Diagnostic, Type}
  alias Catena.Type.{Parser, Scheme}

  @type environment :: map()

  @spec elaborate(map(), [map()]) :: environment()
  def elaborate(ast, interfaces \\ []) do
    imported = imported_types(interfaces)
    headers = allocate_headers(ast, imported)
    exports = Map.new(ast.type_exports, &{&1.name, &1.visibility})
    validate_exports!(exports, headers, ast)

    {types, _constructors} = elaborate_groups(ast, headers, exports)
    types_by_id = Map.new(types, &{&1.id, &1})
    imported_by_id = Map.new(imported, &{&1.id, &1})
    all_by_id = Map.merge(imported_by_id, types_by_id)
    types = annotate_shapes_and_inhabitation(types, all_by_id)
    types_by_id = Map.new(types, &{&1.id, &1})
    constructors = constructor_index(types ++ imported) |> apply_imports(ast.imports)

    %{
      origin: ast.origin,
      module: ast.module,
      types: types,
      types_by_name: headers,
      types_by_id: Map.merge(imported_by_id, types_by_id),
      constructors: constructors,
      imported_interfaces: interfaces
    }
  end

  @doc """
  True when one type belongs to the closed comparable set at 0.1.30:
  `Int`, `Bool`, `Float`, tuples of comparable element types, and
  nominal types whose every constructor field type is comparable after
  argument substitution. Functions, variables, abstract types without
  visible constructors, and everything else are non-comparable.
  """
  @spec comparable_type?(Catena.Type.t(), environment()) :: boolean()
  def comparable_type?(:integer, _environment), do: true
  def comparable_type?(:boolean, _environment), do: true
  def comparable_type?(:float, _environment), do: true

  def comparable_type?({:tuple, elements}, environment),
    do: Enum.all?(elements, &comparable_type?(&1, environment))

  def comparable_type?({:nominal, id, arguments}, environment) do
    with %{} = datatype <- Map.get(environment.types_by_id, id),
         [_ | _] = constructors <- Map.get(datatype, :constructors, []) do
      Enum.all?(constructors, fn constructor ->
        substitution =
          case Map.get(constructor, :result) do
            {:nominal, _, parameter_types} ->
              parameter_types
              |> Enum.zip(arguments)
              |> Enum.flat_map(fn
                {{:var, variable}, argument} -> [{variable, argument}]
                _other -> []
              end)
              |> Map.new()

            _other ->
              %{}
          end

        constructor
        |> Map.get(:fields, [])
        |> Enum.all?(&comparable_type?(Catena.Type.apply(&1.type, substitution), environment))
      end)
    else
      _other -> false
    end
  end

  def comparable_type?(_type, _environment), do: false

  @spec resolve_constructor!(environment(), String.t(), String.t() | nil) :: map()
  def resolve_constructor!(environment, reference, path \\ nil) do
    case Map.fetch(environment.constructors, reference) do
      {:ok, constructor} -> constructor
      :error -> fail("A004", "unknown or inaccessible constructor #{inspect(reference)}", path)
    end
  end

  @spec constructor_scheme(map()) :: Scheme.t()
  def constructor_scheme(constructor) do
    type =
      Enum.reduce(Enum.reverse(constructor.fields), constructor.result, &{:function, &1.type, &2})

    %Scheme{variables: constructor.variables, type: type}
  end

  defp imported_types(interfaces) do
    types =
      Enum.flat_map(interfaces, fn interface ->
        Enum.map(Map.get(interface, :types, []), fn type -> Map.put(type, :imported?, true) end)
      end)

    conflict =
      types
      |> Enum.group_by(&{&1.module, &1.name})
      |> Enum.find(fn {_name, candidates} ->
        candidates |> Enum.map(& &1.id) |> Enum.uniq() |> length() > 1
      end)

    if conflict do
      {{module, name}, _candidates} = conflict
      fail("A005", "conflicting nominal identities for #{module}.#{name}", "$.interfaces")
    end

    Enum.uniq_by(types, & &1.id)
  end

  defp allocate_headers(ast, imported) do
    imported_headers =
      imported
      |> Enum.flat_map(fn type ->
        [
          {"#{type.module}.#{type.name}", %{id: type.id, arity: type.arity, imported?: true}},
          {type.id, %{id: type.id, arity: type.arity, imported?: true}}
        ]
      end)
      |> Map.new()

    local_declarations = Enum.flat_map(ast.type_groups, & &1.declarations)

    duplicate =
      local_declarations
      |> Enum.group_by(& &1.name)
      |> Enum.find(fn {_name, declarations} -> length(declarations) > 1 end)

    if duplicate do
      {name, _} = duplicate
      fail("A002", "duplicate type declaration #{name}", "$.type_groups")
    end

    Enum.reduce(local_declarations, imported_headers, fn declaration, headers ->
      id = nominal_id(ast.origin, ast.module, declaration.name)
      header = %{id: id, arity: length(declaration.parameters), imported?: false}

      headers
      |> Map.put(declaration.name, header)
      |> Map.put("#{ast.module}.#{declaration.name}", header)
      |> Map.put(id, header)
    end)
  end

  defp validate_exports!(exports, headers, ast) do
    Enum.each(exports, fn {name, _visibility} ->
      case Map.get(headers, name) do
        %{imported?: false} ->
          :ok

        _ ->
          fail("A001", "exported type #{name} is not declared by #{ast.module}", "$.type_exports")
      end
    end)
  end

  defp elaborate_groups(ast, headers, exports) do
    Enum.reduce(ast.type_groups, {[], %{}}, fn group, {types, constructors} ->
      group_types =
        Enum.map(group.declarations, fn declaration ->
          elaborate_declaration(
            declaration,
            ast,
            headers,
            Map.get(exports, declaration.name, :internal)
          )
        end)

      group_constructors =
        Enum.reduce(group_types, constructors, fn type, acc ->
          Enum.reduce(type.constructors, acc, fn constructor, current ->
            current
            |> put_unique!("#{type.name}.#{constructor.name}", constructor)
            |> put_unique!(constructor.id, constructor)
            |> Map.put("#{ast.module}.#{type.name}.#{constructor.name}", constructor)
          end)
        end)

      {types ++ group_types, group_constructors}
    end)
  end

  defp elaborate_declaration(declaration, ast, headers, visibility) do
    unless Enum.all?(declaration.parameters, &(&1.kind == "Type")) do
      fail("A001", "C002 supports only Type-kinded datatype parameters", declaration.path)
    end

    variables =
      declaration.parameters
      |> Enum.with_index()
      |> Map.new(fn {parameter, index} -> {parameter.name, index} end)

    type_id = nominal_id(ast.origin, ast.module, declaration.name)
    result = {:nominal, type_id, Enum.map(variable_ids(map_size(variables)), &{:var, &1})}

    constructors =
      declaration.constructors
      |> Enum.with_index()
      |> Enum.map(fn {constructor, index} ->
        elaborate_constructor(
          constructor,
          index,
          declaration,
          result,
          variables,
          headers,
          type_id
        )
      end)

    derivations = validate_derivations!(declaration.derivations, constructors, declaration.path)

    %{
      id: type_id,
      origin: ast.origin,
      module: ast.module,
      name: declaration.name,
      arity: length(declaration.parameters),
      parameters: declaration.parameters,
      visibility: visibility,
      constructors: constructors,
      derivations: derivations,
      imported?: false,
      path: declaration.path
    }
  end

  defp elaborate_constructor(
         constructor,
         index,
         declaration,
         default_result,
         variables,
         headers,
         type_id
       ) do
    unless Enum.all?(constructor.existentials, &(&1.kind == "Type")) do
      fail("A001", "C002 supports only Type-kinded existentials", constructor.path)
    end

    existential_variables =
      constructor.existentials
      |> Enum.with_index(map_size(variables))
      |> Map.new(fn {parameter, id} -> {parameter.name, id} end)

    all_variables = Map.merge(variables, existential_variables)

    fields =
      constructor.fields
      |> Enum.with_index()
      |> Enum.map(fn {field, field_index} ->
        {name, encoded_type, path} =
          case constructor.field_style do
            :positional -> {nil, field, "#{constructor.path}.fields[#{field_index}]"}
            :named -> {field.name, field.type, field.path <> ".type"}
          end

        %{
          name: name,
          index: field_index,
          type: Parser.parse(encoded_type, all_variables, path, headers)
        }
      end)

    result =
      case constructor.result do
        nil -> default_result
        encoded -> Parser.parse(encoded, all_variables, constructor.path <> ".result", headers)
      end

    unless match?({:nominal, ^type_id, _}, result) do
      fail(
        "A003",
        "constructor result must return #{declaration.name}",
        constructor.path <> ".result"
      )
    end

    if not MapSet.disjoint?(Type.free(result), MapSet.new(Map.values(existential_variables))) do
      fail(
        "T009",
        "constructor existentials cannot appear in the datatype result",
        constructor.path <> ".result"
      )
    end

    %{
      id: "#{type_id}::#{constructor.name}",
      type_id: type_id,
      type_name: declaration.name,
      name: constructor.name,
      qualified: "#{declaration.name}.#{constructor.name}",
      index: index,
      fields: fields,
      field_style: constructor.field_style,
      variables: all_variables |> Map.values() |> Enum.sort(),
      universal_count: map_size(variables),
      existential_ids: existential_variables |> Map.values() |> MapSet.new(),
      result: result,
      gadt?: not is_nil(constructor.result),
      visibility: :internal,
      path: constructor.path
    }
  end

  defp validate_derivations!(derivations, constructors, path) do
    unknown = Enum.filter(derivations, &(is_binary(&1) and &1 != "fold"))

    if unknown != [],
      do: fail("A001", "unsupported derivations: #{Enum.join(unknown, ", ")}", path)

    if "fold" in derivations and
         Enum.any?(constructors, &(&1.gadt? or MapSet.size(&1.existential_ids) > 0)) do
      fail(
        "A003",
        "fold derivation is not defined for GADTs or existential constructors in C002",
        path
      )
    end

    derivations
  end

  defp annotate_shapes_and_inhabitation(types, all_by_id) do
    local_ids = MapSet.new(types, & &1.id)
    initial = Map.new(types, &{&1.id, :empty})
    statuses = inhabitance_fixed_point(types, all_by_id, initial, local_ids)
    variances = variance_fixed_point(types, all_by_id)

    Enum.map(types, fn type ->
      {positive?, regular?} = shape(type)

      type
      |> Map.put(:positive?, positive?)
      |> Map.put(:regular?, regular?)
      |> Map.put(:variance, Enum.map(Map.fetch!(variances, type.id), &classify_variance/1))
      |> Map.put(:inhabitation, Map.fetch!(statuses, type.id))
      |> Map.update!(:constructors, fn constructors ->
        Enum.map(constructors, &Map.put(&1, :visibility, type.visibility))
      end)
    end)
  end

  defp variance_fixed_point(types, all_by_id) do
    initial = Map.new(types, &{&1.id, List.duplicate(MapSet.new(), &1.arity)})
    do_variance_fixed_point(types, all_by_id, initial)
  end

  defp do_variance_fixed_point(types, all_by_id, current) do
    next =
      Map.new(types, fn type ->
        discovered =
          Enum.reduce(type.constructors, List.duplicate(MapSet.new(), type.arity), fn constructor,
                                                                                      acc ->
            Enum.reduce(constructor.fields, acc, fn field, field_acc ->
              collect_variance(field.type, 1, type.arity, current, all_by_id, field_acc)
            end)
          end)

        accumulated =
          Enum.zip(Map.fetch!(current, type.id), discovered)
          |> Enum.map(fn {old, found} -> MapSet.union(old, found) end)

        {type.id, accumulated}
      end)

    if next == current, do: next, else: do_variance_fixed_point(types, all_by_id, next)
  end

  defp collect_variance({:var, id}, polarity, arity, _current, _all_by_id, acc)
       when is_integer(id) and id < arity do
    List.update_at(acc, id, &MapSet.put(&1, polarity))
  end

  defp collect_variance({:function, parameter, result}, polarity, arity, current, all, acc) do
    acc = collect_variance(parameter, -polarity, arity, current, all, acc)
    collect_variance(result, polarity, arity, current, all, acc)
  end

  defp collect_variance({:tuple, elements}, polarity, arity, current, all, acc),
    do: Enum.reduce(elements, acc, &collect_variance(&1, polarity, arity, current, all, &2))

  defp collect_variance({:nominal, id, arguments}, polarity, arity, current, all, acc) do
    target_variance =
      case Map.get(current, id) do
        nil ->
          Map.get(all, id, %{})
          |> Map.get(:variance, List.duplicate(:invariant, length(arguments)))

        sets ->
          Enum.map(sets, &classify_variance/1)
      end

    Enum.zip(arguments, target_variance)
    |> Enum.reduce(acc, fn
      {_argument, :phantom}, current_acc ->
        current_acc

      {argument, :covariant}, current_acc ->
        collect_variance(argument, polarity, arity, current, all, current_acc)

      {argument, :contravariant}, current_acc ->
        collect_variance(argument, -polarity, arity, current, all, current_acc)

      {argument, :invariant}, current_acc ->
        next_acc = collect_variance(argument, polarity, arity, current, all, current_acc)
        collect_variance(argument, -polarity, arity, current, all, next_acc)
    end)
  end

  defp collect_variance(_type, _polarity, _arity, _current, _all_by_id, acc), do: acc

  defp classify_variance(signs) do
    case {MapSet.member?(signs, 1), MapSet.member?(signs, -1)} do
      {false, false} -> :phantom
      {true, false} -> :covariant
      {false, true} -> :contravariant
      {true, true} -> :invariant
    end
  end

  defp inhabitance_fixed_point(types, all_by_id, statuses, local_ids) do
    next =
      Map.new(types, fn type ->
        constructor_states =
          Enum.map(type.constructors, fn constructor ->
            field_states =
              Enum.map(
                constructor.fields,
                &type_inhabitation(&1.type, statuses, all_by_id, local_ids)
              )

            cond do
              Enum.any?(field_states, &(&1 == :empty)) -> :empty
              Enum.all?(field_states, &(&1 == :inhabited)) -> :inhabited
              true -> :unknown
            end
          end)

        status =
          cond do
            Enum.any?(constructor_states, &(&1 == :inhabited)) -> :inhabited
            Enum.any?(constructor_states, &(&1 == :unknown)) -> :unknown
            true -> :empty
          end

        {type.id, status}
      end)

    if next == statuses,
      do: next,
      else: inhabitance_fixed_point(types, all_by_id, next, local_ids)
  end

  defp type_inhabitation(type, statuses, all_by_id, local_ids) do
    case type do
      value when value in [:integer, :boolean] ->
        :inhabited

      {:var, _} ->
        :unknown

      {:skolem, _} ->
        :unknown

      {:function, _, _} ->
        :inhabited

      {:tuple, elements} ->
        combine_inhabitation(elements, statuses, all_by_id, local_ids)

      {:nominal, id, _arguments} ->
        cond do
          MapSet.member?(local_ids, id) ->
            Map.fetch!(statuses, id)

          Map.get(all_by_id, id, %{})[:visibility] == :transparent ->
            Map.get(all_by_id, id, %{})[:inhabitation] || :unknown

          true ->
            :unknown
        end
    end
  end

  defp combine_inhabitation(types, statuses, all_by_id, local_ids) do
    states = Enum.map(types, &type_inhabitation(&1, statuses, all_by_id, local_ids))

    cond do
      Enum.any?(states, &(&1 == :empty)) -> :empty
      Enum.all?(states, &(&1 == :inhabited)) -> :inhabited
      true -> :unknown
    end
  end

  defp shape(type) do
    Enum.reduce(type.constructors, {true, true}, fn constructor, {positive, regular} ->
      Enum.reduce(constructor.fields, {positive, regular}, fn field, {p, r} ->
        {field_positive, field_regular} = occurrence_shape(field.type, type.id, 1, type.arity)
        {p and field_positive, r and field_regular}
      end)
    end)
  end

  defp occurrence_shape({:function, parameter, result}, id, variance, arity) do
    {pp, pr} = occurrence_shape(parameter, id, -variance, arity)
    {rp, rr} = occurrence_shape(result, id, variance, arity)
    {pp and rp, pr and rr}
  end

  defp occurrence_shape({:tuple, elements}, id, variance, arity),
    do: combine_shape(elements, id, variance, arity)

  defp occurrence_shape({:nominal, id, arguments}, id, variance, arity) do
    regular =
      length(arguments) == arity and
        Enum.with_index(arguments)
        |> Enum.all?(fn {argument, index} -> argument == {:var, index} end)

    {variance > 0, regular}
  end

  defp occurrence_shape({:nominal, _other, arguments}, id, variance, arity),
    do: combine_shape(arguments, id, variance, arity)

  defp occurrence_shape(_type, _id, _variance, _arity), do: {true, true}

  defp combine_shape(types, id, variance, arity) do
    Enum.reduce(types, {true, true}, fn type, {positive, regular} ->
      {p, r} = occurrence_shape(type, id, variance, arity)
      {positive and p, regular and r}
    end)
  end

  defp put_unique!(map, key, value) do
    if Map.has_key?(map, key),
      do: fail("A002", "duplicate constructor identity #{key}", value.path)

    Map.put(map, key, value)
  end

  defp constructor_index(types) do
    Enum.reduce(types, %{}, fn type, index ->
      if not type.imported? or type.visibility in [:transparent, :internal] do
        Enum.reduce(type.constructors, index, fn constructor, current ->
          current
          |> Map.put(constructor.id, constructor)
          |> Map.put("#{type.module}.#{type.name}.#{constructor.name}", constructor)
          |> maybe_put_local(type, constructor)
        end)
      else
        index
      end
    end)
  end

  defp apply_imports(index, imports) do
    Enum.reduce(imports, index, fn import, current ->
      case import do
        %{"kind" => "constructor", "constructor" => reference} when is_binary(reference) ->
          constructor = Map.get(current, reference)

          if is_nil(constructor) do
            fail("A004", "constructor import cannot resolve #{inspect(reference)}", "$.imports")
          end

          alias_name = Map.get(import, "as", constructor.name)

          unless is_binary(alias_name) and Regex.match?(~r/^[A-Z][A-Za-z0-9_]*$/, alias_name) do
            fail("A001", "constructor import alias must be a constructor name", "$.imports")
          end

          put_unique!(current, alias_name, constructor)

        %{"kind" => "condition"} ->
          current

        _ ->
          fail(
            "A001",
            "unsupported import; C002 accepts explicit constructor imports",
            "$.imports"
          )
      end
    end)
  end

  defp maybe_put_local(index, %{imported?: false}, constructor),
    do: Map.put(index, constructor.qualified, constructor)

  defp maybe_put_local(index, _type, _constructor), do: index

  defp variable_ids(0), do: []
  defp variable_ids(count), do: Enum.to_list(0..(count - 1))

  defp nominal_id(origin, module, name), do: "#{origin}::#{module}::#{name}"

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
