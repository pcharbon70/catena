defmodule Catena.Effect do
  @moduledoc "Elaboration and lexical selection for Catena 0.1.5 effects and handlers."

  alias Catena.Effect.Row
  alias Catena.Type.Parser
  alias Catena.{Diagnostic, LanguageVersion, Type}

  @effect_name ~r/^[A-Z][A-Za-z0-9_]*$/
  @value_name ~r/^[a-z][A-Za-z0-9_]*$/
  @effect_versions LanguageVersion.compilable_from(:effects_and_handlers)

  @spec prepare!(map(), map(), [map()]) :: map()
  def prepare!(%{frontend_version: version}, _data, _interfaces)
      when version not in @effect_versions do
    %{families: %{}, handlers: %{}, exported_families: [], exported_handlers: []}
  end

  def prepare!(ast, data, interfaces) do
    imported = imported_families(interfaces)
    imported_handlers = imported_handlers(interfaces)

    local =
      ast.effects
      |> Enum.map(&decode_family!(&1, ast, data))
      |> unique_map!(:name, "EFX001", "effect family names must be unique")

    collisions = Map.keys(local) -- (Map.keys(local) -- Map.keys(imported))

    if collisions != [] do
      fail(
        "EFX001",
        "local effect families conflict with imported names: #{Enum.join(collisions, ", ")}",
        "$.effects"
      )
    end

    families = Map.merge(imported, local)

    local_handlers =
      ast.handlers
      |> Enum.map(&decode_handler!(&1, ast, data, families))
      |> unique_map!(:name, "EFX006", "handler names must be unique")

    handler_collisions =
      Map.keys(local_handlers) -- (Map.keys(local_handlers) -- Map.keys(imported_handlers))

    if handler_collisions != [] do
      fail(
        "EFX006",
        "local handlers conflict with imported names: #{Enum.join(handler_collisions, ", ")}",
        "$.handlers"
      )
    end

    handlers = Map.merge(imported_handlers, local_handlers)

    Enum.each(local_handlers, fn {_name, handler} ->
      family = Enum.find(Map.values(families), &(&1.id == handler.family))

      if handler.visibility == :public and family.visibility != :public do
        fail("EFX006", "a public handler requires a public effect family", handler.path)
      end
    end)

    %{
      families: families,
      handlers: handlers,
      exported_families: local |> Map.values() |> Enum.filter(&(&1.visibility == :public)),
      exported_handlers:
        local_handlers |> Map.values() |> Enum.filter(&(&1.visibility == :public))
    }
  end

  @spec uses!(map(), map() | nil, map(), String.t(), String.t()) :: %{
          capabilities: [map()],
          row: Row.t()
        }
  def uses!(_effects, nil, _data, _definition_name, _path),
    do: %{capabilities: [], row: Row.empty()}

  def uses!(effects, signature, data, definition_name, path) when is_map(signature) do
    values = Map.get(signature, "uses", [])
    tail = Map.get(signature, "uses_tail")

    unless is_list(values) and (is_nil(tail) or is_binary(tail)) do
      fail(
        "EFX003",
        "uses must be a list with an optional row-tail name",
        path <> ".signature.uses"
      )
    end

    variables = signature_variables(signature, path <> ".signature")

    capabilities =
      values
      |> Enum.with_index()
      |> Enum.map(fn {value, index} ->
        use_path = "#{path}.signature.uses[#{index}]"
        reference = decode_reference!(value, effects.families, variables, data, use_path)
        name = optional_value_name!(Map.get(value, "capability"), use_path <> ".capability")

        Map.merge(reference, %{
          capability: "uses://#{definition_name}/#{index}",
          name: name,
          abstract?: true,
          path: use_path
        })
      end)

    duplicate_names =
      capabilities
      |> Enum.reject(&is_nil(&1.name))
      |> Enum.group_by(& &1.name)
      |> Enum.filter(fn {_name, entries} -> length(entries) > 1 end)

    if duplicate_names != [] do
      fail("EFX003", "uses capability names must be unique", path <> ".signature.uses")
    end

    %{capabilities: capabilities, row: Row.new(capabilities, tail)}
  end

  @spec resolve_request!(map(), map(), [map()]) :: {map(), map()}
  def resolve_request!(effects, expression, capabilities) do
    family =
      Map.get(effects.families, expression.effect) ||
        fail("EFX001", "unknown effect family #{expression.effect}", expression.path)

    operation =
      Map.get(family.operations, expression.operation) ||
        fail(
          "EFX001",
          "unknown operation #{expression.effect}.#{expression.operation}",
          expression.path
        )

    candidates =
      Enum.filter(capabilities, fn capability ->
        capability.family == family.id and length(capability.arguments) == family.arity
      end)

    capability =
      case expression.capability do
        nil -> unique_capability!(candidates, family, expression)
        name -> named_capability!(capabilities, name, family, expression)
      end

    replacements =
      family.parameter_ids
      |> Enum.zip(capability.arguments)
      |> Map.new()

    instantiated = %{
      operation
      | parameters: Enum.map(operation.parameters, &substitute_parameters(&1, replacements)),
        result: substitute_parameters(operation.result, replacements)
    }

    {capability, instantiated}
  end

  @spec handler!(map(), String.t(), String.t()) :: map()
  def handler!(effects, name, path) do
    Map.get(effects.handlers, name) || fail("EFX006", "unknown handler #{name}", path)
  end

  @spec substitute_parameters(Type.t(), map()) :: Type.t()
  def substitute_parameters({:var, id} = variable, replacements),
    do: Map.get(replacements, id, variable)

  def substitute_parameters({:function, parameter, result}, replacements),
    do:
      {:function, substitute_parameters(parameter, replacements),
       substitute_parameters(result, replacements)}

  def substitute_parameters({:tuple, elements}, replacements),
    do: {:tuple, Enum.map(elements, &substitute_parameters(&1, replacements))}

  def substitute_parameters({:nominal, id, arguments}, replacements),
    do: {:nominal, id, Enum.map(arguments, &substitute_parameters(&1, replacements))}

  def substitute_parameters(type, _replacements), do: type

  defp decode_family!(value, ast, data) do
    path = value.path
    name = required_name!(Map.get(value, "name"), @effect_name, "effect", path)
    parameters = Map.get(value, "parameters", [])
    operations = Map.get(value, "operations")
    visibility = visibility!(Map.get(value, "visibility", "private"), path)

    unless is_list(parameters) and
             Enum.all?(parameters, &(is_binary(&1) and Regex.match?(@value_name, &1))) and
             parameters == Enum.uniq(parameters) do
      fail(
        "EFX001",
        "effect parameters must be unique type-variable names",
        path <> ".parameters"
      )
    end

    unless is_list(operations) and operations != [] do
      fail("EFX001", "an effect requires at least one operation", path <> ".operations")
    end

    variables = parameters |> Enum.with_index() |> Map.new()

    decoded_operations =
      operations
      |> Enum.with_index()
      |> Enum.map(fn {operation, index} ->
        decode_operation!(operation, variables, data, "#{path}.operations[#{index}]")
      end)
      |> unique_map!(:name, "EFX001", "operation names must be unique")

    %{
      id: "#{ast.origin}::effect::#{name}",
      origin: ast.origin,
      module: ast.module,
      name: name,
      parameters: parameters,
      parameter_ids: Enum.to_list(0..length(parameters)//1) |> Enum.take(length(parameters)),
      arity: length(parameters),
      operations: decoded_operations,
      visibility: visibility,
      path: path
    }
  end

  defp decode_operation!(value, variables, data, path) when is_map(value) do
    name = required_name!(Map.get(value, "name"), @value_name, "operation", path)
    parameters = Map.get(value, "parameters", [])
    result = Map.get(value, "result")

    unless is_list(parameters) and is_map(result) do
      fail("EFX001", "operation parameters and result are required", path)
    end

    decoded_parameters =
      parameters
      |> Enum.with_index()
      |> Enum.map(fn {parameter, index} ->
        parameter_path = "#{path}.parameters[#{index}]"

        unless is_map(parameter) do
          fail("EFX001", "operation parameters must be named typed entries", parameter_path)
        end

        parameter_name =
          required_name!(Map.get(parameter, "name"), @value_name, "parameter", parameter_path)

        type_value = Map.get(parameter, "type")
        assert_first_order_type!(type_value, parameter_path <> ".type")

        %{
          name: parameter_name,
          type: Parser.parse(type_value, variables, parameter_path <> ".type", data.types_by_name)
        }
      end)

    if Enum.map(decoded_parameters, & &1.name) !=
         Enum.uniq(Enum.map(decoded_parameters, & &1.name)) do
      fail("EFX001", "operation parameter names must be unique", path <> ".parameters")
    end

    assert_first_order_type!(result, path <> ".result")

    %{
      name: name,
      parameters: Enum.map(decoded_parameters, & &1.type),
      parameter_names: Enum.map(decoded_parameters, & &1.name),
      result: Parser.parse(result, variables, path <> ".result", data.types_by_name),
      path: path
    }
  end

  defp decode_operation!(_value, _variables, _data, path),
    do: fail("EFX001", "operation declaration must be an object", path)

  defp decode_handler!(value, ast, data, families) do
    path = value.path
    forall = Map.get(value, "forall", [])

    unless is_list(forall) and Enum.all?(forall, &is_binary/1) and forall == Enum.uniq(forall) do
      fail("EFX006", "handler forall must contain unique variables", path <> ".forall")
    end

    variables = forall |> Enum.with_index() |> Map.new()

    reference =
      decode_reference!(
        %{"effect" => value.effect, "arguments" => Map.get(value, "arguments", [])},
        families,
        variables,
        data,
        path
      )

    family = Map.fetch!(families, value.effect)
    visibility = visibility!(Map.get(value, "visibility", "private"), path)
    input = Parser.parse(Map.get(value, "input"), variables, path <> ".input", data.types_by_name)

    output =
      Parser.parse(Map.get(value, "output"), variables, path <> ".output", data.types_by_name)

    parameters =
      Enum.map(value.parameters, fn parameter ->
        Map.put(
          parameter,
          :parsed_type,
          Parser.parse(parameter.type, variables, parameter.path <> ".type", data.types_by_name)
        )
      end)

    clauses = value.operation_clauses

    handler_uses =
      uses!(
        %{families: families},
        %{
          "forall" => forall,
          "uses" => Map.get(value, "uses", []),
          "uses_tail" => Map.get(value, "uses_tail")
        },
        data,
        "handler::#{value.name}",
        path
      )

    unless Enum.map(clauses, & &1.operation) == Enum.uniq(Enum.map(clauses, & &1.operation)) and
             Enum.sort(Enum.map(clauses, & &1.operation)) ==
               Enum.sort(Map.keys(family.operations)) do
      fail("EFX006", "handler must contain exactly one clause for every operation", path)
    end

    Enum.each(clauses, fn clause ->
      operation = Map.fetch!(family.operations, clause.operation)

      if length(clause.parameters) != length(operation.parameters) do
        fail("EFX006", "handler operation clause has the wrong arity", clause.path)
      end

      assert_affine_surface!(clause.body, clause.resumption, clause.path)
    end)

    %{
      id: "#{ast.origin}::handler::#{value.name}",
      origin: ast.origin,
      module: ast.module,
      name: value.name,
      family: family.id,
      family_name: family.name,
      arguments: reference.arguments,
      variables: Enum.to_list(0..length(forall)//1) |> Enum.take(length(forall)),
      input: input,
      output: output,
      parameters: parameters,
      return_clause: value.return_clause,
      operation_clauses: clauses,
      visibility: visibility,
      uses_raw: Map.get(value, "uses", []),
      uses_row: handler_uses.row,
      uses_capabilities: handler_uses.capabilities,
      path: path
    }
  end

  defp decode_reference!(value, families, variables, data, path) when is_map(value) do
    name = Map.get(value, "effect")

    family =
      Map.get(families, name) || fail("EFX001", "unknown effect family #{inspect(name)}", path)

    arguments = Map.get(value, "arguments", [])

    unless is_list(arguments) and length(arguments) == family.arity do
      fail("EFX001", "effect family #{family.name} has the wrong number of arguments", path)
    end

    %{
      family: family.id,
      family_name: family.name,
      arguments:
        arguments
        |> Enum.with_index()
        |> Enum.map(fn {argument, index} ->
          Parser.parse(argument, variables, "#{path}.arguments[#{index}]", data.types_by_name)
        end)
    }
  end

  defp unique_capability!([], family, expression),
    do:
      fail(
        "EFX004",
        "no lexical capability can serve #{family.name}.#{expression.operation}",
        expression.path
      )

  defp unique_capability!([capability], _family, _expression), do: capability

  defp unique_capability!(candidates, family, expression) do
    names = Enum.map_join(candidates, ", ", &(Map.get(&1, :name) || &1.capability))

    fail(
      "EFX005",
      "ambiguous #{family.name}.#{expression.operation}; qualify one of: #{names}",
      expression.path
    )
  end

  defp named_capability!(capabilities, name, family, expression) do
    case Enum.filter(capabilities, &(&1.name == name)) do
      [%{family: family_id} = capability] when family_id == family.id ->
        capability

      [_other] ->
        fail("EFX004", "capability #{name} has the wrong effect family", expression.path)

      [] ->
        fail("EFX004", "unknown lexical capability #{name}", expression.path)

      _ ->
        fail("EFX005", "capability name #{name} is ambiguous", expression.path)
    end
  end

  defp signature_variables(signature, path) do
    variables = Map.get(signature, "forall", [])

    unless is_list(variables) and Enum.all?(variables, &is_binary/1) do
      fail("EFX003", "signature forall must be a list", path <> ".forall")
    end

    variables |> Enum.with_index() |> Map.new()
  end

  defp assert_first_order_type!(%{"tag" => "function"} = value, path) do
    if Map.get(value, "effect", []) != [] do
      fail("EFX002", "operation function types must have a closed empty effect row", path)
    end

    assert_first_order_type!(Map.get(value, "parameter"), path <> ".parameter")
    assert_first_order_type!(Map.get(value, "result"), path <> ".result")
  end

  defp assert_first_order_type!(%{"tag" => "tuple", "elements" => elements}, path)
       when is_list(elements) do
    Enum.with_index(elements)
    |> Enum.each(fn {element, index} ->
      assert_first_order_type!(element, "#{path}.elements[#{index}]")
    end)
  end

  defp assert_first_order_type!(%{"tag" => "named", "arguments" => arguments}, path)
       when is_list(arguments) do
    Enum.with_index(arguments)
    |> Enum.each(fn {argument, index} ->
      assert_first_order_type!(argument, "#{path}.arguments[#{index}]")
    end)
  end

  defp assert_first_order_type!(%{"tag" => tag}, _path)
       when tag in ["integer", "boolean", "variable"],
       do: :ok

  defp assert_first_order_type!(_value, path),
    do: fail("EFX001", "malformed operation type", path)

  defp assert_affine_surface!(expression, resumption, path) do
    {uses, escaped?} = resumption_uses(expression, resumption, false)

    cond do
      escaped? -> fail("RES001", "resumption #{resumption} escapes its clause", path)
      uses > 1 -> fail("RES002", "resumption #{resumption} is used more than once", path)
      true -> :ok
    end
  end

  defp resumption_uses(%{tag: :resume, resumption: name, value: value}, name, nested?),
    do: add_use(resumption_uses(value, name, nested?), nested?)

  defp resumption_uses(%{tag: :variable, name: name}, name, _nested?), do: {0, true}

  defp resumption_uses(%{tag: :function, body: body}, name, _nested?),
    do: resumption_uses(body, name, true)

  defp resumption_uses(%{tag: :match, scrutinee: scrutinee, clauses: clauses}, name, nested?) do
    scrutinee_evidence = resumption_uses(scrutinee, name, nested?)

    branch_evidence =
      clauses
      |> Enum.map(fn clause ->
        merge_use(
          resumption_uses(Map.get(clause, :guard), name, nested?),
          resumption_uses(clause.body, name, nested?)
        )
      end)
      |> Enum.reduce({0, false}, fn {uses, escaped?}, {maximum, any_escaped?} ->
        {max(maximum, uses), any_escaped? or escaped?}
      end)

    merge_use(scrutinee_evidence, branch_evidence)
  end

  defp resumption_uses(%{} = expression, name, nested?) do
    expression
    |> Map.drop([:path, :tag])
    |> Map.values()
    |> Enum.reduce({0, false}, fn value, acc ->
      merge_use(acc, resumption_uses(value, name, nested?))
    end)
  end

  defp resumption_uses(values, name, nested?) when is_list(values) do
    Enum.reduce(values, {0, false}, fn value, acc ->
      merge_use(acc, resumption_uses(value, name, nested?))
    end)
  end

  defp resumption_uses(_value, _name, _nested?), do: {0, false}
  defp add_use({uses, escaped?}, nested?), do: {uses + 1, escaped? or nested?}

  defp merge_use({left, escaped_left}, {right, escaped_right}),
    do: {left + right, escaped_left or escaped_right}

  defp visibility!("public", _path), do: :public
  defp visibility!("private", _path), do: :private

  defp visibility!(_visibility, path),
    do: fail("EFX001", "visibility must be public or private", path)

  defp required_name!(name, regex, role, path) when is_binary(name) do
    if Regex.match?(regex, name), do: name, else: fail("EFX001", "invalid #{role} name", path)
  end

  defp required_name!(_name, _regex, role, path), do: fail("EFX001", "missing #{role} name", path)

  defp optional_value_name!(nil, _path), do: nil

  defp optional_value_name!(name, path) when is_binary(name) do
    if Regex.match?(@value_name, name),
      do: name,
      else: fail("EFX003", "invalid capability name", path)
  end

  defp optional_value_name!(_name, path), do: fail("EFX003", "invalid capability name", path)

  defp unique_map!(values, key, id, message) do
    names = Enum.map(values, &Map.fetch!(&1, key))

    if names == Enum.uniq(names),
      do: Map.new(values, &{Map.fetch!(&1, key), &1}),
      else: fail(id, message, nil)
  end

  defp imported_families(interfaces) do
    families = Enum.flat_map(interfaces, &Map.get(&1, :effects, []))
    names = Enum.map(families, & &1.name)

    if names == Enum.uniq(names) do
      Map.new(families, &{&1.name, &1})
    else
      fail("EFX001", "imported effect family names are ambiguous", "$.interfaces")
    end
  end

  defp imported_handlers(interfaces) do
    handlers = Enum.flat_map(interfaces, &Map.get(&1, :handlers, []))
    names = Enum.map(handlers, & &1.name)

    if names == Enum.uniq(names) do
      Map.new(handlers, &{&1.name, &1})
    else
      fail("EFX006", "imported handler names are ambiguous", "$.interfaces")
    end
  end

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
