defmodule Catena.Condition do
  @moduledoc "Clause-condition safety checking, normalization, and portable evidence."

  alias Catena.{CanonicalJSON, Diagnostic}
  alias Catena.Type.{Parser, Scheme}

  @default_budget 20_000
  @binary_operators ~w(and or equal not_equal less less_equal greater greater_equal add subtract multiply)a
  @unary_operators ~w(not negate)a

  @spec prepare!(map(), map(), keyword()) :: map()
  def prepare!(ast, data, options \\ []) do
    budget = Keyword.get(options, :condition_budget, @default_budget)
    interfaces = Keyword.get(options, :interfaces, [])
    imported = imported_conditions!(ast.imports, interfaces)

    local_headers =
      ast.definitions
      |> Enum.filter(&(&1.kind == :condition))
      |> Map.new(fn definition ->
        record = local_header!(definition, ast, data)
        {definition.name, record}
      end)

    aliases =
      local_headers
      |> Map.new(fn {name, record} -> {name, record.id} end)
      |> Map.merge(Map.new(imported, fn {name, record} -> {name, record.id} end))

    arities =
      local_headers
      |> Map.values()
      |> Kernel.++(Map.values(imported))
      |> Map.new(&{&1.id, length(&1.parameters)})

    locals =
      Map.new(local_headers, fn {name, record} ->
        {core, dependencies} =
          normalize!(record.body, MapSet.new(record.parameters), aliases, arities, record.path)

        ensure_budget!(core, budget, record.path)
        evidence = evidence(record.id, record.parameters, core, dependencies)
        {name, Map.merge(record, evidence)}
      end)

    by_id =
      locals
      |> Map.values()
      |> Kernel.++(Map.values(imported))
      |> Map.new(&{&1.id, &1})

    assert_acyclic!(locals, by_id)

    expanded_by_id =
      Enum.reduce(Map.keys(by_id), %{}, fn id, cache ->
        {_expanded, cache} = expand_id!(id, by_id, cache, budget)
        cache
      end)

    locals =
      Map.new(locals, fn {name, record} ->
        expanded = Map.fetch!(expanded_by_id, record.id)
        {name, Map.put(record, :expanded_core, expanded)}
      end)

    imported =
      Map.new(imported, fn {name, record} ->
        expanded = Map.get(expanded_by_id, record.id, record.expanded_core)
        {name, Map.put(record, :expanded_core, expanded)}
      end)

    by_id =
      locals
      |> Map.values()
      |> Kernel.++(Map.values(imported))
      |> Map.new(&{&1.id, &1})

    %{
      aliases: aliases,
      arities: arities,
      locals: locals,
      imported: imported,
      by_id: by_id,
      budget: budget,
      schemes:
        locals
        |> Map.merge(imported)
        |> Map.new(fn {alias_name, record} -> {alias_name, record.scheme} end)
    }
  end

  @spec guard!(map(), map(), String.t()) :: map()
  def guard!(typed_guard, catalog, path) do
    {core, dependencies} =
      normalize!(typed_guard, :all, catalog.aliases, catalog.arities, path)

    ensure_budget!(core, catalog.budget, path)
    expanded = bounded_expand!(core, catalog.by_id, catalog.budget, path)

    payload = %{
      version: "0.3",
      core: core,
      expanded_core: expanded,
      dependencies: Enum.sort(dependencies),
      native: native?(expanded)
    }

    Map.put(payload, :digest, digest(payload))
  end

  @spec definition_evidence(map(), String.t()) :: map() | nil
  def definition_evidence(catalog, name) do
    case Map.get(catalog.locals, name) do
      nil ->
        nil

      record ->
        payload = %{
          version: "0.3",
          id: record.id,
          parameters: record.parameters,
          core: record.expanded_core,
          expanded_core: record.expanded_core,
          dependencies: record.dependencies,
          native: true
        }

        Map.put(payload, :digest, digest(payload))
    end
  end

  @spec encode_evidence(map()) :: map()
  def encode_evidence(evidence), do: stringify(evidence)

  @spec decode_evidence(map(), String.t()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode_evidence(value, path \\ "$.values[].condition") do
    try do
      evidence = atomize_evidence(value)
      digest_value = Map.fetch!(evidence, :digest)
      payload = Map.delete(evidence, :digest)

      cond do
        evidence.version != "0.3" ->
          condition_error("unsupported condition evidence version", path)

        not is_binary(evidence.id) ->
          condition_error("condition evidence requires an identity", path)

        digest(payload) != digest_value ->
          condition_error("condition evidence digest is invalid", path)

        evidence.core != evidence.expanded_core ->
          condition_error("condition evidence body is not fully normalized", path)

        not native?(evidence.core) or not native?(evidence.expanded_core) ->
          condition_error("condition evidence is not portable", path)

        true ->
          {:ok, evidence}
      end
    rescue
      _error -> condition_error("malformed condition evidence", path)
    end
  end

  @spec native?(map()) :: boolean()
  def native?(%{tag: tag}) when tag in [:integer, :boolean, :variable], do: true

  def native?(%{tag: :unary, operator: operator, operand: operand}),
    do: operator in @unary_operators and native?(operand)

  def native?(%{tag: :binary, operator: operator, left: left, right: right}),
    do: operator in @binary_operators and native?(left) and native?(right)

  def native?(_core), do: false

  @spec valid_evidence?(map(), :guard | :definition) :: boolean()
  def valid_evidence?(evidence, kind) when is_map(evidence) do
    digest_value = Map.get(evidence, :digest)
    payload = Map.delete(evidence, :digest)

    body_valid? =
      case kind do
        :guard ->
          native?(Map.get(evidence, :expanded_core))

        :definition ->
          Map.get(evidence, :core) == Map.get(evidence, :expanded_core) and
            native?(Map.get(evidence, :core))
      end

    is_binary(digest_value) and digest(payload) == digest_value and
      Map.get(evidence, :version) == "0.3" and body_valid? and
      (kind == :guard or
         (is_binary(Map.get(evidence, :id)) and is_list(Map.get(evidence, :parameters))))
  end

  def valid_evidence?(_evidence, _kind), do: false

  @spec valid_for_scheme?(map(), Scheme.t()) :: boolean()
  def valid_for_scheme?(evidence, %Scheme{} = scheme) do
    {parameter_types, result_type} = split_type(scheme.type)

    scheme.variables == [] and result_type == :boolean and
      length(parameter_types) == length(evidence.parameters) and
      Enum.all?(parameter_types, &(&1 in [:integer, :boolean])) and
      core_type(evidence.core, Map.new(Enum.zip(evidence.parameters, parameter_types))) ==
        :boolean
  rescue
    _error -> false
  end

  @spec node_count(map()) :: pos_integer()
  def node_count(%{tag: :unary, operand: operand}), do: 1 + node_count(operand)

  def node_count(%{tag: :binary, left: left, right: right}),
    do: 1 + node_count(left) + node_count(right)

  def node_count(%{tag: :call, arguments: arguments}),
    do: 1 + Enum.sum(Enum.map(arguments, &node_count/1))

  def node_count(%{tag: _tag}), do: 1

  defp local_header!(definition, ast, data) do
    if is_nil(definition.signature) do
      fail(
        "CND001",
        "condition #{definition.name} requires an explicit signature",
        definition.path
      )
    end

    unless empty_effects?(definition.signature) do
      fail(
        "CND002",
        "condition signatures must have an empty effect",
        definition.path <> ".signature"
      )
    end

    scheme =
      Parser.parse_scheme(
        definition.signature,
        definition.path <> ".signature",
        data.types_by_name
      )

    {parameters, result} = split_type(scheme.type)

    cond do
      scheme.variables != [] ->
        fail("CND002", "conditions must be monomorphic and first-order", definition.path)

      length(parameters) != length(definition.parameters) ->
        fail("CND002", "condition signature arity does not match its parameters", definition.path)

      result != :boolean ->
        fail("CND002", "condition signatures must return Bool", definition.path)

      Enum.any?(parameters, &(&1 not in [:integer, :boolean])) ->
        fail("CND002", "condition parameters must be Int or Bool", definition.path)

      true ->
        %{
          id: "#{ast.module}.#{definition.name}",
          name: definition.name,
          parameters: definition.parameters,
          parameter_types: parameters,
          scheme: scheme,
          body: definition.body,
          path: definition.path,
          imported?: false
        }
    end
  rescue
    error in Catena.TypeError ->
      case error.diagnostic.id do
        "T010" ->
          fail("CND002", "condition signatures must have an empty effect", definition.path)

        "T012" ->
          fail("CND001", "condition signature is malformed", definition.path <> ".signature")

        _other ->
          reraise(error, __STACKTRACE__)
      end
  end

  defp imported_conditions!(imports, interfaces) do
    values =
      interfaces
      |> Enum.flat_map(fn interface ->
        Enum.map(Map.get(interface, :values, []), &{{interface.module, &1.name}, &1})
      end)
      |> Map.new()

    imports
    |> Enum.filter(&(Map.get(&1, "kind") == "condition"))
    |> Enum.reduce(%{}, fn import, imported ->
      reference = Map.get(import, "condition") || Map.get(import, "value")
      alias_name = Map.get(import, "as")

      with true <- is_binary(reference) and is_binary(alias_name),
           [module, name] <- String.split(reference, ".", parts: 2),
           %{condition: evidence, scheme: scheme} <- Map.get(values, {module, name}),
           true <- is_map(evidence) do
        if Map.has_key?(imported, alias_name) do
          fail("CND005", "duplicate condition import alias #{alias_name}", "$.imports")
        end

        record = %{
          id: evidence.id,
          name: name,
          parameters: evidence.parameters,
          scheme: scheme,
          core: evidence.core,
          expanded_core: evidence.expanded_core,
          dependencies: evidence.dependencies,
          digest: evidence.digest,
          path: "interface://#{reference}",
          imported?: true
        }

        Map.put(imported, alias_name, record)
      else
        _ -> fail("CND005", "condition import cannot resolve verified evidence", "$.imports")
      end
    end)
  end

  defp evidence(id, parameters, core, dependencies) do
    payload = %{
      version: "0.3",
      id: id,
      parameters: parameters,
      core: core,
      dependencies: Enum.sort(dependencies),
      native: true
    }

    Map.put(payload, :digest, digest(payload))
  end

  defp normalize!(expression, allowed_variables, aliases, arities, path) do
    {core, dependencies} = do_normalize!(expression, allowed_variables, aliases, arities, path)
    {core, dependencies |> MapSet.to_list() |> Enum.sort()}
  end

  defp do_normalize!(%{tag: tag, value: value}, _allowed, _aliases, _arities, _path)
       when tag in [:integer, :boolean],
       do: {%{tag: tag, value: value}, MapSet.new()}

  defp do_normalize!(%{tag: :variable, name: name}, allowed, _aliases, _arities, path) do
    if allowed == :all or MapSet.member?(allowed, name),
      do: {%{tag: :variable, name: name}, MapSet.new()},
      else: fail("CND003", "condition references non-parameter value #{name}", path)
  end

  defp do_normalize!(
         %{tag: :unary, operator: operator, operand: operand},
         allowed,
         aliases,
         arities,
         path
       )
       when operator in @unary_operators do
    {operand, dependencies} = do_normalize!(operand, allowed, aliases, arities, path)
    {%{tag: :unary, operator: operator, operand: operand}, dependencies}
  end

  defp do_normalize!(
         %{tag: :binary, operator: operator, left: left, right: right},
         allowed,
         aliases,
         arities,
         path
       )
       when operator in @binary_operators do
    {left, left_dependencies} = do_normalize!(left, allowed, aliases, arities, path)
    {right, right_dependencies} = do_normalize!(right, allowed, aliases, arities, path)

    {%{tag: :binary, operator: operator, left: left, right: right},
     MapSet.union(left_dependencies, right_dependencies)}
  end

  defp do_normalize!(
         %{tag: :call, callee: %{tag: :variable, name: name}, arguments: arguments},
         allowed,
         aliases,
         arities,
         path
       ) do
    with {:ok, id} <- Map.fetch(aliases, name),
         true <- Map.fetch!(arities, id) == length(arguments) do
      {arguments, dependencies} =
        Enum.map_reduce(arguments, MapSet.new([id]), fn argument, dependencies ->
          {argument, nested} = do_normalize!(argument, allowed, aliases, arities, path)
          {argument, MapSet.union(dependencies, nested)}
        end)

      {%{tag: :call, target: id, arguments: arguments}, dependencies}
    else
      :error -> fail("CND003", "guards may call only declared condition predicates", path)
      false -> fail("CND003", "condition calls must be direct and fully applied", path)
    end
  end

  defp do_normalize!(%{tag: :unary}, _allowed, _aliases, _arities, path),
    do: fail("CND001", "malformed or unsupported unary condition operator", path)

  defp do_normalize!(%{tag: :binary}, _allowed, _aliases, _arities, path),
    do: fail("CND001", "malformed or unsupported binary condition operator", path)

  defp do_normalize!(_expression, _allowed, _aliases, _arities, path) do
    fail(
      "CND003",
      "conditions exclude ordinary calls, lambdas, local matches, construction, effects, and partial operations",
      path
    )
  end

  defp assert_acyclic!(locals, by_id) do
    local_ids = MapSet.new(Map.values(locals), & &1.id)

    Enum.reduce(MapSet.to_list(local_ids), {MapSet.new(), MapSet.new()}, fn id, {done, _active} ->
      {done, _active} = visit!(id, by_id, local_ids, done, MapSet.new())
      {done, MapSet.new()}
    end)

    :ok
  end

  defp visit!(id, by_id, local_ids, done, active) do
    cond do
      MapSet.member?(done, id) ->
        {done, active}

      MapSet.member?(active, id) ->
        fail("CND004", "cyclic condition dependency involving #{id}", nil)

      true ->
        active = MapSet.put(active, id)
        record = Map.fetch!(by_id, id)

        {done, _active} =
          Enum.reduce(record.dependencies, {done, active}, fn dependency,
                                                              {current_done, current_active} ->
            if MapSet.member?(local_ids, dependency) do
              visit!(dependency, by_id, local_ids, current_done, current_active)
            else
              {current_done, current_active}
            end
          end)

        {MapSet.put(done, id), MapSet.delete(active, id)}
    end
  end

  defp expand_id!(id, by_id, cache, budget) do
    case Map.fetch(cache, id) do
      {:ok, expanded} ->
        {expanded, cache}

      :error ->
        record = Map.fetch!(by_id, id)

        expanded =
          if record.imported? do
            record.expanded_core
          else
            bounded_expand!(record.core, by_id, budget, record.path)
          end

        ensure_budget!(expanded, budget, record.path)
        {expanded, Map.put(cache, id, expanded)}
    end
  end

  defp bounded_expand!(core, by_id, budget, path) do
    if budget < @default_budget do
      ensure_budget!(core, budget, path)
    end

    {expanded, _remaining} = do_expand!(core, by_id, budget, path)
    expanded
  end

  defp do_expand!(_core, _by_id, remaining, path) when remaining <= 0 do
    fail(
      "CND007",
      "condition normalization exceeded its deterministic safety budget",
      path,
      minimum_budget: @default_budget
    )
  end

  defp do_expand!(%{tag: :call, target: target, arguments: arguments}, by_id, remaining, path) do
    record = Map.fetch!(by_id, target)

    body =
      if record.imported? do
        record.expanded_core
      else
        record.core
      end

    substitutions = Map.new(Enum.zip(record.parameters, arguments))
    body |> substitute(substitutions) |> do_expand!(by_id, remaining, path)
  end

  defp do_expand!(%{tag: :unary, operand: operand} = core, by_id, remaining, path) do
    {operand, remaining} = do_expand!(operand, by_id, remaining - 1, path)
    {%{core | operand: operand}, remaining}
  end

  defp do_expand!(%{tag: :binary, left: left, right: right} = core, by_id, remaining, path) do
    {left, remaining} = do_expand!(left, by_id, remaining - 1, path)
    {right, remaining} = do_expand!(right, by_id, remaining, path)
    {%{core | left: left, right: right}, remaining}
  end

  defp do_expand!(core, _by_id, remaining, _path), do: {core, remaining - 1}

  defp substitute(%{tag: :variable, name: name} = core, substitutions),
    do: Map.get(substitutions, name, core)

  defp substitute(%{tag: :unary, operand: operand} = core, substitutions),
    do: %{core | operand: substitute(operand, substitutions)}

  defp substitute(%{tag: :binary, left: left, right: right} = core, substitutions),
    do: %{core | left: substitute(left, substitutions), right: substitute(right, substitutions)}

  defp substitute(%{tag: :call, arguments: arguments} = core, substitutions),
    do: %{core | arguments: Enum.map(arguments, &substitute(&1, substitutions))}

  defp substitute(core, _substitutions), do: core

  defp split_type({:function, parameter, result}) do
    {parameters, result} = split_type(result)
    {[parameter | parameters], result}
  end

  defp split_type(result), do: {[], result}

  defp core_type(%{tag: :integer, value: value}, _environment) when is_integer(value),
    do: :integer

  defp core_type(%{tag: :boolean, value: value}, _environment) when is_boolean(value),
    do: :boolean

  defp core_type(%{tag: :variable, name: name}, environment), do: Map.fetch!(environment, name)

  defp core_type(%{tag: :unary, operator: :not, operand: operand}, environment) do
    if core_type(operand, environment) == :boolean, do: :boolean, else: :invalid
  end

  defp core_type(%{tag: :unary, operator: :negate, operand: operand}, environment) do
    if core_type(operand, environment) == :integer, do: :integer, else: :invalid
  end

  defp core_type(
         %{tag: :binary, operator: operator, left: left, right: right},
         environment
       ) do
    left_type = core_type(left, environment)
    right_type = core_type(right, environment)

    cond do
      operator in [:and, :or] and left_type == :boolean and right_type == :boolean ->
        :boolean

      operator in [:equal, :not_equal] and left_type == right_type and
          left_type in [:integer, :boolean] ->
        :boolean

      operator in [:less, :less_equal, :greater, :greater_equal] and left_type == :integer and
          right_type == :integer ->
        :boolean

      operator in [:add, :subtract, :multiply] and left_type == :integer and
          right_type == :integer ->
        :integer

      true ->
        :invalid
    end
  end

  defp core_type(_core, _environment), do: :invalid

  defp empty_effects?(value) when is_map(value) do
    Map.get(value, "effect", []) == [] and Enum.all?(Map.values(value), &empty_effects?/1)
  end

  defp empty_effects?(value) when is_list(value), do: Enum.all?(value, &empty_effects?/1)
  defp empty_effects?(_value), do: true

  defp ensure_budget!(core, budget, path) do
    if budget < @default_budget or node_count(core) > budget do
      fail(
        "CND007",
        "condition normalization exceeded its deterministic safety budget",
        path,
        minimum_budget: @default_budget
      )
    end
  end

  defp digest(payload),
    do: :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)

  defp stringify(value) when is_map(value),
    do: Map.new(value, fn {key, item} -> {to_string(key), stringify(item)} end)

  defp stringify(value) when is_list(value), do: Enum.map(value, &stringify/1)
  defp stringify(value) when value in [true, false, nil], do: value
  defp stringify(value) when is_atom(value), do: Atom.to_string(value)
  defp stringify(value), do: value

  defp atomize_evidence(value) when is_map(value) do
    Map.new(value, fn {key, item} ->
      atom_key =
        case key do
          "version" -> :version
          "id" -> :id
          "parameters" -> :parameters
          "core" -> :core
          "expanded_core" -> :expanded_core
          "dependencies" -> :dependencies
          "digest" -> :digest
          "native" -> :native
          "tag" -> :tag
          "value" -> :value
          "name" -> :name
          "operator" -> :operator
          "operand" -> :operand
          "left" -> :left
          "right" -> :right
          "target" -> :target
          "arguments" -> :arguments
          other -> other
        end

      value = atomize_evidence(item)

      value =
        if atom_key in [:tag, :operator] and is_binary(value),
          do: evidence_atom(value),
          else: value

      {atom_key, value}
    end)
  end

  defp atomize_evidence(value) when is_list(value), do: Enum.map(value, &atomize_evidence/1)
  defp atomize_evidence(value), do: value

  defp evidence_atom("integer"), do: :integer
  defp evidence_atom("boolean"), do: :boolean
  defp evidence_atom("variable"), do: :variable
  defp evidence_atom("unary"), do: :unary
  defp evidence_atom("binary"), do: :binary
  defp evidence_atom("call"), do: :call
  defp evidence_atom("not"), do: :not
  defp evidence_atom("negate"), do: :negate
  defp evidence_atom("and"), do: :and
  defp evidence_atom("or"), do: :or
  defp evidence_atom("equal"), do: :equal
  defp evidence_atom("not_equal"), do: :not_equal
  defp evidence_atom("less"), do: :less
  defp evidence_atom("less_equal"), do: :less_equal
  defp evidence_atom("greater"), do: :greater
  defp evidence_atom("greater_equal"), do: :greater_equal
  defp evidence_atom("add"), do: :add
  defp evidence_atom("subtract"), do: :subtract
  defp evidence_atom("multiply"), do: :multiply
  defp evidence_atom(_value), do: raise(ArgumentError, "unknown condition evidence atom")

  defp condition_error(message, path),
    do: {:error, Diagnostic.new("CND005", message, path: path)}

  defp fail(id, message, path, details \\ []) do
    raise Catena.TypeError,
      diagnostic: Diagnostic.new(id, message, path: path, details: Map.new(details))
  end
end
