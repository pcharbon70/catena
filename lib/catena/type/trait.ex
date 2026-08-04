defmodule Catena.Type.Trait do
  @moduledoc "Kind-aware, terminating Catena 0.1.4 trait registry and compile-time evidence solver."

  alias Catena.{Diagnostic, Kind}

  @default_budget 20_000

  defstruct traits: %{}, instances: [], aliases: %{}, budget: @default_budget

  @type type_term ::
          {:variable, String.t(), Kind.t()}
          | {:constructor, String.t(), Kind.t(), String.t() | nil}
          | {:application, type_term(), type_term()}
          | term()

  @type predicate :: %{trait: String.t(), arguments: [type_term()]}
  @type trait :: %{
          id: String.t(),
          name: String.t(),
          origin: String.t(),
          parameters: [map()],
          parents: [predicate()],
          methods: [map()],
          laws: [map()],
          fundeps: list()
        }
  @type instance :: %{
          id: String.t(),
          trait: String.t(),
          arguments: [type_term()],
          owner: String.t(),
          context: [predicate()],
          methods: map(),
          associated_types: map()
        }
  @type t :: %__MODULE__{
          traits: map(),
          instances: [instance()],
          aliases: map(),
          budget: pos_integer()
        }

  @spec new(keyword()) :: t()
  def new(options \\ []), do: %__MODULE__{budget: Keyword.get(options, :budget, @default_budget)}

  @doc "Legacy-compatible trait declaration used by the C001 conformance surface."
  @spec add_trait(t(), String.t(), pos_integer(), keyword()) :: t()
  def add_trait(registry, name, arity, options \\ []) when is_integer(arity) and arity > 0 do
    parameters =
      Enum.map(0..(arity - 1), fn index ->
        %{name: "p#{index}", kind: :type}
      end)

    declaration = %{
      id: Keyword.get(options, :id, name),
      name: name,
      formal_name: Keyword.get(options, :formal_name),
      origin: Keyword.get(options, :origin, name),
      parameters: parameters,
      parents: Keyword.get(options, :parents, []),
      methods: Keyword.get(options, :methods, []),
      laws: Keyword.get(options, :laws, []),
      fundeps: Keyword.get(options, :fundeps, []),
      legacy?: true,
      path: Keyword.get(options, :path)
    }

    add_trait(registry, declaration)
  end

  @spec add_trait(t(), map()) :: t()
  def add_trait(%__MODULE__{} = registry, declaration) when is_map(declaration) do
    trait = normalize_trait!(declaration)

    if Map.has_key?(registry.traits, trait.id) or Map.has_key?(registry.aliases, trait.name) do
      fail("TRT001", "duplicate trait #{trait.name}", trait.path)
    end

    validate_parent_predicates!(registry, trait)
    validate_fundeps!(trait)

    next = %{
      registry
      | traits: Map.put(registry.traits, trait.id, trait),
        aliases: Map.put(registry.aliases, trait.name, trait.id)
    }

    ensure_acyclic!(next, trait.id, trait.path)
    next
  end

  @spec add_instance(t(), map()) :: t()
  def add_instance(%__MODULE__{} = registry, instance) when is_map(instance) do
    trait = fetch_trait!(registry, Map.fetch!(instance, :trait), Map.get(instance, :path))
    normalized = normalize_instance!(instance, trait)

    validate_predicate!(trait, normalized.arguments, normalized.path)
    validate_instance_owner!(trait, normalized)
    validate_instance_context!(registry, normalized)
    validate_instance_methods!(trait, normalized)

    if Enum.any?(registry.instances, &overlaps?(&1, normalized)) do
      fail(
        instance_diagnostic(trait),
        "overlapping instances are forbidden for #{trait.name}",
        normalized.path
      )
    end

    validate_fundep_consistency!(registry, trait, normalized)
    %{registry | instances: [normalized | registry.instances]}
  end

  @spec resolve(t(), String.t(), [type_term()], keyword()) ::
          {:ok, map()} | {:error, Diagnostic.t()}
  def resolve(registry, trait_name, arguments, options \\ []) do
    budget = Keyword.get(options, :budget, registry.budget)

    try do
      trait = fetch_trait!(registry, trait_name, Keyword.get(options, :path))
      validate_predicate!(trait, arguments, Keyword.get(options, :path))

      {evidence, _remaining, _memo} =
        solve!(registry, trait, arguments, budget, [], %{}, Keyword.get(options, :path))

      {:ok, evidence}
    rescue
      error in Catena.TypeError -> {:error, error.diagnostic}
    end
  end

  @spec resolve!(t(), String.t(), [type_term()], keyword()) :: map()
  def resolve!(registry, trait, arguments, options \\ []) do
    case resolve(registry, trait, arguments, options) do
      {:ok, evidence} -> evidence
      {:error, diagnostic} -> raise Catena.TypeError, diagnostic: diagnostic
    end
  end

  @spec associated_type(t(), String.t(), [type_term()], String.t()) ::
          {:ok, term()} | {:error, Diagnostic.t()}
  def associated_type(registry, trait_name, arguments, name) do
    with {:ok, evidence} <- resolve(registry, trait_name, arguments),
         {:ok, type} <- Map.fetch(evidence.associated_types, name) do
      {:ok, substitute(type, evidence.substitution)}
    else
      :error -> {:error, Diagnostic.new("TRT004", "associated type #{name} is not defined")}
      {:error, _} = error -> error
    end
  end

  @spec trait(t(), String.t()) :: trait() | nil
  def trait(registry, name) do
    id = Map.get(registry.aliases, name, name)
    Map.get(registry.traits, id)
  end

  @spec public_traits(t()) :: [trait()]
  def public_traits(registry), do: registry.traits |> Map.values() |> Enum.sort_by(& &1.id)

  @spec public_instances(t()) :: [instance()]
  def public_instances(registry), do: Enum.sort_by(registry.instances, & &1.id)

  @spec term_kind!(type_term(), String.t() | nil) :: Kind.t()
  def term_kind!({:variable, _name, kind}, _path), do: kind
  def term_kind!({:constructor, _id, kind, _owner}, _path), do: kind

  def term_kind!({:application, function, argument}, path) do
    Kind.apply!(term_kind!(function, path), term_kind!(argument, path), path)
  end

  def term_kind!({owner, name}, _path) when is_binary(owner) and is_binary(name), do: :type

  def term_kind!({owner, name, _arguments}, _path) when is_binary(owner) and is_binary(name),
    do: :type

  def term_kind!(type, _path) when type in [:integer, :boolean], do: :type
  def term_kind!(_term, _path), do: :type

  defp solve!(registry, trait, arguments, remaining, stack, memo, path) do
    if remaining <= 0 do
      fail("TRT008", "trait resolution exceeded the deterministic budget", path)
    end

    key = {trait.id, canonical(arguments)}

    cond do
      Map.has_key?(memo, key) ->
        {Map.fetch!(memo, key), remaining, memo}

      key in stack ->
        fail("TRT004", "recursive trait resolution for #{trait.name}", path)

      true ->
        matches = matching_instances(registry, trait.id, arguments)

        case matches do
          [] ->
            fail("TRT004", "no instance for #{trait.name} #{inspect(arguments)}", path)

          [{instance, substitution}] ->
            {context_evidence, remaining, memo} =
              solve_predicates!(
                registry,
                instance.context,
                substitution,
                remaining - 1,
                [key | stack],
                memo,
                instance.path
              )

            parent_substitution =
              trait.parameters
              |> Enum.zip(arguments)
              |> Map.new(fn {parameter, argument} -> {parameter.name, argument} end)

            parent_predicates =
              Enum.map(trait.parents, &substitute_predicate(&1, parent_substitution))

            {parent_evidence, remaining, memo} =
              solve_predicates!(
                registry,
                parent_predicates,
                %{},
                remaining,
                [key | stack],
                memo,
                instance.path
              )

            payload = %{
              trait: trait.id,
              trait_name: trait.name,
              arguments: arguments,
              instance_id: instance.id,
              substitution: substitution,
              methods: instance.methods,
              associated_types: instance.associated_types,
              context: context_evidence,
              parents: parent_evidence,
              law_status: instance.law_status,
              derivation: instance.derivation
            }

            evidence = Map.put(payload, :digest, digest(payload))
            {evidence, remaining, Map.put(memo, key, evidence)}

          _ ->
            fail("TRT004", "incoherent instances for #{trait.name} #{inspect(arguments)}", path)
        end
    end
  end

  defp solve_predicates!(registry, predicates, substitution, remaining, stack, memo, path) do
    Enum.reduce(predicates, {[], remaining, memo}, fn predicate, {evidence, budget, current} ->
      trait = fetch_trait!(registry, predicate.trait, path)
      arguments = Enum.map(predicate.arguments, &substitute(&1, substitution))
      {found, budget, current} = solve!(registry, trait, arguments, budget, stack, current, path)
      {[found | evidence], budget, current}
    end)
    |> then(fn {evidence, budget, memo} -> {Enum.reverse(evidence), budget, memo} end)
  end

  defp matching_instances(registry, trait_id, arguments) do
    registry.instances
    |> Enum.filter(&(&1.trait == trait_id))
    |> Enum.flat_map(fn instance ->
      case match_terms(instance.arguments, arguments, %{}) do
        {:ok, substitution} -> [{instance, substitution}]
        :error -> []
      end
    end)
  end

  defp normalize_trait!(declaration) do
    parameters = Map.fetch!(declaration, :parameters)

    unless is_list(parameters) and parameters != [] and
             Enum.all?(parameters, &(is_binary(&1.name) and valid_kind?(&1.kind))) and
             unique?(parameters, & &1.name) do
      fail("TRT001", "trait parameters must have unique names and valid kinds", declaration.path)
    end

    methods = Map.get(declaration, :methods, [])
    laws = Map.get(declaration, :laws, [])

    unless unique?(methods, & &1.name),
      do: fail("TRT001", "trait methods must be unique", declaration.path)

    unless unique?(laws, & &1.id),
      do: fail("TRT005", "trait law identifiers must be unique", declaration.path)

    id = Map.get(declaration, :id, declaration.name)

    %{
      id: id,
      name: Map.fetch!(declaration, :name),
      formal_name: Map.get(declaration, :formal_name),
      origin: Map.get(declaration, :origin, id),
      parameters: parameters,
      parents: Enum.map(Map.get(declaration, :parents, []), &normalize_predicate/1),
      methods: methods,
      laws: laws,
      fundeps: Map.get(declaration, :fundeps, []),
      legacy?: Map.get(declaration, :legacy?, false),
      path: Map.get(declaration, :path)
    }
  end

  defp normalize_instance!(instance, trait) do
    arguments = Map.fetch!(instance, :arguments)
    id = Map.get(instance, :id) || instance_id(trait.id, arguments, Map.get(instance, :owner))

    %{
      id: id,
      trait: trait.id,
      arguments: arguments,
      owner: Map.fetch!(instance, :owner),
      context: Enum.map(Map.get(instance, :context, []), &normalize_predicate/1),
      methods: Map.get(instance, :methods, %{}),
      associated_types: Map.get(instance, :associated_types, %{}),
      law_status: Map.get(instance, :law_status, :promised),
      derivation: Map.get(instance, :derivation),
      path: Map.get(instance, :path)
    }
  end

  defp normalize_predicate(%{trait: trait, arguments: arguments}),
    do: %{trait: trait, arguments: arguments}

  defp normalize_predicate({trait, arguments}) when is_list(arguments),
    do: %{trait: trait, arguments: arguments}

  defp validate_parent_predicates!(registry, trait) do
    variables = Map.new(trait.parameters, &{&1.name, &1.kind})

    Enum.each(trait.parents, fn parent ->
      parent_trait = fetch_trait!(registry, parent.trait, trait.path)
      validate_predicate!(parent_trait, parent.arguments, trait.path)

      Enum.each(parent.arguments, fn argument ->
        validate_declared_variables!(argument, variables, trait.path)
      end)
    end)
  end

  defp validate_predicate!(trait, arguments, path) do
    if length(arguments) != length(trait.parameters) do
      fail("TRT002", "wrong argument count for #{trait.name}", path)
    end

    Enum.zip(arguments, trait.parameters)
    |> Enum.each(fn {argument, parameter} ->
      actual = term_kind!(argument, path)

      if actual != parameter.kind do
        fail(
          "TRT002",
          "#{trait.name}.#{parameter.name} expects #{Kind.encode(parameter.kind)}, got #{Kind.encode(actual)}",
          path
        )
      end
    end)
  end

  defp validate_instance_owner!(trait, instance) do
    head_owners = instance.arguments |> Enum.map(&constructor_owner/1) |> Enum.reject(&is_nil/1)

    unless instance.owner == trait.origin or instance.owner in head_owners or
             instance.owner == trait.id do
      fail(instance_diagnostic(trait), "instance violates trait-or-type ownership", instance.path)
    end
  end

  defp validate_instance_context!(registry, instance) do
    head_size = terms_size(instance.arguments)

    Enum.each(instance.context, fn predicate ->
      trait = fetch_trait!(registry, predicate.trait, instance.path)
      validate_predicate!(trait, predicate.arguments, instance.path)

      if terms_size(predicate.arguments) >= head_size do
        fail(
          instance_diagnostic(trait),
          "instance context does not structurally decrease",
          instance.path
        )
      end
    end)
  end

  defp validate_instance_methods!(trait, instance) do
    required = MapSet.new(trait.methods, & &1.name)
    supplied = instance.methods |> Map.keys() |> MapSet.new()

    if required != supplied do
      missing = MapSet.difference(required, supplied) |> Enum.sort()
      extra = MapSet.difference(supplied, required) |> Enum.sort()

      fail(
        "TRT001",
        "instance methods differ from the minimal set; missing=#{inspect(missing)} extra=#{inspect(extra)}",
        instance.path
      )
    end

    unless instance.law_status in [:promised, :tested, :derived] do
      fail(
        "TRT005",
        "Catena 0.1.4 law status must be promised, tested, or derived",
        instance.path
      )
    end
  end

  defp validate_fundeps!(trait) do
    arity = length(trait.parameters)

    Enum.each(trait.fundeps, fn {inputs, outputs} ->
      unless Enum.all?(inputs ++ outputs, &(&1 in 0..(arity - 1))) do
        fail("TRT002", "functional dependency index is outside trait arity", trait.path)
      end
    end)
  end

  defp validate_fundep_consistency!(registry, trait, instance) do
    Enum.each(trait.fundeps, fn {inputs, outputs} ->
      Enum.each(Enum.filter(registry.instances, &(&1.trait == trait.id)), fn existing ->
        if positions_unify?(existing.arguments, instance.arguments, inputs) and
             not positions_unify?(existing.arguments, instance.arguments, outputs) do
          fail(
            instance_diagnostic(trait),
            "functional dependency consistency is violated",
            instance.path
          )
        end
      end)
    end)
  end

  defp instance_diagnostic(%{legacy?: true}), do: "T007"
  defp instance_diagnostic(_trait), do: "TRT003"

  defp ensure_acyclic!(registry, start, path) do
    visit = fn visit, id, visiting, visited ->
      cond do
        MapSet.member?(visiting, id) ->
          fail("TRT002", "trait parent cycle", path)

        MapSet.member?(visited, id) ->
          visited

        true ->
          trait = Map.fetch!(registry.traits, id)
          visiting = MapSet.put(visiting, id)

          visited =
            Enum.reduce(trait.parents, visited, fn parent, current ->
              parent_id = Map.get(registry.aliases, parent.trait, parent.trait)
              visit.(visit, parent_id, visiting, current)
            end)

          MapSet.put(visited, id)
      end
    end

    visit.(visit, start, MapSet.new(), MapSet.new())
    :ok
  end

  defp validate_declared_variables!({:variable, name, kind}, variables, path) do
    if Map.get(variables, name) != kind,
      do: fail("TRT002", "undeclared or wrongly kinded constructor variable #{name}", path)
  end

  defp validate_declared_variables!({:application, function, argument}, variables, path) do
    validate_declared_variables!(function, variables, path)
    validate_declared_variables!(argument, variables, path)
  end

  defp validate_declared_variables!(_term, _variables, _path), do: :ok

  defp fetch_trait!(registry, name, path) do
    case trait(registry, name) do
      nil -> fail("TRT001", "unknown trait #{name}", path)
      found -> found
    end
  end

  defp overlaps?(left, right) do
    left.trait == right.trait and match_terms(left.arguments, right.arguments, %{}) != :error
  end

  defp match_terms(patterns, actuals, substitution) when length(patterns) == length(actuals) do
    Enum.zip(patterns, actuals)
    |> Enum.reduce_while({:ok, substitution}, fn {pattern, actual}, {:ok, current} ->
      case match_term(pattern, actual, current) do
        {:ok, next} -> {:cont, {:ok, next}}
        :error -> {:halt, :error}
      end
    end)
  end

  defp match_terms(_patterns, _actuals, _substitution), do: :error

  defp match_term({:variable, name, _kind}, actual, substitution) do
    case Map.fetch(substitution, name) do
      {:ok, ^actual} -> {:ok, substitution}
      {:ok, _other} -> :error
      :error -> {:ok, Map.put(substitution, name, actual)}
    end
  end

  defp match_term({:application, pf, pa}, {:application, af, aa}, substitution) do
    with {:ok, substitution} <- match_term(pf, af, substitution),
         do: match_term(pa, aa, substitution)
  end

  defp match_term(pattern, actual, substitution) when pattern == actual, do: {:ok, substitution}
  defp match_term(_pattern, _actual, _substitution), do: :error

  defp substitute({:variable, name, _kind} = variable, substitution),
    do: Map.get(substitution, name, variable)

  defp substitute({:application, function, argument}, substitution),
    do: {:application, substitute(function, substitution), substitute(argument, substitution)}

  defp substitute(value, _substitution), do: value

  defp substitute_predicate(predicate, substitution),
    do: %{predicate | arguments: Enum.map(predicate.arguments, &substitute(&1, substitution))}

  defp positions_unify?(left, right, positions) do
    left = Enum.map(positions, &Enum.at(left, &1))
    right = Enum.map(positions, &Enum.at(right, &1))
    match_terms(left, right, %{}) != :error
  end

  defp instance_id(trait, arguments, owner),
    do: "#{owner || "unknown"}##{trait}[#{digest(canonical(arguments))}]"

  defp constructor_owner({:constructor, _id, _kind, owner}), do: owner
  defp constructor_owner({:application, function, _argument}), do: constructor_owner(function)
  defp constructor_owner({owner, _name}) when is_binary(owner), do: owner
  defp constructor_owner({owner, _name, _arguments}) when is_binary(owner), do: owner
  defp constructor_owner(_), do: nil

  defp terms_size(terms), do: 1 + Enum.sum(Enum.map(terms, &term_size/1))
  defp term_size({:variable, _name, _kind}), do: 1
  defp term_size({:constructor, _id, _kind, _owner}), do: 2

  defp term_size({:application, function, argument}),
    do: 1 + term_size(function) + term_size(argument)

  defp term_size(term) when is_list(term), do: 1 + Enum.sum(Enum.map(term, &term_size/1))
  defp term_size(term) when is_tuple(term), do: term |> Tuple.to_list() |> term_size()
  defp term_size(_term), do: 1

  defp canonical(term), do: :erlang.term_to_binary(term, [:deterministic])

  defp digest(term),
    do: :crypto.hash(:sha256, canonical(term)) |> Base.encode16(case: :lower)

  defp unique?(values, function), do: length(values) == length(Enum.uniq_by(values, function))
  defp valid_kind?(:type), do: true
  defp valid_kind?({:arrow, left, right}), do: valid_kind?(left) and valid_kind?(right)
  defp valid_kind?(_), do: false

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
