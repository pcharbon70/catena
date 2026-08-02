defmodule Catena.Type.Infer do
  @moduledoc "Algorithm W for the executable Catena 0.1 principal-core subset."

  alias Catena.{Diagnostic, Type}
  alias Catena.Type.{Parser, Scheme, Unify}

  @type state :: %{next: non_neg_integer(), substitution: map()}

  @spec module(map()) :: map()
  def module(ast) do
    missing =
      MapSet.difference(MapSet.new(ast.exports), MapSet.new(Enum.map(ast.definitions, & &1.name)))

    if MapSet.size(missing) > 0 do
      fail("T001", "exports have no definitions: #{Enum.join(missing, ", ")}", "$.exports")
    end

    initial = %{next: 10_000, substitution: %{}}

    {definitions, environment, state} =
      Enum.reduce(ast.definitions, {[], %{}, initial}, fn definition,
                                                          {definitions, environment, state} ->
        if definition.name in ast.exports and is_nil(definition.signature) do
          fail("T008", "exported value #{definition.name} requires a signature", definition.path)
        end

        expression =
          Enum.reduce(Enum.reverse(definition.parameters), definition.body, fn parameter, body ->
            %{tag: :function, parameter: parameter, body: body, path: definition.path}
          end)

        {typed, inferred_type, state} = infer(expression, environment, state)
        inferred_type = Type.apply(inferred_type, state.substitution)

        {scheme, state} =
          case definition.signature do
            nil ->
              {Type.generalize(environment, inferred_type, state.substitution), state}

            signature ->
              declared = Parser.parse_scheme(signature, definition.path <> ".signature")
              {expected, state} = skolemize(declared, state)

              substitution =
                Unify.unify(inferred_type, expected, state.substitution, definition.path)

              {declared, %{state | substitution: substitution}}
          end

        typed = Catena.TypedCore.apply_substitution(typed, state.substitution)

        core_definition = %{
          name: definition.name,
          parameters: definition.parameters,
          expression: typed,
          scheme: scheme,
          path: definition.path
        }

        {[core_definition | definitions], Map.put(environment, definition.name, scheme), state}
      end)

    %{
      version: ast.version,
      module: ast.module,
      exports: ast.exports,
      definitions: Enum.reverse(definitions),
      environment: environment,
      profile: :principal_core,
      next: state.next
    }
  end

  @spec infer(map(), map(), state()) :: {map(), Type.t(), state()}
  def infer(%{tag: :integer} = expression, _environment, state),
    do: {Map.put(expression, :type, :integer), :integer, state}

  def infer(%{tag: :boolean} = expression, _environment, state),
    do: {Map.put(expression, :type, :boolean), :boolean, state}

  def infer(%{tag: :variable, name: name, path: path} = expression, environment, state) do
    case Map.fetch(environment, name) do
      {:ok, scheme} ->
        {type, state} = instantiate(scheme, state)
        {Map.put(expression, :type, type), type, state}

      :error ->
        fail("T001", "unbound value #{name}", path)
    end
  end

  def infer(%{tag: :function, parameter: parameter, body: body} = expression, environment, state) do
    {parameter_type, state} = fresh(state)
    local_environment = Map.put(environment, parameter, Scheme.mono(parameter_type))
    {typed_body, body_type, state} = infer(body, local_environment, state)
    type = {:function, Type.apply(parameter_type, state.substitution), body_type}
    {expression |> Map.put(:body, typed_body) |> Map.put(:type, type), type, state}
  end

  def infer(
        %{tag: :call, callee: callee, arguments: arguments, path: path} = expression,
        environment,
        state
      ) do
    {typed_callee, callee_type, state} = infer(callee, environment, state)

    {typed_arguments, result_type, state} =
      Enum.reduce(arguments, {[], callee_type, state}, fn argument,
                                                          {typed_arguments, current_callee, state} ->
        {typed_argument, argument_type, state} = infer(argument, environment, state)
        {result_type, state} = fresh(state)

        substitution =
          Unify.unify(
            current_callee,
            {:function, argument_type, result_type},
            state.substitution,
            path
          )

        {[typed_argument | typed_arguments], Type.apply(result_type, substitution),
         %{state | substitution: substitution}}
      end)

    typed =
      expression
      |> Map.put(:callee, typed_callee)
      |> Map.put(:arguments, Enum.reverse(typed_arguments))
      |> Map.put(:type, result_type)

    {typed, result_type, state}
  end

  def infer(%{tag: :let, name: name, value: value, body: body} = expression, environment, state) do
    {typed_value, value_type, state} = infer(value, environment, state)
    scheme = Type.generalize(environment, value_type, state.substitution)
    {typed_body, body_type, state} = infer(body, Map.put(environment, name, scheme), state)

    typed =
      expression
      |> Map.put(:value, typed_value)
      |> Map.put(:body, typed_body)
      |> Map.put(:scheme, scheme)
      |> Map.put(:type, body_type)

    {typed, body_type, state}
  end

  def infer(%{tag: :tuple, elements: elements} = expression, environment, state) do
    {typed_elements, types, state} =
      Enum.reduce(elements, {[], [], state}, fn element, {typed, types, state} ->
        {typed_element, type, state} = infer(element, environment, state)
        {[typed_element | typed], [type | types], state}
      end)

    type = {:tuple, Enum.reverse(types)}

    {expression |> Map.put(:elements, Enum.reverse(typed_elements)) |> Map.put(:type, type), type,
     state}
  end

  def infer(
        %{tag: :annotate, expression: annotated, signature: signature, path: path} = expression,
        environment,
        state
      ) do
    declared = Parser.parse_scheme(signature, path <> ".signature")
    {expected, state} = skolemize(declared, state)
    {typed, inferred, state} = infer(annotated, environment, state)
    substitution = Unify.unify(inferred, expected, state.substitution, path)
    type = Type.apply(expected, substitution)

    {expression |> Map.put(:expression, typed) |> Map.put(:type, type), type,
     %{state | substitution: substitution}}
  end

  @spec instantiate(Scheme.t(), state()) :: {Type.t(), state()}
  def instantiate(%Scheme{variables: variables, type: type}, state) do
    {replacements, state} =
      Enum.reduce(variables, {%{}, state}, fn variable, {replacements, state} ->
        {fresh, state} = fresh(state)
        {Map.put(replacements, variable, fresh), state}
      end)

    {Type.apply(type, replacements), state}
  end

  defp skolemize(%Scheme{variables: variables, type: type}, state) do
    replacements =
      variables
      |> Enum.with_index(state.next)
      |> Map.new(fn {variable, id} -> {variable, {:skolem, id}} end)

    {Type.apply(type, replacements), %{state | next: state.next + length(variables)}}
  end

  defp fresh(state), do: {{:var, state.next}, %{state | next: state.next + 1}}

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
