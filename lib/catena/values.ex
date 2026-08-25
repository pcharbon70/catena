defmodule Catena.Values do
  @moduledoc """
  Value classification at 0.1.25: the closed ten-form value grammar
  over typed-core expression forms and kernel runtime values.

  The classifier is total over decodable input and implements the
  normative grammar of the values-and-evaluation area: integers,
  Booleans, Unit, Floats, tuples of values, closures, constructor
  values, records of values, variant injections, and opaque process
  handles are values; evidence, handler declarations, capability
  names, resumptions, traps, effect rows, signatures, and every
  unevaluated computation are not.
  """

  alias Catena.Runtime.ResumptionToken
  alias Catena.{Effect.Row, Type.Scheme}

  @value_tags ~w(integer boolean unit float function)a
  @container_tags ~w(tuple record construct inject)a

  @computation_tags ~w(
    variable call let sequence annotate unary binary match trait_call
    select update extend restrict spawn send receive self
  )a

  @effect_machinery_tags ~w(request handle resume)a

  @spec value?(term()) :: boolean()
  def value?(term), do: classify(term) == true

  @doc """
  Classifies one term as `true` (a value) or a non-value reason:
  one of the closed non-value kinds, `{:computation, tag}` for an
  unevaluated expression form, or `:unknown_form` for a decodable
  shape outside every known carrier.
  """
  @spec classify(term()) :: true | atom() | {atom(), atom()}
  def classify(term)

  # Typed-core expression forms: literal and closure-forming tags are
  # values; containers are values exactly when recursive; everything
  # else is a computation or effect machinery.

  def classify(%{tag: tag} = expression) when tag in @value_tags do
    case tag do
      :integer -> if is_integer(expression.value), do: true, else: :unknown_form
      :boolean -> if is_boolean(expression.value), do: true, else: :unknown_form
      :float -> if is_float(expression.value), do: true, else: :unknown_form
      _other -> true
    end
  end

  def classify(%{tag: tag} = expression) when tag in @container_tags do
    contents =
      case tag do
        :tuple -> Map.get(expression, :elements, [])
        :record -> Enum.map(Map.get(expression, :fields, []), & &1.expression)
        :construct -> Map.get(expression, :arguments, [])
        :inject -> [Map.get(expression, :payload)]
      end

    if Enum.all?(contents, &value?/1), do: true, else: {:computation, tag}
  end

  def classify(%{tag: tag}) when tag in @computation_tags, do: {:computation, tag}

  def classify(%{tag: tag}) when tag in @effect_machinery_tags do
    case tag do
      :handle -> :handler_declaration
      :resume -> :resumption
      :request -> :capability_name
    end
  end

  def classify(%{tag: :trap}), do: :trap

  # Kernel runtime values: the shapes the stepper places in value
  # control, plus the Float the kernel grammar predates.

  def classify(term) when is_integer(term), do: true
  def classify(term) when is_boolean(term), do: true
  def classify(term) when is_float(term), do: true
  def classify(:unit), do: true

  def classify({:trap, _reason, _result}), do: :trap
  def classify({:trap, _reason}), do: :trap

  def classify({:closure, parameter, _body, environment})
      when is_binary(parameter) and is_map(environment),
      do: true

  def classify({:catena_variant, label, payload}) when is_atom(label) do
    if value?(payload), do: true, else: :unknown_form
  end

  def classify({:catena_constructor, constructor, fields})
      when is_atom(constructor) and is_tuple(fields) do
    if Enum.all?(Tuple.to_list(fields), &value?/1), do: true, else: :unknown_form
  end

  def classify({:catena_process, id}) when is_binary(id), do: true

  def classify(term) when is_tuple(term) do
    if Enum.all?(Tuple.to_list(term), &value?/1), do: true, else: :unknown_form
  end

  def classify(term) when is_map(term) do
    cond do
      struct?(term, Row) -> :effect_row
      struct?(term, Scheme) -> :signature
      struct?(term, ResumptionToken) -> :resumption
      is_map_key(term, :evidence) or is_map_key(term, "evidence") -> :evidence
      term == %{} or Enum.all?(Map.values(term), &value?/1) -> true
      true -> :unknown_form
    end
  end

  def classify(_other), do: :unknown_form

  @doc """
  The closed non-value kinds of the normative grammar, for exclusion
  evidence: a form in this list is never a value at any revision that
  keeps this grammar.
  """
  @spec non_value_kinds() :: [atom()]
  def non_value_kinds,
    do: ~w(evidence handler_declaration capability_name resumption trap effect_row signature)a

  @doc """
  Every terminal the kernel stepper produces carries a value or a
  trap; this predicate witnesses the terminal-outcome rule over
  stepper results.
  """
  @spec terminal_witness({:ok, term(), map()} | {:trap, term(), map()} | term()) ::
          {:value, term()} | {:trap, term()}
  def terminal_witness({:ok, value, _outcome}), do: {:value, value}
  def terminal_witness({:trap, reason, _outcome}), do: {:trap, reason}

  def terminal_witness({:value, value}), do: {:value, value}

  defp struct?(term, module), do: is_struct(term, module)
end
