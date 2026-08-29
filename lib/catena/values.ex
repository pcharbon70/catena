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
    contents = container_contents(expression)

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
      is_struct(term, Catena.Text.Meaning) -> true
      is_map_key(term, :evidence) or is_map_key(term, "evidence") -> :evidence
      term == %{} or Enum.all?(Map.values(term), &value?/1) -> true
      true -> :unknown_form
    end
  end

  def classify(term) when is_binary(term), do: true

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
  True when the term belongs to the closed comparable set at 0.1.30:
  Int, Bool, and Float primitives, plus structural recursion over
  tuples, records, variant injections, and constructor values. Closures
  and process handles are never comparable; neither are Unit,
  computations, traps, effect rows, signatures, evidence, and every
  other non-value or excluded form.
  """
  @spec comparable?(term()) :: boolean()
  def comparable?(term)

  def comparable?(%{tag: tag} = expression) when tag in @value_tags do
    case tag do
      :integer -> true
      :boolean -> true
      :float -> is_float(expression.value)
      _other -> false
    end
  end

  def comparable?(%{tag: tag} = expression) when tag in @container_tags,
    do: container_contents(expression) |> Enum.all?(&comparable?/1)

  def comparable?(%{tag: _}), do: false

  def comparable?(term) when is_integer(term), do: true
  def comparable?(term) when is_boolean(term), do: true
  def comparable?(term) when is_float(term), do: true
  def comparable?(term) when is_binary(term), do: true
  def comparable?(term) when is_struct(term, Catena.Text.Meaning), do: true

  def comparable?({:catena_variant, _label, payload}), do: comparable?(payload)

  def comparable?({:catena_constructor, _name, fields})
      when is_tuple(fields),
      do: fields |> Tuple.to_list() |> Enum.all?(&comparable?/1)

  def comparable?(term) when is_tuple(term),
    do: term |> Tuple.to_list() |> Enum.all?(&comparable?/1)

  def comparable?(term) when is_map(term),
    do: term |> Map.values() |> Enum.all?(&comparable?/1)

  def comparable?(_other), do: false

  @doc """
  True when the term belongs to the closed orderable set at 0.1.30:
  Int and Float only, in both the typed-core and runtime carriers.
  """
  @spec orderable?(term()) :: boolean()
  def orderable?(%{tag: :integer}), do: true
  def orderable?(%{tag: :float, value: value}), do: is_float(value)
  def orderable?(term) when is_integer(term), do: true
  def orderable?(term) when is_float(term), do: true
  def orderable?(term) when is_binary(term), do: true
  def orderable?(term) when is_struct(term, Catena.Text.Meaning), do: true
  def orderable?(_other), do: false

  @doc """
  The normative total order over one orderable kind: `:lt`, `:eq`, or
  `:gt`. Floats order bit-exactly — the two signed zeros are distinct
  with `-0.0 < 0.0` — and no NaN exists. Raises `ArgumentError` for
  mixed kinds or non-orderable terms: comparison is monomorphic by
  contract.
  """
  @spec compare(term(), term()) :: :lt | :eq | :gt
  def compare(a, b)

  def compare(a, b) when is_integer(a) and is_integer(b) do
    cond do
      a < b -> :lt
      a > b -> :gt
      true -> :eq
    end
  end

  def compare(a, b) when is_float(a) and is_float(b) do
    cond do
      bit_exact_equal?(a, b) -> :eq
      total_order_less?(a, b) -> :lt
      true -> :gt
    end
  end

  def compare(a, b) when is_binary(a) and is_binary(b) do
    cond do
      a == b -> :eq
      a < b -> :lt
      true -> :gt
    end
  end

  def compare(a, b),
    do: raise(ArgumentError, "comparison is monomorphic: #{inspect(a)} vs #{inspect(b)}")

  defp bit_exact_equal?(a, b), do: a === b

  defp total_order_less?(a, b), do: a < b or (a === -0.0 and b === 0.0)

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

  defp container_contents(%{tag: :tuple} = expression),
    do: Map.get(expression, :elements, [])

  defp container_contents(%{tag: :record} = expression),
    do: expression |> Map.get(:fields, []) |> Enum.map(& &1.expression)

  defp container_contents(%{tag: :construct} = expression),
    do: Map.get(expression, :arguments, [])

  defp container_contents(%{tag: :inject} = expression),
    do: [Map.get(expression, :payload)]
end
