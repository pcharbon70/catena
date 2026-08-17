defmodule Catena.Condition.Facts do
  @moduledoc "Deterministic Boolean and integer difference-constraint reasoning for coverage."

  alias Catena.ImplementationLimits

  @default_budget ImplementationLimits.configured(:condition_fact_nodes)
  @branch_budget ImplementationLimits.configured(:condition_fact_branch_steps)

  @spec guard_formula(map() | nil, map()) :: map()
  def guard_formula(nil, _pattern), do: %{tag: :boolean, value: true}

  def guard_formula(%{condition_evidence: %{expanded_core: core}}, pattern),
    do: rename_pattern_bindings(core, pattern)

  def guard_formula(%{tag: :boolean, value: value}, _pattern),
    do: %{tag: :boolean, value: value}

  def guard_formula(_guard, _pattern), do: %{tag: :unknown}

  @spec satisfiable?(map(), keyword()) :: true | false | :unknown
  def satisfiable?(formula, options \\ []) do
    budget = Keyword.get(options, :fact_budget, @default_budget)

    if budget < @default_budget or count(formula) > budget do
      :unknown
    else
      case dnf(formula, true) do
        :unknown -> :unknown
        branches -> Enum.any?(branches, &branch_satisfiable?/1)
      end
    end
  end

  @spec tautology?(map(), keyword()) :: true | false | :unknown
  def tautology?(formula, options \\ []) do
    case satisfiable?(not_formula(formula), options) do
      false -> true
      true -> false
      :unknown -> :unknown
    end
  end

  @spec disjoin([map()]) :: map()
  def disjoin([]), do: %{tag: :boolean, value: false}
  def disjoin([formula]), do: formula
  def disjoin([head | tail]), do: %{tag: :binary, operator: :or, left: head, right: disjoin(tail)}

  @spec conjoin(map(), map()) :: map()
  def conjoin(left, right), do: %{tag: :binary, operator: :and, left: left, right: right}

  @spec negate(map()) :: map()
  def negate(formula), do: not_formula(formula)

  @spec root_pattern?(map()) :: boolean()
  def root_pattern?(%{tag: tag}) when tag in [:wildcard, :bind], do: true
  def root_pattern?(%{tag: :as, pattern: pattern}), do: root_pattern?(pattern)
  def root_pattern?(_pattern), do: false

  defp rename_pattern_bindings(core, pattern) do
    names = root_binding_names(pattern)

    replace_variables(core, fn name ->
      if MapSet.member?(names, name), do: "$scrutinee", else: name
    end)
  end

  defp root_binding_names(%{tag: :bind, name: name}), do: MapSet.new([name])

  defp root_binding_names(%{tag: :as, pattern: pattern, name: name}),
    do: MapSet.put(root_binding_names(pattern), name)

  defp root_binding_names(_pattern), do: MapSet.new()

  defp replace_variables(%{tag: :variable, name: name} = core, rename),
    do: %{core | name: rename.(name)}

  defp replace_variables(%{tag: :unary, operand: operand} = core, rename),
    do: %{core | operand: replace_variables(operand, rename)}

  defp replace_variables(%{tag: :binary, left: left, right: right} = core, rename),
    do: %{core | left: replace_variables(left, rename), right: replace_variables(right, rename)}

  defp replace_variables(core, _rename), do: core

  defp not_formula(%{tag: :unary, operator: :not, operand: operand}), do: operand
  defp not_formula(formula), do: %{tag: :unary, operator: :not, operand: formula}

  defp dnf(%{tag: :boolean, value: value}, polarity) do
    if value == polarity, do: [empty_branch()], else: []
  end

  defp dnf(%{tag: :variable, name: name}, polarity),
    do: [%{empty_branch() | propositions: %{name => polarity}}]

  defp dnf(%{tag: :unary, operator: :not, operand: operand}, polarity),
    do: dnf(operand, not polarity)

  defp dnf(%{tag: :binary, operator: :and, left: left, right: right}, true),
    do: combine(dnf(left, true), dnf(right, true))

  defp dnf(%{tag: :binary, operator: :and, left: left, right: right}, false),
    do: alternate(dnf(left, false), dnf(right, false))

  defp dnf(%{tag: :binary, operator: :or, left: left, right: right}, true),
    do: alternate(dnf(left, true), dnf(right, true))

  defp dnf(%{tag: :binary, operator: :or, left: left, right: right}, false),
    do: combine(dnf(left, false), dnf(right, false))

  defp dnf(%{tag: :binary, operator: operator, left: left, right: right}, polarity)
       when operator in [:less, :less_equal, :greater, :greater_equal, :equal, :not_equal] do
    if boolean_equality?(operator, left, right) do
      boolean_equality_dnf(operator, left, right, polarity)
    else
      comparison_dnf(operator, left, right, polarity)
    end
  end

  defp dnf(%{tag: :unknown}, _polarity), do: :unknown
  defp dnf(_formula, _polarity), do: :unknown

  defp boolean_equality?(operator, left, right) when operator in [:equal, :not_equal],
    do: boolean_shaped?(left) or boolean_shaped?(right)

  defp boolean_equality?(_operator, _left, _right), do: false

  defp boolean_shaped?(%{tag: :boolean}), do: true
  defp boolean_shaped?(%{tag: :unary, operator: :not}), do: true
  defp boolean_shaped?(%{tag: :binary, operator: operator}) when operator in [:and, :or], do: true
  defp boolean_shaped?(_core), do: false

  defp boolean_equality_dnf(operator, left, right, polarity) do
    equal? = operator == :equal == polarity

    if equal? do
      alternate(
        combine(dnf(left, true), dnf(right, true)),
        combine(dnf(left, false), dnf(right, false))
      )
    else
      alternate(
        combine(dnf(left, true), dnf(right, false)),
        combine(dnf(left, false), dnf(right, true))
      )
    end
  end

  defp comparison_dnf(operator, left, right, polarity) do
    operator = if polarity, do: operator, else: negate_operator(operator)

    case operator do
      :equal ->
        combine(single_constraint(left, right, 0), single_constraint(right, left, 0))

      :not_equal ->
        alternate(single_constraint(left, right, -1), single_constraint(right, left, -1))

      :less ->
        single_constraint(left, right, -1)

      :less_equal ->
        single_constraint(left, right, 0)

      :greater ->
        single_constraint(right, left, -1)

      :greater_equal ->
        single_constraint(right, left, 0)
    end
  end

  defp negate_operator(:equal), do: :not_equal
  defp negate_operator(:not_equal), do: :equal
  defp negate_operator(:less), do: :greater_equal
  defp negate_operator(:less_equal), do: :greater
  defp negate_operator(:greater), do: :less_equal
  defp negate_operator(:greater_equal), do: :less

  defp single_constraint(left, right, adjustment) do
    with {:ok, left} <- linear(left),
         {:ok, right} <- linear(right),
         {:ok, constraint} <- difference_constraint(subtract_linear(left, right), adjustment) do
      case constraint do
        true -> [empty_branch()]
        false -> []
        constraint -> [%{empty_branch() | constraints: [constraint]}]
      end
    else
      _ -> :unknown
    end
  end

  defp linear(%{tag: :integer, value: value}), do: {:ok, {%{}, value}}
  defp linear(%{tag: :variable, name: name}), do: {:ok, {%{name => 1}, 0}}

  defp linear(%{tag: :unary, operator: :negate, operand: operand}) do
    with {:ok, value} <- linear(operand), do: {:ok, scale_linear(value, -1)}
  end

  defp linear(%{tag: :binary, operator: :add, left: left, right: right}) do
    with {:ok, left} <- linear(left),
         {:ok, right} <- linear(right),
         do: {:ok, add_linear(left, right)}
  end

  defp linear(%{tag: :binary, operator: :subtract, left: left, right: right}) do
    with {:ok, left} <- linear(left),
         {:ok, right} <- linear(right),
         do: {:ok, subtract_linear(left, right)}
  end

  defp linear(%{tag: :binary, operator: :multiply, left: left, right: right}) do
    case {linear(left), linear(right)} do
      {{:ok, {coefficients, constant}}, {:ok, value}}
      when map_size(coefficients) == 0 and constant in [-1, 0, 1] ->
        {:ok, scale_linear(value, constant)}

      {{:ok, value}, {:ok, {coefficients, constant}}}
      when map_size(coefficients) == 0 and constant in [-1, 0, 1] ->
        {:ok, scale_linear(value, constant)}

      _ ->
        :unknown
    end
  end

  defp linear(_core), do: :unknown

  defp add_linear({left_coefficients, left_constant}, {right_coefficients, right_constant}) do
    coefficients =
      Map.merge(left_coefficients, right_coefficients, fn _name, left, right -> left + right end)
      |> Enum.reject(fn {_name, coefficient} -> coefficient == 0 end)
      |> Map.new()

    {coefficients, left_constant + right_constant}
  end

  defp subtract_linear(left, right), do: add_linear(left, scale_linear(right, -1))

  defp scale_linear({coefficients, constant}, factor),
    do: {Map.new(coefficients, fn {name, value} -> {name, value * factor} end), constant * factor}

  defp difference_constraint({coefficients, constant}, adjustment) do
    bound = adjustment - constant

    case Enum.sort(coefficients) do
      [] -> {:ok, if(0 <= bound, do: true, else: false)}
      [{x, 1}] -> {:ok, {x, :zero, bound}}
      [{x, -1}] -> {:ok, {:zero, x, bound}}
      [{x, -1}, {y, 1}] -> {:ok, {y, x, bound}}
      [{x, 1}, {y, -1}] -> {:ok, {x, y, bound}}
      _ -> :unknown
    end
  end

  defp alternate(:unknown, _right), do: :unknown
  defp alternate(_left, :unknown), do: :unknown

  defp alternate(left, right) do
    if length(left) + length(right) > @branch_budget, do: :unknown, else: left ++ right
  end

  defp combine(:unknown, _right), do: :unknown
  defp combine(_left, :unknown), do: :unknown

  defp combine(left, right) do
    if length(left) * length(right) > @branch_budget do
      :unknown
    else
      for left_branch <- left,
          right_branch <- right,
          merged = merge_branches(left_branch, right_branch),
          not is_nil(merged),
          do: merged
    end
  end

  defp merge_branches(left, right) do
    conflict? =
      Enum.any?(left.propositions, fn {name, value} ->
        Map.has_key?(right.propositions, name) and right.propositions[name] != value
      end)

    if conflict? do
      nil
    else
      %{
        constraints: left.constraints ++ right.constraints,
        propositions: Map.merge(left.propositions, right.propositions)
      }
    end
  end

  defp branch_satisfiable?(branch) do
    vertices =
      branch.constraints
      |> Enum.flat_map(fn {left, right, _bound} -> [left, right] end)
      |> Kernel.++([:zero])
      |> Enum.uniq()

    distances = Map.new(vertices, &{&1, 0})

    {distances, changed?} =
      Enum.reduce(1..max(length(vertices), 1), {distances, false}, fn _pass,
                                                                      {current, _changed} ->
        Enum.reduce(branch.constraints, {current, false}, fn {left, right, bound},
                                                             {next, changed} ->
          candidate = Map.fetch!(next, right) + bound

          if candidate < Map.fetch!(next, left) do
            {Map.put(next, left, candidate), true}
          else
            {next, changed}
          end
        end)
      end)

    _ = distances
    not changed?
  end

  defp empty_branch, do: %{constraints: [], propositions: %{}}

  defp count(%{tag: :unary, operand: operand}), do: 1 + count(operand)
  defp count(%{tag: :binary, left: left, right: right}), do: 1 + count(left) + count(right)
  defp count(%{tag: _tag}), do: 1
  defp count(_formula), do: @default_budget + 1
end
