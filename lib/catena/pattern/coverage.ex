defmodule Catena.Pattern.Coverage do
  @moduledoc "Typed-pattern usefulness, exhaustiveness, and redundancy analysis."

  alias Catena.{Diagnostic, ImplementationLimits, Type}
  alias Catena.Condition.Facts

  @default_budget ImplementationLimits.configured(:pattern_coverage_steps)

  @spec check!([map()], Type.t(), map(), keyword()) :: map()
  def check!(clauses, scrutinee_type, data, options \\ []) do
    budget_limit = Keyword.get(options, :coverage_budget, @default_budget)
    match_path = Keyword.get(options, :path)

    {matrix, checked, budget, root_formulas} =
      clauses
      |> Enum.with_index()
      |> Enum.reduce({[], [], budget_limit, []}, fn {clause, index},
                                                    {matrix, checked, remaining, root_formulas} ->
        alternatives = expand(clause.pattern, remaining)

        {useful?, remaining} =
          Enum.reduce(alternatives, {false, remaining}, fn pattern, {found, current} ->
            case useful(matrix, [pattern], [scrutinee_type], data, current) do
              {{:useful, _witness}, next} -> {true, next}
              {:not_useful, next} -> {found, next}
            end
          end)

        guard_formula = Facts.guard_formula(clause.guard, clause.pattern)
        guard_class = guard_class(clause.guard, options)

        fact_redundant? =
          if Facts.root_pattern?(clause.pattern) and root_formulas != [] do
            prior = Facts.disjoin(root_formulas)
            uncovered = Facts.conjoin(guard_formula, Facts.negate(prior))
            Facts.satisfiable?(uncovered, options) == false
          else
            false
          end

        if guard_class == false or not useful? or fact_redundant? do
          fail("M002", "redundant match clause #{index + 1}", clause.path)
        end

        matrix =
          if guard_class == true do
            matrix ++ Enum.map(alternatives, &[&1])
          else
            matrix
          end

        root_formulas =
          if Facts.root_pattern?(clause.pattern),
            do: root_formulas ++ [guard_formula],
            else: root_formulas

        checked_clause =
          clause
          |> Map.put(:guard_class, guard_class)
          |> Map.put(:fact_evidence, %{
            formula: guard_formula,
            classification: guard_class,
            theory: :integer_difference_constraints
          })

        {matrix, [checked_clause | checked], remaining, root_formulas}
      end)

    wildcard = %{tag: :wildcard, type: scrutinee_type, path: nil}

    structural_result = useful(matrix, [wildcard], [scrutinee_type], data, budget)

    case structural_result do
      {{:useful, witness}, _remaining} ->
        if root_formulas != [] and Facts.tautology?(Facts.disjoin(root_formulas), options) == true do
          %{clauses: Enum.reverse(checked), exhaustive?: true, budget: budget_limit - budget}
        else
          rendered = witness |> List.first() |> render_witness()

          fail("M001", "non-exhaustive match; missing #{rendered}", match_path,
            witness: rendered,
            scrutinee_type: Type.normalize(scrutinee_type)
          )
        end

      {:not_useful, remaining} ->
        %{clauses: Enum.reverse(checked), exhaustive?: true, budget: budget_limit - remaining}
    end
  end

  def guard_class(nil), do: true
  def guard_class(%{tag: :boolean, value: true}), do: true
  def guard_class(%{tag: :boolean, value: false}), do: false
  def guard_class(_), do: :unknown

  def guard_class(guard, options) do
    formula = Facts.guard_formula(guard, %{tag: :wildcard})

    case {Facts.satisfiable?(formula, options), Facts.tautology?(formula, options)} do
      {false, _} -> false
      {_, true} -> true
      _ -> :unknown
    end
  end

  defp useful(_matrix, _vector, _types, _data, budget) when budget <= 0 do
    fail(
      "M004",
      "pattern coverage analysis exceeded its deterministic budget",
      nil,
      ImplementationLimits.details(:pattern_coverage_steps, @default_budget + 1)
      |> Map.put(:minimum_budget, @default_budget)
    )
  end

  defp useful(matrix, [], [], _data, budget) do
    if matrix == [], do: {{:useful, []}, budget - 1}, else: {:not_useful, budget - 1}
  end

  defp useful(matrix, [pattern | rest], [type | rest_types], data, budget) do
    budget = budget - 1

    if wildcard?(pattern) do
      case signatures(type, data) do
        {:finite, []} ->
          {:not_useful, budget}

        {:finite, constructors} ->
          Enum.reduce_while(constructors, {:not_useful, budget}, fn signature,
                                                                    {_result, current} ->
            specialized = specialize_matrix(matrix, signature)

            wildcards =
              Enum.map(signature.argument_types, &%{tag: :wildcard, type: &1, path: nil})

            case useful(
                   specialized,
                   wildcards ++ rest,
                   signature.argument_types ++ rest_types,
                   data,
                   current
                 ) do
              {{:useful, witness}, next} ->
                {head, tail} = rebuild_witness(signature, witness)
                {:halt, {{:useful, [head | tail]}, next}}

              {:not_useful, next} ->
                {:cont, {:not_useful, next}}
            end
          end)

        :open ->
          case useful(default_matrix(matrix), rest, rest_types, data, budget) do
            {{:useful, witness}, next} -> {{:useful, [%{tag: :wildcard} | witness]}, next}
            {:not_useful, next} -> {:not_useful, next}
          end
      end
    else
      signature = pattern_signature(pattern, type, data)
      specialized = specialize_matrix(matrix, signature)
      arguments = pattern_arguments(pattern, signature)

      case useful(
             specialized,
             arguments ++ rest,
             signature.argument_types ++ rest_types,
             data,
             budget
           ) do
        {{:useful, witness}, next} ->
          {head, tail} = rebuild_witness(signature, witness)
          {{:useful, [head | tail]}, next}

        {:not_useful, next} ->
          {:not_useful, next}
      end
    end
  end

  defp signatures(:boolean, _data) do
    {:finite,
     [
       %{key: {:boolean, false}, argument_types: [], witness: %{tag: :boolean, value: false}},
       %{key: {:boolean, true}, argument_types: [], witness: %{tag: :boolean, value: true}}
     ]}
  end

  defp signatures({:tuple, elements}, _data) do
    {:finite,
     [%{key: {:tuple, length(elements)}, argument_types: elements, witness: %{tag: :tuple}}]}
  end

  defp signatures({:nominal, id, _arguments} = type, data) do
    case Map.get(data.types_by_id, id) do
      %{visibility: visibility, constructors: constructors, imported?: imported?}
      when not imported? or visibility in [:internal, :transparent] ->
        signatures =
          constructors
          |> Enum.filter(&constructor_possible?(&1, type))
          |> Enum.reject(&constructor_proven_empty?(&1, data))
          |> Enum.map(fn constructor ->
            %{
              key: {:constructor, constructor.id},
              argument_types: constructor_field_types(constructor, type),
              witness: %{tag: :constructor, constructor: constructor}
            }
          end)

        {:finite, signatures}

      _ ->
        :open
    end
  end

  defp signatures(_type, _data), do: :open

  defp pattern_signature(%{tag: :boolean, value: value}, _type, _data),
    do: %{key: {:boolean, value}, argument_types: [], witness: %{tag: :boolean, value: value}}

  defp pattern_signature(%{tag: :integer, value: value}, _type, _data),
    do: %{key: {:integer, value}, argument_types: [], witness: %{tag: :integer, value: value}}

  defp pattern_signature(%{tag: :tuple, elements: elements}, {:tuple, types}, _data),
    do: %{key: {:tuple, length(elements)}, argument_types: types, witness: %{tag: :tuple}}

  defp pattern_signature(%{tag: :constructor, constructor: constructor}, type, _data),
    do: %{
      key: {:constructor, constructor.id},
      argument_types: constructor_field_types(constructor, type),
      witness: %{tag: :constructor, constructor: constructor}
    }

  defp pattern_arguments(%{tag: :tuple, elements: elements}, _signature), do: elements
  defp pattern_arguments(%{tag: :constructor, patterns: patterns}, _signature), do: patterns
  defp pattern_arguments(_pattern, _signature), do: []

  defp specialize_matrix(matrix, signature) do
    Enum.flat_map(matrix, fn [head | tail] ->
      cond do
        wildcard?(head) -> [wildcard_arguments(signature.argument_types) ++ tail]
        pattern_key(head) == signature.key -> [pattern_arguments(head, signature) ++ tail]
        true -> []
      end
    end)
  end

  defp default_matrix(matrix) do
    Enum.flat_map(matrix, fn [head | tail] -> if wildcard?(head), do: [tail], else: [] end)
  end

  defp wildcard_arguments(types),
    do: Enum.map(types, &%{tag: :wildcard, type: &1, path: nil})

  defp wildcard?(%{tag: tag}) when tag in [:wildcard, :bind], do: true
  defp wildcard?(%{tag: :as, pattern: pattern}), do: wildcard?(pattern)
  defp wildcard?(_pattern), do: false

  defp pattern_key(%{tag: :boolean, value: value}), do: {:boolean, value}
  defp pattern_key(%{tag: :integer, value: value}), do: {:integer, value}
  defp pattern_key(%{tag: :tuple, elements: elements}), do: {:tuple, length(elements)}

  defp pattern_key(%{tag: :constructor, constructor: constructor}),
    do: {:constructor, constructor.id}

  defp pattern_key(%{tag: :as, pattern: pattern}), do: pattern_key(pattern)

  defp rebuild_witness(signature, witness) do
    {arguments, rest} = Enum.split(witness, length(signature.argument_types))
    {Map.put(signature.witness, :arguments, arguments), rest}
  end

  defp constructor_field_types(constructor, {:nominal, _id, arguments}) do
    replacements =
      arguments
      |> Enum.with_index()
      |> Map.new(fn {argument, index} -> {index, argument} end)

    Enum.map(constructor.fields, &Type.apply(&1.type, replacements))
  end

  defp constructor_possible?(constructor, {:nominal, id, arguments}) do
    case constructor.result do
      {:nominal, ^id, result_arguments} ->
        Enum.zip(result_arguments, arguments)
        |> Enum.all?(fn {left, right} -> compatible?(left, right) end)

      _ ->
        false
    end
  end

  defp compatible?({:var, _}, _), do: true
  defp compatible?({:skolem, _}, _), do: true
  defp compatible?(_, {:var, _}), do: true
  defp compatible?(_, {:skolem, _}), do: true

  defp compatible?({:nominal, id, left}, {:nominal, id, right}),
    do: Enum.zip(left, right) |> Enum.all?(fn {l, r} -> compatible?(l, r) end)

  defp compatible?({:tuple, left}, {:tuple, right}) when length(left) == length(right),
    do: Enum.zip(left, right) |> Enum.all?(fn {l, r} -> compatible?(l, r) end)

  defp compatible?(left, right), do: left == right

  defp constructor_proven_empty?(constructor, data) do
    Enum.any?(constructor.fields, fn field ->
      case field.type do
        {:nominal, id, _} -> Map.get(data.types_by_id, id, %{})[:inhabitation] == :empty
        {:tuple, fields} -> Enum.any?(fields, &proven_empty_type?(&1, data))
        _ -> false
      end
    end)
  end

  defp proven_empty_type?({:nominal, id, _}, data),
    do: Map.get(data.types_by_id, id, %{})[:inhabitation] == :empty

  defp proven_empty_type?({:tuple, fields}, data),
    do: Enum.any?(fields, &proven_empty_type?(&1, data))

  defp proven_empty_type?(_type, _data), do: false

  defp expand(pattern, budget) do
    alternatives = do_expand(pattern)

    if length(alternatives) > budget,
      do: fail("M004", "or-pattern expansion exceeded budget", pattern.path)

    alternatives
  end

  defp do_expand(%{tag: :or, alternatives: alternatives}),
    do: Enum.flat_map(alternatives, &do_expand/1)

  defp do_expand(%{tag: :tuple, elements: elements} = pattern) do
    elements
    |> Enum.map(&do_expand/1)
    |> cartesian()
    |> Enum.map(&%{pattern | elements: &1})
  end

  defp do_expand(%{tag: :constructor, patterns: patterns} = pattern) do
    patterns
    |> Enum.map(&do_expand/1)
    |> cartesian()
    |> Enum.map(&%{pattern | patterns: &1})
  end

  defp do_expand(%{tag: :as, pattern: inner} = pattern),
    do: Enum.map(do_expand(inner), &%{pattern | pattern: &1})

  defp do_expand(pattern), do: [pattern]

  defp cartesian([]), do: [[]]

  defp cartesian([head | tail]) do
    for item <- head, rest <- cartesian(tail), do: [item | rest]
  end

  defp render_witness(%{tag: :wildcard}), do: "_"
  defp render_witness(%{tag: :boolean, value: value}), do: to_string(value)
  defp render_witness(%{tag: :integer, value: value}), do: Integer.to_string(value)

  defp render_witness(%{tag: :tuple, arguments: arguments}),
    do: "(" <> Enum.map_join(arguments, ", ", &render_witness/1) <> ")"

  defp render_witness(%{tag: :constructor, constructor: constructor, arguments: []}),
    do: constructor.qualified

  defp render_witness(%{tag: :constructor, constructor: constructor, arguments: arguments}),
    do: constructor.qualified <> "(" <> Enum.map_join(arguments, ", ", &render_witness/1) <> ")"

  defp fail(id, message, path, details \\ []) do
    raise Catena.TypeError,
      diagnostic: Diagnostic.new(id, message, path: path, details: Map.new(details))
  end
end
