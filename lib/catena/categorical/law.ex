defmodule Catena.Categorical.Law do
  @moduledoc "Bounded Catena 0.1.4 law checks; results are test evidence, never optimizer proofs."

  alias Catena.Type.Trait

  @spec check_equatable!(Trait.t(), [Trait.type_term()], [term()], (map(), String.t(), [term()] ->
                                                                      term())) :: map()
  def check_equatable!(registry, arguments, values, invoke) when is_function(invoke, 3) do
    evidence = Trait.resolve!(registry, "Equatable", arguments)
    equals = fn left, right -> invoke.(evidence, "equals", [left, right]) end

    reflexive? = Enum.all?(values, &equals.(&1, &1))

    symmetric? =
      Enum.all?(values, fn left ->
        Enum.all?(values, fn right -> equals.(left, right) == equals.(right, left) end)
      end)

    transitive? =
      Enum.all?(values, fn first ->
        Enum.all?(values, fn second ->
          Enum.all?(values, fn third ->
            not (equals.(first, second) and equals.(second, third)) or equals.(first, third)
          end)
        end)
      end)

    unless reflexive? and symmetric? and transitive? do
      raise ArgumentError, "Equatable failed bounded equivalence checks"
    end

    %{
      status: :tested,
      law: "equivalence",
      instance_id: evidence.instance_id,
      sample_count: length(values),
      domain: :finite_values
    }
  end

  @spec extensionally_equal?((term() -> term()), (term() -> term()), [term()]) :: boolean()
  def extensionally_equal?(left, right, samples)
      when is_function(left, 1) and is_function(right, 1) and is_list(samples),
      do: Enum.all?(samples, &(left.(&1) === right.(&1)))
end
