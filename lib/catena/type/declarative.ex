defmodule Catena.Type.Declarative do
  @moduledoc "A small bounded declarative oracle, deliberately separate from Algorithm W."

  @base [:integer, :boolean]

  @spec typable?(map(), Catena.Type.t(), map(), non_neg_integer()) :: boolean()
  def typable?(expression, expected, environment \\ %{}, depth \\ 2)

  def typable?(%{tag: :integer}, :integer, _environment, _depth), do: true
  def typable?(%{tag: :boolean}, :boolean, _environment, _depth), do: true

  def typable?(%{tag: :variable, name: name}, expected, environment, _depth),
    do: Map.get(environment, name) == expected

  def typable?(
        %{tag: :function, parameter: parameter, body: body},
        {:function, input, output},
        environment,
        depth
      ) do
    typable?(body, output, Map.put(environment, parameter, input), depth)
  end

  def typable?(%{tag: :call, callee: callee, arguments: [argument]}, expected, environment, depth)
      when depth > 0 do
    Enum.any?(universe(depth - 1), fn input ->
      typable?(callee, {:function, input, expected}, environment, depth - 1) and
        typable?(argument, input, environment, depth - 1)
    end)
  end

  def typable?(%{tag: :tuple, elements: elements}, {:tuple, types}, environment, depth)
      when length(elements) == length(types) do
    Enum.zip(elements, types)
    |> Enum.all?(fn {expression, type} -> typable?(expression, type, environment, depth) end)
  end

  def typable?(_expression, _expected, _environment, _depth), do: false

  @spec universe(non_neg_integer()) :: [Catena.Type.t()]
  def universe(0), do: @base

  def universe(depth) do
    smaller = universe(depth - 1)
    @base ++ for(left <- smaller, right <- smaller, do: {:function, left, right})
  end
end
