defmodule Catena.Test.SemanticGenerators do
  @moduledoc false

  alias Catena.{CanonicalJCS, Comprehension}
  alias Catena.Kernel.Stepper

  def mixed do
    %{
      generate: &generate/2,
      valid: &valid?/1,
      shrink: &shrink/1
    }
  end

  def adapters do
    %{
      supports?: &valid?/1,
      reference: &reference/1,
      production: &production/1
    }
  end

  def generate(seed, size) do
    if rem(seed, 2) == 0 do
      %{
        "family" => "typed-kernel",
        "operator" => Enum.at(~w(add multiply), rem(div(seed, 2), 2)),
        "left" => rem(seed, max(size, 1) + 1),
        "right" => rem(div(seed, 7), max(size, 1) + 1)
      }
    else
      count = rem(seed, min(max(size, 1), 8) + 1)
      values = if count == 0, do: [], else: Enum.map(0..(count - 1), &rem(seed + &1, 21))

      %{
        "family" => "comprehension",
        "values" => values,
        "threshold" => rem(div(seed, 11), 21),
        "offset" => rem(div(seed, 17), 9)
      }
    end
  end

  def valid?(%{
        "family" => "typed-kernel",
        "operator" => operator,
        "left" => left,
        "right" => right
      }) do
    operator in ~w(add multiply) and bounded_integer?(left) and bounded_integer?(right)
  end

  def valid?(%{
        "family" => "comprehension",
        "values" => values,
        "threshold" => threshold,
        "offset" => offset
      }) do
    is_list(values) and length(values) <= 8 and Enum.all?(values, &bounded_integer?/1) and
      bounded_integer?(threshold) and bounded_integer?(offset)
  end

  def valid?(_), do: false

  def shrink(%{"family" => "typed-kernel"} = scenario) do
    [
      %{scenario | "left" => div(scenario["left"], 2)},
      %{scenario | "right" => div(scenario["right"], 2)},
      %{scenario | "operator" => "add"}
    ]
    |> Enum.uniq()
  end

  def shrink(%{"family" => "comprehension"} = scenario) do
    [
      %{scenario | "values" => Enum.drop(scenario["values"], -1)},
      %{scenario | "threshold" => div(scenario["threshold"], 2)},
      %{scenario | "offset" => div(scenario["offset"], 2)}
    ]
    |> Enum.uniq()
  end

  def shrink(_), do: []

  def reference(%{"family" => "typed-kernel"} = scenario) do
    source = kernel_source(scenario)

    with {:ok, core} <- Catena.check_kernel(source),
         {:ok, value, outcome} <- Stepper.run(core, "main") do
      {:ok,
       %{status: :completed, value: value, reason: nil, events: [], lifetime: lifetime(outcome)}}
    end
  end

  def reference(%{"family" => "comprehension"} = scenario) do
    with {:ok, source, []} <- comprehension_source(scenario),
         {:ok, core} <- Catena.check_kernel(source),
         {:ok, value, outcome} <- Stepper.run(core, "main") do
      {:ok,
       %{
         status: :completed,
         value: flatten(value),
         reason: nil,
         events: [],
         lifetime: lifetime(outcome)
       }}
    end
  end

  def production(%{"family" => "typed-kernel"} = scenario),
    do: compile_and_run(kernel_source(scenario))

  def production(%{"family" => "comprehension"} = scenario) do
    with {:ok, source, []} <- comprehension_source(scenario) do
      compile_and_run(source, &flatten/1)
    end
  end

  defp compile_and_run(source, transform \\ &Function.identity/1) do
    with {:ok, module, binary, _metadata} <- Catena.compile_kernel(source) do
      :code.purge(module)
      :code.delete(module)

      case :code.load_binary(module, ~c"catena-differential.beam", binary) do
        {:module, ^module} ->
          try do
            {:ok,
             %{
               status: :completed,
               value: transform.(apply(module, :main, [])),
               reason: nil,
               events: [],
               lifetime: [%{pid: 0, status: :terminated}]
             }}
          after
            :code.purge(module)
            :code.delete(module)
          end

        error ->
          {:error, {:load_failed, error}}
      end
    end
  end

  defp kernel_source(scenario) do
    module = module_name(scenario)

    """
    (module #{module}
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://p134/#{module}")
      (export value main)
      (def main
        (signature Int (uses))
        (#{scenario["operator"]} #{scenario["left"]} #{scenario["right"]})))
    """
  end

  defp comprehension_source(scenario) do
    scenario
    |> comprehension_spec()
    |> Comprehension.elaborate()
  end

  defp comprehension_spec(scenario) do
    Comprehension.new(
      module: module_name(scenario),
      origin: "test://p134/#{module_name(scenario)}",
      context: [{"xs", "(List Int)", ints(scenario["values"])}],
      qualifiers: [
        {:generator,
         [
           pattern: "(bind x)",
           element_type: "Int",
           source: "(var xs)",
           binds: [{"x", "Int"}]
         ]},
        {:filter, [expr: "(greater (var x) #{scenario["threshold"]})"]}
      ],
      yield: "(add (var x) #{scenario["offset"]})",
      result_element_type: "Int"
    )
  end

  defp module_name(scenario) do
    suffix = scenario |> CanonicalJCS.digest() |> String.slice(0, 12) |> String.upcase()
    "P134Generated#{suffix}"
  end

  defp ints(values),
    do: Enum.reduce(Enum.reverse(values), "(construct Nil)", &"(construct Cons #{&1} #{&2})")

  defp flatten({:catena_data, _type, 0, []}), do: []
  defp flatten({:catena_data, _type, 1, [head, tail]}), do: [head | flatten(tail)]
  defp flatten(other), do: other

  defp lifetime(outcome),
    do: Enum.map(outcome.processes, &Map.take(&1, [:pid, :status]))

  defp bounded_integer?(value), do: is_integer(value) and value >= 0 and value <= 1_000
end
