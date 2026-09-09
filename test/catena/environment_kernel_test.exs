defmodule Catena.EnvironmentKernelTest do
  use ExUnit.Case, async: true
  alias Catena.Runtime.Environment.Kernel

  defp parsed do
    {:ok, tree} =
      Catena.Kernel.Parser.parse("""
      (module EnvironmentRead (edition 0.1) (revision 0.1.8) (origin "test://environment-read")
        (export value main)
        (effect Store (operation read (params Unit) Int))
        (def main (signature Int (uses Store)) (request Store read (unit))))
      """)

    replace(tree)
  end

  defp replace(:integer), do: :bytes
  defp replace(%Catena.SourceSpan{} = span), do: span
  defp replace(map) when is_map(map), do: Map.new(map, fn {k, v} -> {k, replace(v)} end)
  defp replace(list) when is_list(list), do: Enum.map(list, &replace/1)

  defp replace(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&replace/1) |> List.to_tuple()

  defp replace(value), do: value

  test "new checked profile combines closed scalar data with explicit capability slots" do
    assert {:ok, core} = Kernel.check(parsed(), %{"Store" => "catena://environment/filesystem"})
    assert core.version == "0.1.68"
    assert core.profile == :environmental_effects
    assert :ok = Kernel.verify(core)
    assert {:ok, _forms, _symbol} = Catena.Kernel.Backend.lower_foreign_entry(core, "main")
    assert {:error, _} = Kernel.verify(%{core | profile: :value_boundaries})
    assert {:error, _} = Kernel.check(parsed(), %{})
  end

  test "retained capability profiles do not gain scalar carriers" do
    {:ok, prepared} =
      Catena.Kernel.CapabilityKernel.prepare(parsed(), %{
        "Store" => "catena://environment/filesystem"
      })

    assert {:error, _} = Catena.Kernel.Checker.check(prepared)
  end

  test "exact environmental profile refuses a lexical capability escaping in a returned closure" do
    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module EscapedEnvironment (edition 0.1) (revision 0.1.8) (origin "test://escaped-environment")
        (export value main)
        (effect Ask (operation ask (params Int) Int))
        (handler Answer (effect Ask)
          (input (Fn Int (effects Ask) Int)) (output (Fn Int (effects Ask) Int))
          (return value (var value))
          (operation ask (params (value Int)) (resume next) (resume next (var value))))
        (def main (signature (Fn Int (effects Ask) Int) (uses))
          (handle Answer (sequence (request Ask ask 1) (fn (n Int) (request Ask ask (var n)))))))
      """)

    assert {:error, _} = Kernel.check(parsed, %{"Ask" => "catena://environment/test"})
  end
end
