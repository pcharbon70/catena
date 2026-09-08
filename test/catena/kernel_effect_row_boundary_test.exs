defmodule Catena.KernelEffectRowBoundaryTest do
  use ExUnit.Case, async: true

  # C010 canonical syntax retains ordinary multiplicity. C005's concrete
  # capability-identity union cannot silently replace that exact revision.
  test "two ordinary requests retain two occurrences in the exact kernel" do
    expression = "(add (request Tick tick 1) (request Tick tick 2))"

    assert {:ok, _core} = Catena.check_kernel(program("Tick Tick", expression))

    assert {:error,
            %{
              id: "T002",
              details: %{actual: ["Tick", "Tick"], declared: ["Tick"]}
            }} = Catena.check_kernel(program("Tick", expression))
  end

  test "finite ordinary rows cannot hide a recursive escaping request" do
    # With n declared occurrences, the body has n + 1. These samples expose
    # the boundary; the row equation explains why increasing n cannot fix it.
    for count <- 0..4 do
      row = Enum.join(List.duplicate("Tick", count), " ")

      source = """
      (module KernelRecursiveRowBoundary
        (edition 0.1) (revision 0.1.8) (origin "test://kernel-recursive-row-boundary")
        (effect Tick (operation tick (params Int) Int))
        (def loop
          (signature (Fn Int (effects #{row}) Int) (uses))
          (fn (n Int)
            (match (equal (var n) 0)
              (case true 0)
              (case false
                (sequence (request Tick tick (var n))
                  (call (var loop) (subtract (var n) 1))))))))
      """

      assert {:error, %{id: "T002", message: "function effect rows are incompatible"}} =
               Catena.check_kernel(source)
    end
  end

  defp program(row, expression) do
    """
    (module KernelRowMultiplicity
      (edition 0.1) (revision 0.1.8) (origin "test://kernel-row-multiplicity")
      (effect Tick (operation tick (params Int) Int))
      (def main (signature Int (uses #{row})) #{expression}))
    """
  end
end
