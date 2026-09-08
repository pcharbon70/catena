defmodule Catena.TaskInstrumentTest do
  use ExUnit.Case, async: false
  alias Catena.Task.{Instrument, Runtime}

  @tag obligations: ~w(OT-OBL-011)
  test "verified generated recursive code observes cancellation without an explicit source poll" do
    source = """
    (module TaskPollProbe (edition 0.1) (revision 0.1.8) (origin "test://task/poll")
      (export value spin)
      (def spin (signature (Fn Int (effects) Int) (uses))
        (fn (n Int) (call (var spin) (var n)))))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, forms} = Instrument.lower(core)
    assert {:ok, module, binary} = :compile.forms(forms, [:binary, :return_errors])
    assert {:module, ^module} = :code.load_binary(module, ~c"task-instrument-experiment", binary)
    owner = self()

    try do
      assert {{:exit, {:child, _, {:cancelled, 7}}}, _} =
               Runtime.scope(
                 fn scope ->
                   child =
                     Runtime.start(scope, fn ->
                       send(owner, :ready)
                       apply(module, :spin, [0])
                     end)

                   receive do
                     :ready -> :ok
                   end

                   Runtime.cancel(child, 7)
                   :ok
                 end,
                 1_000_000_000
               )
    after
      :code.purge(module)
      :code.delete(module)
    end
  end

  @tag obligations: ~w(OT-OBL-001)
  test "the experimental pass rejects forged typed-core evidence" do
    assert {:error, _} = Instrument.lower(%{format: :kernel_core, version: "0.1.8"})
    assert :ok = Runtime.checkpoint()
  end
end
