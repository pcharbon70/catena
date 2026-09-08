defmodule Catena.KernelCapabilityIntegrationTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Backend, CapabilityKernel, Stepper, Verifier}

  @prefix """
  (module CapabilityIntegration
    (edition 0.1) (revision 0.1.8) (origin "test://capability-integration")
    (export value main)
    (effect Ask (operation ask (params Int) Int))
    (handler Answer (effect Ask) (input Int) (output Int)
      (return value (var value))
      (operation ask (params (value Int)) (resume next)
        (resume next (add (var value) 1))))
  """

  @tag obligations: ~w(CK-OBL-003 CK-OBL-007)
  test "recursive repeated requests through one slot agree on reference and BEAM" do
    source =
      @prefix <>
        """
        (def loop (signature (Fn Int (effects Ask) Int) (uses))
          (fn (n Int)
            (match (equal (var n) 0)
              (case true 0)
              (case false (add (request Ask ask (var n))
                (call (var loop) (subtract (var n) 1)))))))
        (def main (signature Int (uses)) (handle Answer (call (var loop) 3))))
        """

    assert {:error, _} = Catena.check_kernel(source)
    assert {:ok, core} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
    assert :ok = Verifier.verify(core)
    assert {:ok, 9, outcome} = Stepper.run(core, "main")
    assert Enum.count(outcome.trace, &(&1.label == :request)) == 3
    assert beam(core) == 9
    assert {:ok, _, _, %{artifact_version: "0.1.50", interface: nil}} = Backend.compile(core)
    assert {:error, _} = Verifier.verify(%{core | version: "0.1.8"})
  end

  @tag obligations: ~w(CK-OBL-004 CK-OBL-006)
  test "a handler declining resumption aborts the entire recursive computation" do
    source = @prefix |> String.replace("(resume next (add (var value) 1))", "77")

    source =
      source <>
        """
        (def loop (signature (Fn Int (effects Ask) Int) (uses))
          (fn (n Int) (add (request Ask ask (var n)) (call (var loop) (var n)))))
        (def main (signature Int (uses)) (handle Answer (call (var loop) 3))))
        """

    assert {:ok, core} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
    assert {:ok, 77, outcome} = Stepper.run(core, "main")
    assert Enum.count(outcome.trace, &(&1.label == :request)) == 1
    assert beam(core) == 77
  end

  @tag obligations: ~w(CK-OBL-004)
  test "an effectful closure cannot escape its handler and ambient slot access rejects" do
    source =
      @prefix <>
        """
        (def main (signature Int (uses)) (request Ask ask 1)))
        """

    assert {:error, _} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
    # The handler consumes an evaluation effect but the returned callable still carries the slot.
    source =
      @prefix
      |> String.replace(
        "(input Int) (output Int)",
        "(input (Fn Int (effects Ask) Int)) (output (Fn Int (effects Ask) Int))"
      )

    source =
      source <>
        """
        (def main (signature (Fn Int (effects Ask) Int) (uses))
          (handle Answer (sequence (request Ask ask 1) (fn (n Int) (request Ask ask (var n)))))))
        """

    assert {:error, _} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
  end

  @tag obligations: ~w(CK-OBL-002)
  test "two slots from one nominal family select their own handlers" do
    source = """
    (module CapabilityIntegration (edition 0.1) (revision 0.1.8) (origin "test://two-slots")
      (export value main)
      (effect Left (operation ask (params Int) Int))
      (effect Right (operation ask (params Int) Int))
      (handler L (effect Left) (input Int) (output Int)
        (return value (var value))
        (operation ask (params (value Int)) (resume next) (resume next (add (var value) 10))))
      (handler R (effect Right) (input Int) (output Int)
        (return value (var value))
        (operation ask (params (value Int)) (resume next) (resume next (add (var value) 100))))
      (def main (signature Int (uses))
        (handle L (handle R (add (request Left ask 1) (request Right ask 2))))))
    """

    bindings = %{"Left" => "State", "Right" => "State"}
    assert {:ok, core} = CapabilityKernel.check(source, bindings)
    assert {:ok, 113, outcome} = Stepper.run(core, "main")
    assert beam(core) == 113
    requests = Enum.filter(outcome.trace, &(&1.label == :request))
    assert length(Enum.uniq_by(requests, & &1.effect)) == 2

    forged =
      put_in(
        core.effects[hd(requests).effect].occurrence,
        {:capability, List.last(requests).effect}
      )

    assert {:error, _} = Verifier.verify(forged)

    inconsistent =
      String.replace(
        source,
        "(effect Right (operation ask (params Int) Int))",
        "(effect Right (operation ask (params Bool) Int))"
      )

    assert {:error, _} = CapabilityKernel.check(inconsistent, bindings)
  end

  @tag obligations: ~w(CK-OBL-007)
  test "a forged inner request cannot retain the old checked effect evidence" do
    source = @prefix <> "(def main (signature Int (uses)) (handle Answer (request Ask ask 1))))"
    assert {:ok, core} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
    [definition] = core.definitions
    expression = put_in(definition.expression.expression.effects, [])
    forged = %{core | definitions: [%{definition | expression: expression}]}
    assert {:error, _} = Verifier.verify(forged)
    assert {:error, _} = CapabilityKernel.check(source, %{})
  end

  @tag obligations: ~w(CK-OBL-001 CK-OBL-008)
  test "registered boundary preserves historical formats and records the new artifact selection" do
    assert Catena.LanguageVersion.capability_frontend_versions() == ["0.1.50"]
    assert "0.1.50" in Catena.LanguageVersion.artifact_versions()
    refute "0.1.50" in Catena.LanguageVersion.interface_versions()
    refute "0.1.50" in Catena.LanguageVersion.signed_format_versions()
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("closed-capability-kernel", "0.1.50")
    source = @prefix <> "(def main (signature Int (uses)) (handle Answer (request Ask ask 1))))"

    assert {:error, %{id: "EDN001"}} =
             CapabilityKernel.check(source, %{"Ask" => "AskFamily"},
               language_selection: %Catena.LanguageSelection{
                 edition: "0.1",
                 language_revision: "0.1.8",
                 previews: []
               }
             )

    assert {:ok, core} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})

    assert {:ok, module, binary, %{interface: nil, artifact_version: "0.1.50"}} =
             Backend.compile(core)

    assert {:ok, {^module, [compile_info: info]}} = :beam_lib.chunks(binary, [:compile_info])
    assert info[:catena_language_revision] == ~c"0.1.50"
    assert info[:catena_frontend] == ~c"closed-capability-tree-0.1.50"
    assert {:error, _} = Verifier.verify(%{core | language_revision: "0.1.8"})
  end

  @tag obligations: ~w(CK-OBL-008)
  test "abstract unhandled main can be checked but cannot become an executable artifact" do
    source = @prefix <> "(def main (signature Int (uses Ask)) (request Ask ask 1)))"
    assert {:ok, core} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
    assert {:error, %{id: "EFX003"}} = Backend.compile(core)

    source =
      @prefix <>
        "(def main (signature (Fn Int (effects Ask) Int) (uses)) (fn (n Int) (request Ask ask (var n)))))"

    assert {:ok, core} = CapabilityKernel.check(source, %{"Ask" => "AskFamily"})
    assert {:error, %{id: "EFX003"}} = Backend.compile(core)
  end

  @tag obligations: ~w(CK-OBL-007 CK-OBL-008)
  test "intrinsic Process remains available without inventing ambient ordinary authority" do
    source = """
    (module CapabilityProcess (edition 0.1) (revision 0.1.8) (origin "test://capability-process")
      (export value main)
      (process Wait (mailbox Int) (params) (receive (case (bind value) (unit))))
      (def main (signature (Process Int) (uses Process)) (spawn Wait)))
    """

    assert {:ok, core} = CapabilityKernel.check(source, %{})
    assert {:quiescent, outcome} = Stepper.run(core, "main")
    assert outcome.root_result == {:catena_process, 1}
    assert {:ok, module, binary, _} = Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"capability-process.beam", binary)
    worker = apply(module, :main, [])
    assert is_pid(worker)
    monitor = Process.monitor(worker)
    Process.exit(worker, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^worker, _}, 1000
    :code.delete(module)
    :code.purge(module)
  end

  defp beam(core) do
    assert {:ok, module, binary, %{artifact_version: "0.1.50"}} = Backend.compile(core)
    assert {:module, ^module} = :code.load_binary(module, ~c"capability-integration.beam", binary)

    try do
      apply(module, :main, [])
    after
      :code.delete(module)
      :code.purge(module)
    end
  end
end
