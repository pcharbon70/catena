defmodule Catena.OptimizerValidityTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Optimizer}
  alias Catena.Kernel.Stepper

  @tag obligations: ~w(OZ-OBL-001 OZ-OBL-002 OZ-OBL-003 OZ-OBL-004)
  test "profile inventories the closed checked rewrite domain" do
    profile = Optimizer.profile()
    assert profile.version == "0.1.86"
    assert profile.modes == [:disabled, :checked]
    assert profile.enabled_rules == [:fold_integer_literals, :right_integer_identity]
    assert profile.refused_rules == [:integer_annihilation]
    assert profile.public_source == :held_for_p109
    refute profile.finite_testing_is_proof
    assert map_size(profile.inventories) == 5
  end

  @tag obligations: ~w(OZ-OBL-005 OZ-OBL-006 OZ-OBL-007 OZ-OBL-008)
  test "checked literal and identity rewrites replay from verified core" do
    source = module_source("P135Certificates", "(add (multiply 2 3) 0)")
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, result} = Catena.optimize_kernel(core, mode: :checked)
    assert length(result.certificates) == 2

    assert Enum.map(result.certificates, & &1.rule) == [
             :fold_integer_literals,
             :fold_integer_literals
           ]

    assert :ok = Optimizer.verify_result(core, result)
    assert {:ok, 6, _} = Stepper.run(result.core, "main")

    tampered = update_in(result.certificates, [Access.at(0), :premises], &Enum.drop(&1, 1))

    assert {:error, :invalid_optimizer_certificate} =
             Optimizer.verify_result(core, %{result | certificates: tampered})
  end

  @tag obligations: ~w(OZ-OBL-009 OZ-OBL-010 OZ-OBL-011)
  test "disabled and checked compilation preserve reference and BEAM observations" do
    source = module_source("P135Agreement", "(multiply (add 20 1) 1)")
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, 21, _} = Stepper.run(core, "main")

    assert {:ok, disabled_module, disabled_binary, disabled_metadata} =
             Catena.compile_kernel(source, optimizer: :disabled)

    assert disabled_metadata.optimizer.certificates == []
    assert run(disabled_module, disabled_binary) == 21

    assert {:ok, checked_module, checked_binary, checked_metadata} =
             Catena.compile_kernel(source, optimizer: :checked)

    assert length(checked_metadata.optimizer.certificates) == 2
    assert run(checked_module, checked_binary) == 21
    refute disabled_metadata.optimizer.output_digest == checked_metadata.optimizer.output_digest
  end

  @tag obligations: ~w(OZ-OBL-012 OZ-OBL-013 OZ-OBL-014)
  test "annihilation is refused without machine-checked purity and totality" do
    source = module_source("P135Refusal", "(let x 7 (multiply 0 (var x)))")
    assert {:ok, core} = Catena.check_kernel(source)

    assert {:ok, result} =
             Catena.optimize_kernel(core, mode: :checked, rules: [:integer_annihilation])

    assert result.core == core
    assert result.certificates == []

    assert [%{rule: :integer_annihilation, reason: :requires_machine_checked_purity_and_totality}] =
             result.rejected

    assert {:ok, 0, _} = Stepper.run(result.core, "main")
  end

  @tag obligations: ~w(OZ-OBL-010 OZ-OBL-012 OZ-OBL-015)
  test "trap-producing annihilation is refused and the terminal remains observable" do
    trap_source = module_source("P135Trap", "(multiply 0 (trap 9))")
    assert {:ok, trap_core} = Catena.check_kernel(trap_source)
    assert {:ok, trap_result} = Catena.optimize_kernel(trap_core, mode: :checked)
    assert trap_result.certificates == []
    assert [_] = trap_result.rejected
    assert {:trap, 9, _} = Stepper.run(trap_result.core, "main")

    tags = collect_tags(trap_result.core)
    assert :trap in tags
    assert :binary in tags
  end

  @tag obligations: ~w(OZ-OBL-009 OZ-OBL-010 OZ-OBL-012)
  test "right identity preserves a call and its exact-once evaluation" do
    source =
      module_source(
        "P135Call",
        "(let f (fn (value Int) (add (var value) 1)) (add (call (var f) 41) 0))"
      )

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, result} = Catena.optimize_kernel(core, mode: :checked)
    assert Enum.any?(result.certificates, &(&1.rule == :right_integer_identity))
    assert Enum.count(collect_tags(result.core), &(&1 == :call)) == 1
    assert {:ok, 42, _} = Stepper.run(result.core, "main")
  end

  @tag obligations: ~w(OZ-OBL-006 OZ-OBL-009 OZ-OBL-016)
  test "large pure structures optimize deterministically without changing value" do
    expression = Enum.reduce(1..128, "1", fn _, inner -> "(add #{inner} 0)" end)
    source = module_source("P135Large", expression)
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, first} = Catena.optimize_kernel(core, mode: :checked)
    assert {:ok, second} = Catena.optimize_kernel(core, mode: :checked)
    assert first == second
    assert length(first.certificates) == 128
    assert {:ok, 1, _} = Stepper.run(first.core, "main")
  end

  @tag obligations: ~w(OZ-OBL-001 OZ-OBL-017 OZ-OBL-018)
  test "optimizer validity is selected cumulatively and disclosed" do
    assert LanguageVersion.introduced(:optimizer_validity) == "0.1.86"

    assert LanguageVersion.from(:optimizer_validity) == [
             "0.1.86",
             "0.1.87",
             "0.1.88",
             "0.1.89",
             "0.1.90",
             "0.1.91"
           ]

    assert %{"version" => "0.1.86"} = Catena.ConformanceInfo.document()["optimizer_validity"]

    assert {:ok, :stable} == LanguageLifecycle.state("optimizer-validity", "0.1.86")
  end

  test "invalid modes and rule sets fail before transformation" do
    assert {:ok, core} = Catena.check_kernel(module_source("P135Invalid", "1"))
    assert {:error, :invalid_optimizer_mode} = Catena.optimize_kernel(core, mode: :fast)

    assert {:error, :invalid_optimizer_rules} =
             Catena.optimize_kernel(core, mode: :checked, rules: [:unknown])

    assert {:error, :invalid_optimizer_rules} =
             Catena.optimize_kernel(core,
               mode: :checked,
               rules: [:fold_integer_literals, :fold_integer_literals]
             )
  end

  defp module_source(module, expression) do
    """
    (module #{module}
      (edition 0.1)
      (revision 0.1.8)
      (origin "test://p135/#{module}")
      (export value main)
      (def main (signature Int (uses)) #{expression}))
    """
  end

  defp run(module, binary) do
    :code.purge(module)
    :code.delete(module)
    assert {:module, ^module} = :code.load_binary(module, ~c"p135-optimizer.beam", binary)

    try do
      apply(module, :main, [])
    after
      :code.purge(module)
      :code.delete(module)
    end
  end

  defp collect_tags(term) when is_list(term), do: Enum.flat_map(term, &collect_tags/1)
  defp collect_tags(%{__struct__: _}), do: []

  defp collect_tags(term) when is_map(term),
    do: Enum.flat_map(Map.values(term), &collect_tags/1) ++ List.wrap(Map.get(term, :tag))

  defp collect_tags(_), do: []
end
