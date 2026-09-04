defmodule Catena.C081ExceptionBoundaryTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47)

  @decline_kernel """
  (module C081Decline
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c081/decline")
    (effect Ask
      (operation ask (params Int) Int))
    (handler Fallback
      (effect Ask)
      (input Int)
      (output Int)
      (return result (var result))
      (operation ask
        (params (value Int))
        (resume next)
        0))
    (export value main)
    (def main
      (signature Int (uses))
      (handle Fallback (request Ask ask 99))))
  """

  @trap_kernel """
  (module C081Trap
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c081/trap")
    (export value main)
    (def main
      (signature Int (uses))
      (trap 100)))
  """

  describe "revision registration" do
    @tag obligations: ~w(XB-OBL-001)
    test "0.1.47 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.47"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.47" in LanguageVersion.compilable_revisions()
      refute "0.1.47" in LanguageVersion.artifact_versions()
      refute "0.1.47" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("exception-boundary", "0.1.47")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-47-exception-boundary")
        )

      assert change["affects"] == ~w(static-meaning)
      assert change["summary"] =~ "exception boundary"

      assert String.contains?(
               change["specification"],
               "exception-boundary/the-mechanism-partition.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.47"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :raise_exception, 1)
      refute function_exported?(Catena, :catch_exception, 1)
      refute function_exported?(Catena, :try_expression, 1)
      refute function_exported?(Catena, :rescue_clause, 0)
    end
  end

  describe "the partition" do
    @tag obligations: ~w(XB-OBL-002 XB-OBL-003)
    test "declining to resume aborts to the handler's result on both targets" do
      assert {:ok, core} = Catena.check_kernel(@decline_kernel)

      assert {:ok, 0, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C081Decline, binary, _} = Catena.compile_kernel(@decline_kernel)

      assert {:module, :C081Decline} =
               :code.load_binary(:C081Decline, ~c"c081_decline.beam", binary)

      assert apply(:C081Decline, :main, []) == 0

      on_exit(fn ->
        :code.purge(:C081Decline)
        :code.delete(:C081Decline)
      end)
    end

    @tag obligations: ~w(XB-OBL-002 XB-OBL-007)
    test "the trap stays terminal and uncaught on both targets" do
      assert {:ok, core} = Catena.check_kernel(@trap_kernel)

      assert {:trap, 100, _outcome} = Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C081Trap, binary, _} = Catena.compile_kernel(@trap_kernel)

      assert {:module, :C081Trap} =
               :code.load_binary(:C081Trap, ~c"c081_trap.beam", binary)

      on_exit(fn ->
        :code.purge(:C081Trap)
        :code.delete(:C081Trap)
      end)
    end

    @tag obligations: ~w(XB-OBL-005 XB-OBL-007)
    test "process exits stay local: C010's spared-spawner outcome unchanged" do
      source = File.read!("test/fixtures/c010-kernel.catena")

      assert {:ok, core} = Catena.check_kernel(source)

      assert {:ok, {2, true, 5}, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")
    end
  end

  describe "panics and the door" do
    @tag obligations: ~w(XB-OBL-004)
    test "panics are trap kinds entering with producers: no producer, no form" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :panic, 1)
      refute function_exported?(Catena, :assert_form, 1)

      panic_program = unknown_expression_program("panic")
      assert {:error, _} = Catena.check_json(panic_program)
    end

    @tag obligations: ~w(XB-OBL-006)
    test "no language exception form exists on any frontend" do
      for tag <- ["raise", "try", "catch", "rescue", "throw"] do
        assert {:error, _} = Catena.check_json(unknown_expression_program(tag))
      end

      kernel_attempt = """
      (module C081Try
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c081/try")
        (export value main)
        (def main
          (signature Int (uses))
          (try (trap 1) (catch (var reason) 0))))
      """

      assert {:error, _} = Catena.check_kernel(kernel_attempt)
    end

    @tag obligations: ~w(XB-OBL-002 XB-OBL-007)
    test "determinism: repeated runs repeat" do
      assert {:ok, core} = Catena.check_kernel(@decline_kernel)

      assert {:ok, first, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, second, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert first == second
    end
  end

  defp unknown_expression_program(tag) do
    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c081/unknown",
      "module" => "C081Unknown",
      "source" => "c081.catena.json",
      "exports" => ["main"],
      "type_exports" => [],
      "type_groups" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}, "uses" => []},
          "body" => %{"tag" => tag}
        }
      ],
      "effects" => [],
      "handlers" => []
    })
  end
end
