defmodule Catena.C061NumericRelationshipsTest do
  use ExUnit.Case, async: false

  alias Catena.{Effect.Runtime, LanguageLifecycle, LanguageVersion, Values}
  alias Catena.Type.Infer
  alias Catena.Type.Scheme

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46)

  @int_kernel """
  (module C061Int
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c061/int")
    (export value main)
    (def main
      (signature Int (uses))
      (add (multiply 2 3) 4)))
  """

  describe "revision registration" do
    @tag obligations: ~w(NR-OBL-001)
    test "0.1.40 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.46"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.40" in LanguageVersion.compilable_revisions()
      refute "0.1.40" in LanguageVersion.artifact_versions()
      refute "0.1.40" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("numeric-relationships", "0.1.40")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-40-numeric-relationships")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "numeric-relationships/the-closed-set-instantiation-rule.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
      assert {:ok, %{selection: %{language_revision: "0.1.46"}}} = Catena.decode_source_text("")
      assert true = Values.value?(1.5)

      refute function_exported?(Catena, :numeric_trait, 0)
      refute function_exported?(Catena, :operator_instance, 2)
    end
  end

  describe "the closed-set instantiation rule" do
    @tag obligations: ~w(NR-OBL-002 NR-OBL-005)
    test "float arithmetic infers Float and Int arithmetic infers Int" do
      assert {:ok, :float, :float} =
               infer_binary(:add, Scheme.mono(:float), Scheme.mono(:float))

      assert {:ok, :float, :float} =
               infer_binary(:subtract, Scheme.mono(:float), Scheme.mono(:float))

      assert {:ok, :float, :float} =
               infer_binary(:multiply, Scheme.mono(:float), Scheme.mono(:float))

      assert {:ok, :integer, :integer} =
               infer_binary(:add, Scheme.mono(:integer), Scheme.mono(:integer))
    end

    @tag obligations: ~w(NR-OBL-004)
    test "mixed numeric operands reject" do
      assert {:error, :type} =
               infer_binary(:add, Scheme.mono(:float), Scheme.mono(:integer))

      assert {:error, :type} =
               infer_binary(:multiply, Scheme.mono(:integer), Scheme.mono(:float))
    end

    @tag obligations: ~w(NR-OBL-002)
    test "ordering stays same-type over the closed set" do
      assert {:ok, :boolean, :float} =
               infer_binary(:less, Scheme.mono(:float), Scheme.mono(:float))

      assert {:ok, :boolean, :integer} =
               infer_binary(:greater, Scheme.mono(:integer), Scheme.mono(:integer))

      assert {:eq, :eq} = {Values.compare(-0.0, -0.0), Values.compare(1, 1)}
    end

    @tag obligations: ~w(NR-OBL-006 NR-OBL-008)
    test "determinism: repeated inference repeats" do
      first = infer_binary(:add, Scheme.mono(:float), Scheme.mono(:float))
      second = infer_binary(:add, Scheme.mono(:float), Scheme.mono(:float))
      assert first == second
    end
  end

  describe "unchanged behavior" do
    @tag obligations: ~w(NR-OBL-001 NR-OBL-006)
    test "Int arithmetic programs run unchanged on both targets" do
      {:ok, core} = Catena.check_kernel(@int_kernel)

      assert {:ok, 10, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, :C061Int, binary, _} = Catena.compile_kernel(@int_kernel)

      assert {:module, :C061Int} =
               :code.load_binary(:C061Int, ~c"c061_int.beam", binary)

      assert apply(:C061Int, :main, []) == 10

      {result, _trace} =
        Runtime.capture_trace(fn -> Catena.Kernel.Stepper.run(core, "main") end)

      assert {:ok, 10, %{root_status: :terminated}} = result

      on_exit(fn ->
        :code.purge(:C061Int)
        :code.delete(:C061Int)
      end)
    end
  end

  describe "exclusions" do
    @tag obligations: ~w(NR-OBL-003)
    test "no dispatch or overloadability entry points exist" do
      assert {:module, _} = Code.ensure_loaded(Catena.Type.Trait)
      refute function_exported?(Catena, :resolve_operator, 2)
      refute function_exported?(Catena.Type.Trait, :numeric_instance, 1)
      refute function_exported?(Catena, :overload, 2)
    end

    @tag obligations: ~w(NR-OBL-007)
    test "no divide or remainder operator exists" do
      for operator <- ["divide", "div", "remainder", "modulo", "/"] do
        assert {:error, %{}} = check_binary_program(operator)
      end
    end

    @tag obligations: ~w(NR-OBL-008)
    test "the closed set is exactly the numeric runtime types" do
      assert Values.value?(1.5)
      assert Values.value?(7)
      assert Catena.Data.comparable_type?(:float, sample_data())
      assert Catena.Data.comparable_type?(:integer, sample_data())
      refute Catena.Data.comparable_type?(:unit, sample_data())

      refute function_exported?(Catena, :decimal_type, 0)
      refute function_exported?(Catena, :bignum_type, 0)
    end
  end

  defp infer_binary(operator, left_scheme, right_scheme) do
    expression = %{
      tag: :binary,
      operator: operator,
      left: %{tag: :variable, name: "left", path: nil},
      right: %{tag: :variable, name: "right", path: nil},
      path: nil
    }

    environment = %{"left" => left_scheme, "right" => right_scheme}

    {typed, type, _state} =
      Infer.infer(expression, environment, %{next: 100, substitution: %{}})

    {:ok, type, typed.operand_type}
  rescue
    Catena.TypeError -> {:error, :type}
  end

  defp check_binary_program(operator) do
    program =
      JSON.encode!(%{
        "version" => "0.1.7",
        "edition" => "0.1",
        "language_revision" => "0.1.7",
        "previews" => [],
        "origin" => "test://c061/ops",
        "module" => "C061Ops",
        "source" => "c061.catena.json",
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
            "body" => %{
              "tag" => "binary",
              "operator" => operator,
              "left" => %{"tag" => "integer", "value" => 1},
              "right" => %{"tag" => "integer", "value" => 2}
            }
          }
        ],
        "effects" => [],
        "handlers" => []
      })

    Catena.check_json(program)
  end

  defp sample_data do
    Catena.Data.elaborate(
      %{
        origin: "test://c061",
        module: "Sample",
        types: [],
        type_groups: [],
        type_exports: [],
        imports: [],
        definitions: [],
        exports: [],
        traits: [],
        instances: [],
        effects: %{families: %{}, handlers: %{}}
      },
      []
    )
  end
end
