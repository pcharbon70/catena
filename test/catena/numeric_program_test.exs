defmodule Catena.NumericProgramTest do
  use ExUnit.Case, async: false
  alias Catena.Standard.Numeric, as: N
  alias Catena.Standard.Numeric.{Program, Decimal, Package}
  alias Catena.Standard.Outcomes, as: O
  @limits %{nodes: 10000, bytes: 100_000, depth: 1000}
  @moduletag obligations: ~w(NL-OBL-001 NL-OBL-009 NL-OBL-010 NL-OBL-011 NL-OBL-012)
  defp tree(type, name) do
    {:ok, parsed} =
      Catena.Kernel.Parser.parse("""
      (module #{name} (edition 0.1) (revision 0.1.8) (origin "test://numeric-program/#{name}")
        (export value work)
        (def work (signature (Fn Int (effects) Int) (uses)) (fn (value Int) (var value))))
      """)

    {:ok, core} = Catena.ValueBoundary.Kernel.check(replace(parsed, type))
    core
  end

  defp replace(:integer, type), do: type
  defp replace(%Catena.SourceSpan{} = span, _), do: span

  defp replace(map, type) when is_map(map),
    do: Map.new(map, fn {k, v} -> {k, replace(v, type)} end)

  defp replace(tuple, type) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.map(&replace(&1, type)) |> List.to_tuple()

  defp replace(list, type) when is_list(list), do: Enum.map(list, &replace(&1, type))
  defp replace(value, _), do: value

  defp unload(module),
    do:
      (
        :code.delete(module)
        :code.purge(module)
      )

  test "checked value-tree numeric operations agree in reference and compiled execution" do
    {:ok, context} = Decimal.context(8, 2, :half_even)

    cases = [
      {{:tuple, [:integer, :integer]}, [:euclidean], {-23, -7}, {4, 5}},
      {{:tuple, [:float, :float]}, [{:float, :divide}, :sqrt], {18.0, 2.0}, 3.0},
      {:integer, [{:int_to_float, :exact}, :format_float, :parse_float], 42, 42.0},
      {{:tuple, [:integer, :integer]},
       [:decimal, {:rescale, context}, {:decimal_to_float, :exact}], {125, 2}, 1.25},
      {:float, [{:float_to_decimal, context}, {:decimal_to_float, :nearest_even}], 0.1, 0.1}
    ]

    for {{type, steps, value, expected}, i} <- Enum.with_index(cases) do
      core = tree(type, "NumericPipeline#{i}")
      assert {:ok, artifact} = Program.build(core, "work", steps, @limits)

      try do
        assert Program.reference(core, "work", steps, value, @limits) ==
                 {:ok, O.success(expected)}

        assert Program.invoke(artifact, core, "work", steps, value, @limits) ==
                 {:ok, O.success(expected)}

        assert Program.invoke(artifact, core, "work", steps, value, @limits) ==
                 {:ok, O.success(expected)}

        changed = put_in(artifact.sidecar.profile.rounding, :toward_zero)

        assert {:error, :unverified_numeric_program} =
                 Program.verify(changed, core, "work", steps, @limits)
      after
        unload(artifact.module)
      end
    end
  end

  test "dependent failures stop before later operations and incompatible pipelines are refused" do
    core = tree({:tuple, [:float, :float]}, "NumericFailure")
    steps = [{:float, :divide}, :sqrt, :format_float]
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      expected = {:ok, O.failure(N.failure(:zero_divisor))}
      assert Program.reference(core, "work", steps, {1.0, 0.0}, @limits) == expected
      assert Program.invoke(artifact, core, "work", steps, {1.0, 0.0}, @limits) == expected
      assert {:error, _} = Program.invoke(artifact, core, "work", steps, {1, 0.0}, @limits)
      {:ok, other} = Program.build(core, "work", [{:float, :add}], @limits)

      assert {:error, :numeric_module_conflict} =
               Program.invoke(other, core, "work", [{:float, :add}], {1.0, 2.0}, @limits)
    after
      unload(artifact.module)
    end

    assert {:error, :ill_typed_numeric_step} = Program.build(core, "work", [:sqrt], @limits)

    assert {:error, :ill_typed_numeric_step} =
             Program.build(core, "work", [{:integer, :add}], @limits)

    float = tree(:float, "NumericLimit")
    assert {:ok, _} = Program.build(float, "work", List.duplicate(:sqrt, 253), @limits)
    assert {:error, _} = Program.build(float, "work", List.duplicate(:sqrt, 254), @limits)
  end

  test "decimal constructors and elimination compile as ordinary nominal data" do
    assert :ok = Package.verify(Package.package!())

    assert {:error, :invalid_numeric_package} =
             Package.verify(Map.put(Package.package!(), "extra", true))

    assert {:ok, module, binary, _} = Package.compile()

    try do
      {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"numeric-package", binary)
      value = apply(module, :decimal, [125, 2])
      assert {:ok, ^value} = Decimal.new(125, 2)
      assert apply(module, :decimal_parts, [value]) == {125, 2}
    after
      unload(module)
    end
  end

  test "ordinary source computation precedes numeric operations in both execution paths" do
    source =
      JSON.encode!(%{
        "version" => "0.1.3",
        "origin" => "test://ordinary-numeric-pipeline",
        "module" => "OrdinaryNumericOrdinary",
        "exports" => ["work"],
        "definitions" => [
          %{
            "name" => "work",
            "parameters" => ["value"],
            "signature" => %{
              "forall" => [],
              "type" => %{
                "tag" => "function",
                "parameter" => %{"tag" => "integer"},
                "result" => %{"tag" => "integer"}
              }
            },
            "body" => %{
              "tag" => "binary",
              "operator" => "add",
              "left" => %{"tag" => "variable", "name" => "value"},
              "right" => %{"tag" => "integer", "value" => 1}
            }
          }
        ]
      })

    {:ok, core} = Catena.check_json(source)
    steps = [{:int_to_float, :exact}, :sqrt, :format_float]
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      expected = {:ok, O.success("10.0")}
      assert Program.reference(core, "work", steps, 99, @limits) == expected
      assert Program.invoke(artifact, core, "work", steps, 99, @limits) == expected
    after
      unload(artifact.module)
    end
  end

  test "explicit primitive arithmetic traps while checked overflow returns" do
    core = tree({:tuple, [:float, :float]}, "NumericPrimitiveFault")
    steps = [{:primitive, :multiply}]
    {:ok, maximum} = Catena.Standard.Numeric.Binary64.from_bits(0x7FEFFFFFFFFFFFFF)
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      reason = Catena.Standard.Numeric.Primitive.reason()

      assert Program.reference(core, "work", steps, {maximum, maximum}, @limits) ==
               {:trap, reason}

      assert catch_error(
               Program.invoke(artifact, core, "work", steps, {maximum, maximum}, @limits)
             ) == {:catena_trap, reason}

      assert N.run({:float, :multiply}, {maximum, maximum}, @limits) ==
               {:ok, O.failure(N.failure(:overflow))}
    after
      unload(artifact.module)
    end
  end

  test "compiled decimal pair arithmetic rounds once and remains an ordinary value" do
    {:ok, context} = Decimal.context(8, 2, :half_even)
    core = tree({:tuple, [:integer, :integer, :integer, :integer]}, "NumericDecimalPair")
    steps = [{:decimal_pair, :divide, context}, :decimal_parts]
    {:ok, artifact} = Program.build(core, "work", steps, @limits)

    try do
      expected = {:ok, O.success({33, 2})}
      assert Program.reference(core, "work", steps, {1, 0, 3, 0}, @limits) == expected
      assert Program.invoke(artifact, core, "work", steps, {1, 0, 3, 0}, @limits) == expected
    after
      unload(artifact.module)
    end
  end

  test "effectful source entries cannot be disguised as pure numeric pipelines" do
    {:ok, core} =
      Catena.check_kernel("""
      (module NumericEffectRefusal (edition 0.1) (revision 0.1.8) (origin "test://numeric-effect")
        (effect E (operation get (params) Int))
        (export value work)
        (def work (signature (Fn Int (effects E) Int) (uses))
          (fn (value Int) (request E get))))
      """)

    assert {:error, _} = Program.build(core, "work", [{:int_to_float, :exact}], @limits)
  end
end
