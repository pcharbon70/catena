defmodule Catena.C067DynamicUnsafeTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47 0.1.48)

  @standard_chunks ~w(AtU8 Code StrT ImpT ExpT FunT LitT LocT Attr CInf Dbgi Line Type)c

  describe "revision registration" do
    @tag obligations: ~w(DU-OBL-001)
    test "0.1.43 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.48"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.43" in LanguageVersion.compilable_revisions()
      refute "0.1.43" in LanguageVersion.artifact_versions()
      refute "0.1.43" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} ==
               LanguageLifecycle.state("dynamic-and-unsafe-boundaries", "0.1.43")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-43-dynamic-and-unsafe-boundaries")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "dynamic-and-unsafe-boundaries/the-intralanguage-exclusions.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.48"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :cast, 2)
      refute function_exported?(Catena, :typecase, 1)
      refute function_exported?(Catena, :type_of, 1)
    end
  end

  describe "the intralanguage exclusions" do
    @tag obligations: ~w(DU-OBL-002 DU-OBL-005)
    test "no cast, inspection, intrinsic, or reflection entry points exist" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :unsafe_cast, 2)
      refute function_exported?(Catena, :checked_cast, 2)
      refute function_exported?(Catena, :dynamic_test, 1)
      refute function_exported?(Catena, :reflect, 1)
      refute function_exported?(Catena, :intrinsic, 1)
      refute function_exported?(Catena, :unchecked, 1)

      cast_program = unknown_expression_program("cast")
      typecase_program = unknown_expression_program("typecase")

      for program <- [cast_program, typecase_program] do
        assert {:error, _} = Catena.check_json(program)
      end
    end

    @tag obligations: ~w(DU-OBL-003)
    test "the guard fragment still rejects what it always rejected" do
      for guard <- [let_guard(), call_guard()] do
        assert {:error, %{}} = Catena.check_json(guard_module(guard))
      end

      valid =
        guard_module(%{
          "tag" => "binary",
          "operator" => "equal",
          "left" => var("value"),
          "right" => int(1)
        })

      assert {:ok, _} = Catena.check_json(valid)
    end

    @tag obligations: ~w(DU-OBL-007)
    test "erasure holds: compiled artifacts carry no specification or governance chunks" do
      source = File.read!("test/fixtures/c010-kernel.catena")
      {:ok, _module, binary, _metadata} = Catena.compile_kernel(source)

      info = :beam_lib.info(binary)
      chunk_names = info[:chunks] |> Enum.map(&elem(&1, 0))

      assert MapSet.subset?(MapSet.new(chunk_names), MapSet.new(@standard_chunks))

      for forbidden <- ~w(Spec CatenaSpec Governance Evidence Policy Approval)c do
        refute forbidden in chunk_names
      end
    end

    @tag obligations: ~w(DU-OBL-008)
    test "no dyn, any, or unknown type spelling exists" do
      for tag <- ["dyn", "any", "unknown", "dynamic"] do
        assert {:error, _} = Catena.check_json(dyn_typed_program(tag))
      end

      assert {:module, _} = Code.ensure_loaded(Catena.Values)
      refute function_exported?(Catena.Values, :dyn?, 1)
    end
  end

  describe "the foreign visibility routing" do
    @tag obligations: ~w(DU-OBL-004 DU-OBL-005)
    test "no foreign or dynamic entry path exists before its owning slice" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :from_erlang, 1)
      refute function_exported?(Catena, :foreign_call, 2)
      refute function_exported?(Catena, :nif, 1)
      refute function_exported?(Catena, :port, 1)
    end

    @tag obligations: ~w(DU-OBL-006)
    test "the arrival conditions are the only amendment route" do
      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-43-dynamic-and-unsafe-boundaries")
        )

      assert change["summary"] =~ "dynamic and unsafe boundaries"
      assert change["affects"] == ~w(static-meaning)
      assert is_map(change)
    end

    @tag obligations: ~w(DU-OBL-002 DU-OBL-008)
    test "determinism: unchanged programs keep identical artifacts" do
      source = File.read!("test/fixtures/c010-kernel.catena")

      {:ok, _, first, _} = Catena.compile_kernel(source)
      {:ok, _, second, _} = Catena.compile_kernel(source)
      assert first == second
    end
  end

  defp let_guard do
    %{
      "tag" => "let",
      "name" => "y",
      "value" => int(1),
      "body" => bool(true)
    }
  end

  defp call_guard do
    %{
      "tag" => "call",
      "callee" => %{"tag" => "variable", "name" => "value"},
      "arguments" => []
    }
  end

  defp guard_module(guard) do
    JSON.encode!(%{
      "version" => "0.1.3",
      "origin" => "test://c067/guard",
      "module" => "C067Guard",
      "type_groups" => [],
      "type_exports" => [],
      "imports" => [],
      "exports" => ["classify"],
      "definitions" => [
        %{
          "name" => "classify",
          "signature" => %{
            "forall" => [],
            "type" => %{
              "tag" => "function",
              "parameter" => %{"tag" => "integer"},
              "result" => %{"tag" => "integer"},
              "effect" => []
            }
          },
          "clauses" => [
            %{"patterns" => [bind("value")], "guard" => guard, "body" => int(1)},
            %{"patterns" => [bind("value")], "body" => int(0)}
          ]
        }
      ]
    })
  end

  defp unknown_expression_program(tag) do
    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c067/unknown",
      "module" => "C067Unknown",
      "source" => "c067.catena.json",
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

  defp dyn_typed_program(tag) do
    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c067/dyn",
      "module" => "C067Dyn",
      "source" => "c067.catena.json",
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
          "signature" => %{"forall" => [], "type" => %{"tag" => tag}, "uses" => []},
          "body" => int(0)
        }
      ],
      "effects" => [],
      "handlers" => []
    })
  end

  defp int(value), do: %{"tag" => "integer", "value" => value}
  defp bool(value), do: %{"tag" => "boolean", "value" => value}
  defp var(name), do: %{"tag" => "variable", "name" => name}
  defp bind(name), do: %{"tag" => "bind", "name" => name}
end
