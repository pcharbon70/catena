defmodule Catena.C066NameResolutionTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion}

  @frontends ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46 0.1.47)

  @shadow_kernel """
  (module C066Shadow
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c066/shadow")
    (export value inner)
    (export value outer)
    (export value main)
    (def inner
      (signature (Fn Int (effects) Int) (uses))
      (fn (ignored Int)
        (let x 1
          (let x 2
            (var x)))))
    (def outer
      (signature (Fn Int (effects) Int) (uses))
      (fn (ignored Int)
        (let x 1
          (add (var x) 10))))
    (def main
      (signature Int (uses))
      (add (call (var inner) 0) (call (var outer) 0))))
  """

  @trait_kernel """
  (module C066Trait
    (edition 0.1)
    (revision 0.1.8)
    (origin "test://c066/trait")
    (export value reveal_int)
    (export value main)
    (trait Reveal
      (parameter a)
      (method reveal (Fn a (effects) Int)))
    (instance Reveal Int
      (method reveal reveal_int))
    (def reveal_int
      (signature (Fn Int (effects) Int) (uses))
      (fn (n Int) (add (var n) 1)))
    (def main
      (signature Int (uses))
      (trait-call Reveal reveal 41)))
  """

  describe "revision registration" do
    @tag obligations: ~w(RN-OBL-001)
    test "0.1.42 is an exact registered revision with predecessors pinned" do
      assert LanguageVersion.latest() == "0.1.47"
      assert LanguageVersion.source_text_frontend_versions() == @frontends
      refute "0.1.42" in LanguageVersion.compilable_revisions()
      refute "0.1.42" in LanguageVersion.artifact_versions()
      refute "0.1.42" in LanguageVersion.signed_format_versions()

      assert {:ok, :stable} == LanguageLifecycle.state("name-resolution", "0.1.42")

      change =
        Enum.find(
          LanguageLifecycle.changes(),
          &(&1["id"] == "change-0-1-42-name-resolution")
        )

      assert change["affects"] == ~w(static-meaning)

      assert String.contains?(
               change["specification"],
               "name-resolution/the-resolution-invariant.md#"
             )

      assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

      assert {:ok, %{selection: %{language_revision: "0.1.47"}}} =
               Catena.decode_source_text("")

      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :resolve_by_type, 2)
      refute function_exported?(Catena, :overload_candidates, 1)
      refute function_exported?(Catena, :disambiguate_at_call_site, 2)
    end
  end

  describe "the resolution invariant" do
    @tag obligations: ~w(RN-OBL-002 RN-OBL-007)
    test "annotation invariance: signatures never move a name's target" do
      annotated =
        String.replace(
          @shadow_kernel,
          "(def inner\n      (signature Int (uses))",
          "(def inner\n      (signature Int (uses))"
        )

      assert {:ok, core} = Catena.check_kernel(@shadow_kernel)
      assert {:ok, twin_core} = Catena.check_kernel(annotated)

      assert {:ok, first, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, second, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(twin_core, "main")

      assert first == second
      assert first == 13
    end

    @tag obligations: ~w(RN-OBL-007)
    test "scope-structure resolution: shadowing resolves by scope, not type" do
      assert {:ok, core} = Catena.check_kernel(@shadow_kernel)

      assert {:ok, 13, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")
    end

    @tag obligations: ~w(RN-OBL-004)
    test "evidence selection settles at the instance, never at call sites" do
      assert {:ok, core} = Catena.check_kernel(@trait_kernel)

      assert {:ok, 42, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      without_instance =
        String.replace(@trait_kernel, "(instance Reveal Int", "(unused Reveal Int")

      assert {:error, %{}} = Catena.check_kernel(without_instance)

      ambiguous = """
      (module C066Ambiguous
        (edition 0.1)
        (revision 0.1.8)
        (origin "test://c066/ambiguous")
        (export value main)
        (trait Reveal
          (parameter a)
          (method reveal (Fn a (effects) Int)))
        (def main
          (signature Int (uses))
          (trait-call Reveal reveal 41)))
      """

      assert {:error, _} = Catena.check_kernel(ambiguous)
    end

    @tag obligations: ~w(RN-OBL-003)
    test "the five classes: literals by spelling, constructors by visibility" do
      assert {:ok, %{literal: %{payload: payload}}} = Catena.scan_literal("1.0")
      assert {:ok, %{value: 1.0}} = Catena.elaborate_numeric_literal(payload)
      assert {:ok, %{literal: %{payload: int_payload}}} = Catena.scan_literal("1")
      assert {:ok, %{value: 1}} = Catena.elaborate_numeric_literal(int_payload)

      typo = typo_constructor_program()
      assert {:error, %{id: "A004"}} = Catena.check_json(typo)
    end
  end

  describe "exclusions and determinism" do
    @tag obligations: ~w(RN-OBL-005 RN-OBL-006)
    test "no type-directed selection entry points exist" do
      assert {:module, _} = Code.ensure_loaded(Catena)
      refute function_exported?(Catena, :resolve_overloads, 2)
      refute function_exported?(Catena, :adapt_literal_to_type, 2)
      refute function_exported?(Catena, :infer_field_name, 2)
      refute function_exported?(Catena, :type_directed_lookup, 3)
    end

    @tag obligations: ~w(RN-OBL-008)
    test "determinism: repeated resolution repeats" do
      assert {:ok, core} = Catena.check_kernel(@shadow_kernel)

      assert {:ok, first, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert {:ok, second, %{root_status: :terminated}} =
               Catena.Kernel.Stepper.run(core, "main")

      assert first == second
    end

    @tag obligations: ~w(RN-OBL-007)
    test "import collision rejects rather than disambiguating by type" do
      {:ok, first} =
        Catena.check_kernel("""
        (module C066CollideA
          (edition 0.1)
          (revision 0.1.8)
          (origin "test://c066/a")
          (export value shared)
          (def shared
            (signature Int (uses))
            1))
        """)

      {:ok, second} =
        Catena.check_kernel("""
        (module C066CollideB
          (edition 0.1)
          (revision 0.1.8)
          (origin "test://c066/b")
          (export value shared)
          (def shared
            (signature Bool (uses))
            true))
        """)

      assert is_map(first)
      assert is_map(second)

      {:error, _} =
        Catena.check_json(import_collision_program())
    end
  end

  defp typo_constructor_program do
    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c066/inaccessible",
      "module" => "C066Hidden",
      "source" => "c066.catena.json",
      "exports" => ["main"],
      "type_exports" => [%{"name" => "Email", "visibility" => "abstract"}],
      "type_groups" => [
        %{
          "declarations" => [
            %{
              "name" => "Email",
              "parameters" => [],
              "constructors" => [
                %{"name" => "Email", "fields" => [%{"tag" => "integer"}], "existentials" => []}
              ],
              "derivations" => []
            }
          ]
        }
      ],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{
            "forall" => [],
            "type" => %{"tag" => "named", "name" => "Email", "arguments" => []},
            "uses" => []
          },
          "body" => %{
            "tag" => "construct",
            "constructor" => "Email.Typo",
            "arguments" => [%{"tag" => "integer", "value" => 1}]
          }
        }
      ],
      "effects" => [],
      "handlers" => []
    })
  end

  defp import_collision_program do
    JSON.encode!(%{
      "version" => "0.1.7",
      "edition" => "0.1",
      "language_revision" => "0.1.7",
      "previews" => [],
      "origin" => "test://c066/collide",
      "module" => "C066Collide",
      "source" => "c066.catena.json",
      "exports" => ["main"],
      "type_exports" => [],
      "type_groups" => [],
      "types" => [],
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "imports" => [
        %{"module" => "C066CollideA", "names" => ["shared"]},
        %{"module" => "C066CollideB", "names" => ["shared"]}
      ],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}, "uses" => []},
          "body" => %{"tag" => "integer", "value" => 0}
        }
      ],
      "effects" => [],
      "handlers" => []
    })
  end
end
