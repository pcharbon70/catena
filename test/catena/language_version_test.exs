defmodule Catena.LanguageVersionTest do
  use ExUnit.Case, async: true

  alias Catena.{Assurance, CanonicalJCS, Governance, Interface, LanguageVersion}
  alias Catena.Categorical.Standard
  alias Catena.Governance.TrustRoot
  alias Catena.Package.Manifest

  test "prototype slices use one ordered 0.1 patch sequence" do
    assert LanguageVersion.all() ==
             ~w(0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7 0.1.8 0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24)

    assert LanguageVersion.json_frontend_versions() ==
             ~w(0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7)

    assert LanguageVersion.kernel_frontend_versions() == ["0.1.8"]

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24)

    assert LanguageVersion.compilable_revisions() ==
             ~w(0.1.1 0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7 0.1.8)

    assert LanguageVersion.interface_versions() ==
             ~w(0.1.2 0.1.3 0.1.4 0.1.5 0.1.6 0.1.7 0.1.8)

    assert LanguageVersion.artifact_versions() == LanguageVersion.interface_versions()
    assert LanguageVersion.signed_format_versions() == ~w(0.1.6 0.1.7 0.1.8)

    assert LanguageVersion.compilable_from(:specifications_and_governance) ==
             ~w(0.1.6 0.1.7 0.1.8)

    assert LanguageVersion.latest() == "0.1.24"
    assert LanguageVersion.internal_representation("0.1.1") == "0.1.2"
    assert LanguageVersion.default_artifact_version("0.1.1", "0.1.1") == "0.1.2"
    assert LanguageVersion.default_artifact_version("0.1.6", "0.1.6") == "0.1.6"
    assert LanguageVersion.default_artifact_version("0.1.7", "0.1.1") == "0.1.7"
    assert Enum.all?(LanguageVersion.all(), &LanguageVersion.valid_core_semver?/1)
    refute LanguageVersion.valid_core_semver?("0.6")
    refute LanguageVersion.valid_core_semver?("0.1.6-preview")
    assert Application.spec(:catena, :vsn) == ~c"0.1.0"
  end

  test "every current AST slice is accepted and every retired identifier is rejected" do
    for version <- LanguageVersion.json_frontend_versions() do
      document =
        %{
          "version" => version,
          "module" => "Version#{String.replace(version, ".", "")}",
          "exports" => [],
          "definitions" => []
        }
        |> maybe_put_origin(version)

      assert {:ok, core} = document |> JSON.encode!() |> Catena.check_json()
      assert core.frontend_version == version
    end

    for version <- LanguageVersion.retired() do
      document = %{
        "version" => version,
        "module" => "Retired",
        "exports" => [],
        "definitions" => []
      }

      assert {:error, %{id: "T012", path: "$.version"}} =
               document |> JSON.encode!() |> Catena.check_json()
    end
  end

  test "retired identifiers are rejected at every persisted protocol boundary" do
    for version <- LanguageVersion.retired() do
      assert {:error, _diagnostic} =
               %{"format" => "catena-interface", "version" => version}
               |> JSON.encode!()
               |> Interface.decode()

      assert {:error, %{id: "LNK001"}} =
               %{"format" => "catena-package-manifest", "version" => version}
               |> JSON.encode!()
               |> Manifest.decode()
    end

    retired_governance = %{"format" => "catena-governance-bundle", "version" => "0.6"}
    retired_root = %{"format" => "catena-trust-root", "version" => "0.6"}
    retired_assurance = %{"format" => "catena-assurance-manifest", "version" => "0.6"}

    assert {:error, %{id: "GOV001"}} =
             retired_governance |> CanonicalJCS.encode() |> Governance.decode_bundle()

    assert {:error, %{id: "GOV005"}} =
             retired_root |> CanonicalJCS.encode() |> TrustRoot.decode()

    assert {:error, %{id: "ART001"}} =
             retired_assurance |> CanonicalJCS.encode() |> Assurance.verify(".", nil)
  end

  test "canonical identities and signature domains use their introducing patch" do
    standard = Standard.interface!()

    assert standard["version"] == "0.1.4"
    assert standard["origin"] == "catena://standard/0.1.4"

    assert standard["digest"] ==
             "c841bf5b4cbdbf8969ccf5375bc327ae106eb222671595b29ea49acd5b2f1013"

    for kind <- ~w(root delegation evidence approval transition manifest) do
      assert CanonicalJCS.payload(kind, %{}) == "catena:#{kind}:0.1.6\n{}"
      assert CanonicalJCS.payload(kind, "0.1.7", %{}) == "catena:#{kind}:0.1.7\n{}"
      refute CanonicalJCS.payload(kind, %{}) == "catena:#{kind}:0.6\n{}"
    end
  end

  defp maybe_put_origin(document, "0.1.1"), do: document
  defp maybe_put_origin(document, _version), do: Map.put(document, "origin", "test://version")
end
