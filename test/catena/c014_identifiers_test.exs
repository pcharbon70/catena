defmodule Catena.C014IdentifiersTest do
  use ExUnit.Case, async: false

  import ExUnit.CaptureIO

  alias Catena.{Interface, LanguageLifecycle, LanguageVersion, QualifiedName, SourceSpan}

  @tag obligations: ~w(ID-OBL-001 ID-OBL-002 ID-OBL-003)
  test "Unicode 17 XID names are case-sensitive and role-neutral" do
    for {source, scripts} <- [
          {"alpha_1", ["Latn"]},
          {"Alpha_1", ["Latn"]},
          {"μεταβλητή", ["Grek"]},
          {"переменная", ["Cyrl"]},
          {"变量", ["Hani"]},
          {"कक्षा", ["Deva"]}
        ] do
      assert {:ok, identifier} = Catena.parse_identifier(source)
      assert identifier.canonical == source
      assert identifier.scripts == scripts
    end

    assert {:ok, lower} = Catena.parse_identifier("value")
    assert {:ok, upper} = Catena.parse_identifier("Value")
    refute lower.canonical == upper.canonical

    for source <- ["", "_value", "1value", "value-name", "value name", "value\nname"] do
      assert {:error, %{id: "IDN001"}} = Catena.parse_identifier(source)
    end
  end

  @tag obligations: ~w(ID-OBL-004 ID-OBL-011)
  test "identifiers must be NFC and receive an original-byte replacement fix" do
    assert {:ok, composed} = Catena.parse_identifier("éclair")
    assert composed.span == span(0, 7, 1, 1, 1, 7)

    assert {:error,
            %{
              id: "IDN002",
              span: %SourceSpan{byte_start: 0, byte_end: 8},
              details: %{normalization: "NFC", replacement: "éclair"},
              fixes: [fix]
            }} = Catena.parse_identifier("e\u0301clair")

    assert fix["kind"] == "source-edit"
    assert fix["text"] == "éclair"
    assert fix["range"].byte_end == 8
  end

  @tag obligations: ~w(ID-OBL-005 ID-OBL-006)
  test "the General Security and Highly Restrictive profiles are enforced per segment" do
    assert {:error, %{id: "IDN003", details: %{reason: "identifier_status_restricted"}}} =
             Catena.parse_identifier("a\u200C")

    assert {:error, %{id: "IDN004", details: %{scripts: ["Cyrl", "Latn"]}}} =
             Catena.parse_identifier("pаypal")

    assert {:error, %{id: "IDN004", details: %{scripts: ["Grek", "Latn"]}}} =
             Catena.parse_identifier("pαypal")

    for source <- ["日本語かなvalue", "한국value"] do
      assert {:ok, _east_asian} = Catena.parse_identifier(source)
    end

    assert Catena.UnicodeData.highly_restrictive?("注音ㄅvalue")

    assert {:ok, _single_script} = Catena.parse_identifier("ѕсоре")
  end

  @tag obligations: ~w(ID-OBL-007 ID-OBL-008)
  test "the closed keyword set is hard-reserved and backticks preserve identifier identity" do
    assert QualifiedName.keywords() ==
             ~w(
               as condition derives effect exists false fn forall handle handler import let match
               or request resume returns true type uses when where with
             )

    for keyword <- QualifiedName.keywords() do
      assert {:error, %{id: "IDN005", details: %{keyword: ^keyword}}} =
               Catena.parse_identifier(keyword)

      assert {:ok, escaped} = Catena.parse_identifier("`#{keyword}`")
      assert escaped.canonical == keyword
      assert escaped.escaped
    end

    assert {:ok, redundant} = Catena.parse_identifier("`ordinary`")
    assert redundant.canonical == "ordinary"

    for malformed <- ["``", "`name", "name`", "`name`tail", "`na`me`"] do
      assert {:error, %{id: "IDN005"}} = Catena.parse_identifier(malformed)
    end
  end

  @tag obligations: ~w(ID-OBL-009)
  test "qualification is a nonempty dot-separated sequence of independently checked segments" do
    assert {:ok, qualified} = Catena.parse_qualified_name("Option.`type`.value")
    assert qualified.canonical == "Option.type.value"
    assert Enum.map(qualified.segments, & &1.escaped) == [false, true, false]
    assert qualified.span == span(0, 19, 1, 1, 1, 20)

    assert {:ok, single} = Catena.parse_qualified_name("single")
    assert length(single.segments) == 1

    for malformed <- ["", ".name", "name.", "name..part"] do
      assert {:error, %{id: "IDN006"}} = Catena.parse_qualified_name(malformed)
    end

    assert {:error, %{id: "IDN001"}} = Catena.parse_qualified_name("name .part")
    assert {:error, %{id: "IDN006"}} = Catena.parse_identifier("name.part")
  end

  @tag obligations: ~w(ID-OBL-010)
  test "confusable skeleton collisions are deterministic deny-able warnings" do
    assert {:ok, names, [warning]} = Catena.audit_identifiers(["scope", "ѕсоре"])
    assert Enum.map(names, & &1.canonical) == ["scope", "ѕсоре"]
    assert warning.id == "IDN007"
    assert warning.severity == :warning
    assert warning.details.confusable_with == "scope"
    assert warning.details.unicode_version == "17.0.0"
    assert Catena.UnicodeData.skeleton("a\u200C") == "a"

    assert {:ok, _duplicates, []} = Catena.audit_identifiers(["scope", "scope"])
    assert {:ok, _distinct, []} = Catena.audit_identifiers(["Alpha.name", "Beta.name"])

    assert {:error, %{id: "IDN007", severity: :error, details: %{promoted_from_warning: true}}} =
             Catena.audit_identifiers(["scope", "ѕсоре"], denied_diagnostics: ["IDN007"])
  end

  @tag obligations: ~w(ID-OBL-001 ID-OBL-012 ID-OBL-013)
  test "0.1.10 is an identifier-only revision with deterministic CLI discovery" do
    assert LanguageVersion.latest() == "0.1.22"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22)

    refute "0.1.10" in LanguageVersion.compilable_revisions()
    refute "0.1.10" in LanguageVersion.interface_versions()
    refute "0.1.10" in LanguageVersion.artifact_versions()
    refute "0.1.10" in LanguageVersion.signed_format_versions()
    assert {:ok, :stable} == LanguageLifecycle.state("identifiers", "0.1.10")

    assert {:error, %{id: "EDN001", details: %{frontend: "identifiers"}}} =
             Catena.parse_identifier("name", language_selection: selection("0.1.9"))

    assert {:error, _diagnostic} = Interface.decode(forged_interface())

    first = capture_io(fn -> Catena.CLI.main(["check-identifiers", "Option.name", "ѕсоре"]) end)
    second = capture_io(fn -> Catena.CLI.main(["check-identifiers", "Option.name", "ѕсоре"]) end)
    assert first == second
    assert {:ok, output} = JSON.decode(String.trim(first))
    assert output["language_revision"] == "0.1.10"
    assert output["unicode_version"] == "17.0.0"
    assert Enum.map(output["names"], & &1["canonical"]) == ["Option.name", "ѕсоре"]
  end

  @tag obligations: ~w(ID-OBL-001 ID-OBL-012)
  test "the packaged escript carries its pinned Unicode table" do
    Mix.Task.reenable("escript.build")
    assert :ok = Mix.Task.run("escript.build", ["--force"])

    {output, exit_status} =
      System.cmd(Path.expand("catena"), ["check-identifiers", "alpha", "Option.Some"])

    assert exit_status == 0
    assert {:ok, decoded} = JSON.decode(String.trim(output))
    assert decoded["unicode_version"] == "17.0.0"
    assert Enum.map(decoded["names"], & &1["canonical"]) == ["alpha", "Option.Some"]
  end

  @tag obligations: ~w(ID-OBL-001 ID-OBL-004)
  test "the complete Unicode 17 normalization corpus agrees with Catena NFC" do
    failures =
      unicode_path("NormalizationTest.txt")
      |> File.stream!()
      |> Stream.map(&(&1 |> String.split("#", parts: 2) |> hd() |> String.trim()))
      |> Stream.reject(&(&1 == "" or String.starts_with?(&1, "@")))
      |> Enum.reduce([], fn line, failures ->
        [c1, c2, c3, c4, c5 | _] = line |> String.split(";") |> Enum.map(&String.trim/1)
        [s1, s2, s3, s4, s5] = Enum.map([c1, c2, c3, c4, c5], &scalars/1)

        valid =
          Catena.UnicodeData.nfc(s1) == s2 and Catena.UnicodeData.nfc(s2) == s2 and
            Catena.UnicodeData.nfc(s3) == s2 and Catena.UnicodeData.nfc(s4) == s4 and
            Catena.UnicodeData.nfc(s5) == s4

        if valid, do: failures, else: [line | failures]
      end)

    assert failures == []

    manifest = Catena.UnicodeData.source_manifest()

    assert Map.keys(manifest) |> Enum.sort() ==
             ~w(
               DerivedCoreProperties.txt DerivedNormalizationProps.txt IdentifierStatus.txt
               NormalizationTest.txt PropertyValueAliases.txt ScriptExtensions.txt Scripts.txt
               UnicodeData.txt confusables.txt
             )
             |> Enum.sort()

    assert Enum.all?(manifest, fn {_name, source} ->
             String.length(source.sha256) == 64 and String.starts_with?(source.url, "https://") and
               String.contains?(source.url, "/17.0.0/")
           end)
  end

  defp unicode_path(name) do
    Path.join([:code.priv_dir(:catena) |> List.to_string(), "unicode", "17.0.0", name])
  end

  defp scalars(field) do
    field |> String.split() |> Enum.map(&String.to_integer(&1, 16)) |> List.to_string()
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp forged_interface do
    %{
      "format" => "catena-interface",
      "version" => "0.1.10",
      "edition" => "0.1",
      "language_revision" => "0.1.10",
      "previews" => [],
      "required_previews" => [],
      "origin" => "test://c014-forged",
      "module" => "C014Forged",
      "types" => [],
      "values" => [],
      "digest" => String.duplicate("0", 64)
    }
    |> JSON.encode!()
  end

  defp span(byte_start, byte_end, line_start, column_start, line_end, column_end) do
    %SourceSpan{
      byte_start: byte_start,
      byte_end: byte_end,
      line_start: line_start,
      column_start: column_start,
      line_end: line_end,
      column_end: column_end
    }
  end
end
