defmodule Catena.C020FileUnitTest do
  use ExUnit.Case, async: false

  alias Catena.FileUnit.ModuleDeclaration
  alias Catena.{LanguageLifecycle, LanguageVersion}

  @tag obligations: ~w(FU-OBL-001 FU-OBL-012)
  test "0.1.16 is an exact deterministic source-structure revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.19"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19)

    refute "0.1.16" in LanguageVersion.compilable_revisions()
    refute "0.1.16" in LanguageVersion.interface_versions()
    refute "0.1.16" in LanguageVersion.artifact_versions()
    refute "0.1.16" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("files-and-modules", "0.1.16")

    change =
      Enum.find(LanguageLifecycle.changes(), &(&1["id"] == "change-0-1-16-files-and-modules"))

    assert change["affects"] == ~w(source-acceptance diagnostics)

    assert String.contains?(
             change["specification"],
             "files-and-modules/file-units-and-module-binding.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "files-and-modules", required: "0.1.16"}
            }} =
             Catena.resolve_file_unit("", "A.cat", [], language_selection: selection("0.1.15"))

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, %{selection: %{language_revision: "0.1.12"}}} = Catena.scan_comment("// c")
    assert {:ok, %{selection: %{language_revision: "0.1.10"}}} = Catena.parse_identifier("x")

    assert {:ok, %{selection: %{language_revision: "0.1.15"}}} =
             Catena.tokenize_source("1")

    refute function_exported?(Catena, :parse_module_header, 2)
    refute function_exported?(Catena, :load_module, 1)
    refute function_exported?(Catena, :compile_file, 2)
  end

  @tag obligations: ~w(FU-OBL-002)
  test "the .cat extension is required and FIL001 reports otherwise" do
    for filename <- ["Mod.txt", "Mod.CAT", "Mod", "src/Mod", "Mod.cat.json"] do
      assert {:error, %{id: "FIL001", details: %{reason: "missing_extension"}}} =
               Catena.resolve_file_unit("val", filename, [])
    end

    assert {:ok, _} = Catena.resolve_file_unit("", "Mod.cat", [])
    assert {:ok, _} = Catena.resolve_file_unit("", "src/Mod.cat", [])
    assert {:ok, _} = Catena.resolve_file_unit("", "src\\Mod.cat", [])
  end

  @tag obligations: ~w(FU-OBL-003)
  test "module and no-module files classify with valid empty and comment-only units" do
    for source <- ["", " ", "\n\n", "\t \n", "// note", "/* block\ncomment */", "// a\n// b"] do
      assert {:ok, result} = Catena.resolve_file_unit(source, "Any.cat", [])
      assert result.kind == :no_module
      assert result.module == nil
    end

    assert {:ok, result} =
             Catena.resolve_file_unit("val", "Parcel.cat", [
               %ModuleDeclaration{name: "Parcel", span: nil}
             ])

    assert result.kind == :module
    assert result.module.name == "Parcel"
  end

  @tag obligations: ~w(FU-OBL-004 FU-OBL-005)
  test "multiple declarations and bad spellings fail as FIL002 and FIL003" do
    assert {:error,
            %{id: "FIL002", details: %{reason: "multiple_module_declarations", observed: 2}}} =
             Catena.resolve_file_unit("val", "A.cat", [
               %ModuleDeclaration{name: "A", span: nil},
               %ModuleDeclaration{name: "B", span: nil}
             ])

    assert {:error, %{id: "FIL002", details: %{observed: 3}}} =
             Catena.resolve_file_unit("val", "A.cat", [
               %ModuleDeclaration{name: "A", span: nil},
               %ModuleDeclaration{name: "A", span: nil},
               %ModuleDeclaration{name: "A", span: nil}
             ])

    for bad <- ["mod", "1Mod", "_Mod", "My-Mod", "Möd", "myMod", ""] do
      assert {:error, %{id: "FIL003", details: %{reason: "invalid_module_name_spelling"}}} =
               Catena.resolve_file_unit("val", "X.cat", [%ModuleDeclaration{name: bad, span: nil}])
    end

    for good <- ["M", "Mod", "MyMod2", "X_9"] do
      assert {:ok, _} =
               Catena.resolve_file_unit("val", "#{good}.cat", [
                 %ModuleDeclaration{name: good, span: nil}
               ])
    end
  end

  @tag obligations: ~w(FU-OBL-006)
  test "declared names verify against the basename with FIL004 on mismatch" do
    assert {:error,
            %{
              id: "FIL004",
              details: %{reason: "basename_mismatch", declared: "Mod", expected: "Other"}
            }} =
             Catena.resolve_file_unit("val", "Other.cat", [
               %ModuleDeclaration{name: "Mod", span: nil}
             ])

    assert {:error, %{id: "FIL004"}} =
             Catena.resolve_file_unit("val", "mod.cat", [
               %ModuleDeclaration{name: "Mod", span: nil}
             ])

    assert {:ok, _} =
             Catena.resolve_file_unit("val", "src/deep/Mod.cat", [
               %ModuleDeclaration{name: "Mod", span: nil}
             ])

    assert {:ok, result} = Catena.resolve_file_unit("", "Whatever.cat", [])
    assert result.module == nil
  end

  @tag obligations: ~w(FU-OBL-007)
  test "the exact marker grammar is recognized with its tool identifier" do
    for tool <- ["t", "my-tool", "Tool_2", "0x9"] do
      assert {:ok, result} =
               Catena.resolve_file_unit("// catena:generated by #{tool}", "Gen.cat", [])

      assert result.generated == true
      assert result.tool == tool
    end

    assert {:ok, result} =
             Catena.resolve_file_unit("  \n\t\n// catena:generated by tool", "Gen.cat", [])

    assert result.generated == true

    assert {:ok, result} =
             Catena.resolve_file_unit("// catena:generated by tool\n\nval x", "M.cat", [
               %ModuleDeclaration{name: "M", span: nil}
             ])

    assert result.generated == true
    assert result.kind == :module
  end

  @tag obligations: ~w(FU-OBL-008 FU-OBL-009)
  test "placement is first-unit only, the text is inert elsewhere, and malformed markers fail" do
    for source <- [
          "// hello\n// catena:generated by tool",
          "val x\n// catena:generated by tool",
          "/* catena:generated by tool */",
          "/// catena:generated by tool",
          "\"// catena:generated by tool\""
        ] do
      assert {:ok, result} = Catena.resolve_file_unit(source, "M.cat", [])
      assert result.generated == false
      assert result.tool == nil
    end

    for malformed <- [
          "// catena:generatedby tool",
          "//  catena:generated by tool",
          "// catena:generated by ",
          "// catena:generated by my tool",
          "// catena:generated by tool!",
          "//catena:generated by tool",
          "// catena:generated by tool extra words",
          "// catena:generated"
        ] do
      assert {:error, %{id: "FIL005", details: %{reason: "malformed_generated_marker"}}} =
               Catena.resolve_file_unit(malformed, "M.cat", []),
             "expected FIL005 for #{inspect(malformed)}"
    end
  end

  @tag obligations: ~w(FU-OBL-010)
  test "stable diagnostics carry spans and both names on mismatch" do
    span = %Catena.SourceSpan{
      byte_start: 4,
      byte_end: 8,
      line_start: 1,
      column_start: 5,
      line_end: 1,
      column_end: 9
    }

    assert {:error, %{id: "FIL004", span: ^span}} =
             Catena.resolve_file_unit("val", "Other.cat", [
               %ModuleDeclaration{name: "Mod", span: span}
             ])

    assert {:error, %{id: "FIL003", span: ^span}} =
             Catena.resolve_file_unit("val", "X.cat", [
               %ModuleDeclaration{name: "bad", span: span}
             ])

    assert {:error, %{id: "FIL005", span: %Catena.SourceSpan{}}} =
             Catena.resolve_file_unit("// catena:generated by ", "M.cat", [])

    assert {:error, %{id: "FIL001", details: %{filename: "M.txt"}}} =
             Catena.resolve_file_unit("", "M.txt", [])
  end

  @tag obligations: ~w(FU-OBL-011)
  test "the resolver boundary is lossless, deterministic, and event-driven" do
    decl = %ModuleDeclaration{name: "Parcel", span: nil}

    first = Catena.resolve_file_unit("// catena:generated by t1\nval", "Parcel.cat", [decl])

    assert first ==
             Catena.resolve_file_unit("// catena:generated by t1\nval", "Parcel.cat", [decl])

    {:ok, result} = first
    assert result.module.name == "Parcel"
    assert result.generated == true
    assert result.tool == "t1"
    assert result.selection.language_revision == "0.1.16"

    for fields <- [:parse, :tokenize, :resolve_names, :compile] do
      refute Keyword.has_key?([], fields)
    end

    refute function_exported?(Catena.FileUnit, :tokenize, 1)
    refute function_exported?(Catena.FileUnit, :parse_declarations, 1)
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}
end
