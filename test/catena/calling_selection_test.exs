defmodule Catena.CallingSelectionTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(CV-OBL-001)
  alias Catena.Calling.{Artifact, Descriptor}

  test "calling selection and metadata are exact without broadening retained formats" do
    {:ok, core} =
      Catena.check_kernel("""
      (module CallingSelection (edition 0.1) (revision 0.1.8)
        (origin "test://calling-selection") (export value main)
        (def main (signature Int (uses)) 42))
      """)

    assert Catena.LanguageVersion.latest() == "0.1.83"
    assert Catena.LanguageVersion.calling_frontend_versions() == ["0.1.59"]
    refute "0.1.59" in Catena.LanguageVersion.interface_versions()
    refute "0.1.59" in Catena.LanguageVersion.signed_format_versions()
    assert {:ok, artifact} = Artifact.build(core)
    assert artifact.descriptor.version == "0.1.59"
    assert artifact.descriptor.selection.language_revision == "0.1.59"
    assert core.version == "0.1.8"
    {:ok, {_, [compile_info: info]}} = :beam_lib.chunks(artifact.binary, [:compile_info])
    assert info[:catena_language_revision] == ~c"0.1.59"
    assert info[:catena_frontend] == ~c"calling-0.1.59"

    for revision <- Catena.LanguageVersion.before(:calling_conventions) do
      assert {:error, :invalid_calling_selection} =
               Descriptor.build(core,
                 language_selection: Catena.LanguageVersion.legacy_selection(revision)
               )
    end

    assert {:error, :invalid_calling_selection} =
             Descriptor.build(core,
               language_selection: %{
                 edition: "0.1",
                 language_revision: "0.1.59",
                 previews: ["unapproved"]
               }
             )
  end
end
