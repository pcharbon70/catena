defmodule Catena.Standard.Text.Indices do
  @moduledoc "Ordinary nominal index roles and explicit conversion to operation descriptors."
  @external_resource "priv/stdlib/catena-text-0.1.66.json"
  @package JSON.decode!(File.read!(@external_resource))
  @roles [
    {:byte, :"catena://text-contract/0.1.66::CatenaTextRoles::ByteIndex"},
    {:scalar, :"catena://text-contract/0.1.66::CatenaTextRoles::ScalarIndex"},
    {:grapheme, :"catena://text-contract/0.1.66::CatenaTextRoles::GraphemeIndex"}
  ]
  def package!, do: @package

  def verify_package(package) do
    if package == @package and
         package["digest"] == Catena.Categorical.Standard.digest(Map.delete(package, "digest")) and
         package["hierarchy_digest"] == Catena.Categorical.Standard.interface!()["digest"],
       do: :ok,
       else: {:error, :invalid_text_package}
  end

  def compile do
    with :ok <- verify_package(@package),
         do: Catena.compile_json(JSON.encode!(@package["ast"]), layout: :uniform)
  end

  for {unit, identity} <- @roles do
    def from_nominal({:catena_adt, unquote(identity), 0, {offset}}) when is_integer(offset),
      do: Catena.Standard.Text.index(unquote(unit), offset)
  end

  def from_nominal(_), do: {:error, :invalid_nominal_text_index}
end
