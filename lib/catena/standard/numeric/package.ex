defmodule Catena.Standard.Numeric.Package do
  @moduledoc "Verified ordinary decimal roles; no new primitive numeric type."
  @external_resource "priv/stdlib/catena-numeric-0.1.67.json"
  @package JSON.decode!(File.read!(@external_resource))
  def package!, do: @package

  def verify(package) do
    if package == @package and
         package["digest"] == Catena.Categorical.Standard.digest(Map.delete(package, "digest")) and
         package["hierarchy_digest"] == Catena.Categorical.Standard.interface!()["digest"],
       do: :ok,
       else: {:error, :invalid_numeric_package}
  end

  def compile do
    with :ok <- verify(@package),
         do: Catena.compile_json(JSON.encode!(@package["ast"]), layout: :uniform)
  end
end
