defmodule Catena.Trust.Profile do
  @moduledoc "Guarantee-specific trust disclosure and reviewed development boundary baseline."
  @external_resource "priv/trust/inventory.json"
  @profile JSON.decode!(File.read!("priv/trust/inventory.json"))
  def document, do: @profile

  def summary do
    %{
      "contract" => "0.1.70",
      "source_count" => map_size(@profile["sources"]),
      "data_count" => map_size(@profile["data"]),
      "digest" => @profile["digest"],
      "guarantees" => Enum.sort(Map.keys(@profile["guarantees"])),
      "assumptions" => @profile["external_assumptions"],
      "proof_verified_compiler" => false,
      "runtime_sandbox" => false
    }
  end

  def verify(profile) do
    components = Map.keys(profile["components"])
    assigned = Enum.flat_map(profile["components"], fn {_, component} -> component["sources"] end)

    if profile == @profile and
         profile["digest"] == Catena.Categorical.Standard.digest(Map.delete(profile, "digest")) and
         Enum.sort(assigned) == Enum.sort(Map.keys(profile["sources"])) and
         length(assigned) == length(Enum.uniq(assigned)) and
         Enum.all?(profile["guarantees"], fn {_, guarantee} ->
           guarantee["depends_on"] != [] and guarantee["checks"] != [] and
             guarantee["residual_trust"] != [] and
             Enum.all?(guarantee["depends_on"], &(&1 in components))
         end), do: :ok, else: {:error, :invalid_trust_profile}
  rescue
    _ -> {:error, :invalid_trust_profile}
  end

  def audit(root) do
    with :ok <- verify(@profile),
         {:ok, actual} <- Catena.Trust.Inventory.scan(root),
         :ok <- Catena.Trust.Inventory.compare(@profile["sources"], actual),
         :ok <- Catena.Trust.Inventory.compare(@profile["data"], data(root)),
         do: :ok
  end

  def data(root) do
    (Path.wildcard(Path.join(root, "priv/**/*")) ++
       Enum.filter(Enum.map(["mix.exs", "mix.lock"], &Path.join(root, &1)), &File.regular?/1))
    |> Enum.filter(
      &(File.regular?(&1) and not String.ends_with?(&1, ".py") and
          not String.starts_with?(Path.relative_to(&1, root), "priv/trust/"))
    )
    |> Map.new(fn path ->
      {Path.relative_to(path, root), Catena.Trust.Inventory.digest(File.read!(path))}
    end)
  end
end
