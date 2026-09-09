defmodule Catena.Runtime.Environment.Manifest do
  @moduledoc "Separate exact entry requirements; no widening of retained package manifests."
  alias Catena.Runtime.Environment.Schema

  def decode(binary) when is_binary(binary) and byte_size(binary) <= 16384 do
    with {:ok, value} <- JSON.decode(binary),
         true <- is_map(value),
         true <- Enum.sort(Map.keys(value)) == ~w(entry format services version),
         "catena-environment-entry" <- value["format"],
         "0.1.68" <- value["version"],
         entry when is_binary(entry) <- value["entry"],
         true <- Regex.match?(~r/\A[a-z][A-Za-z0-9_]*\z/, entry),
         services when is_list(services) <- value["services"],
         true <- services == Enum.sort(Enum.uniq(services)),
         true <-
           Enum.all?(
             services,
             &(&1 in Enum.map(Schema.services(), fn service -> Atom.to_string(service) end))
           ) do
      {:ok, value}
    else
      _ -> {:error, :invalid_environment_manifest}
    end
  end

  def decode(_), do: {:error, :invalid_environment_manifest}

  def validate(manifest, description) do
    with {:ok, ^manifest} <- decode(JSON.encode!(manifest)),
         true <- manifest["entry"] == description.name,
         true <-
           manifest["services"] == Enum.sort(Enum.map(description.services, &Atom.to_string/1)) do
      :ok
    else
      _ -> {:error, :environment_manifest_mismatch}
    end
  rescue
    _ -> {:error, :invalid_environment_manifest}
  end
end
