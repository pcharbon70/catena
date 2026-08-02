defmodule Catena.Categorical.Standard do
  @moduledoc "Loads and verifies the ordinary-library Catena 0.4 categorical hierarchy."

  alias Catena.{CanonicalJSON, Diagnostic}

  @path Application.app_dir(:catena, "priv/stdlib/catena-standard-0.4.json")

  @spec interface!() :: map()
  def interface! do
    value = @path |> File.read!() |> JSON.decode!()
    digest = Map.get(value, "digest")
    payload = Map.delete(value, "digest")
    actual = digest(payload)

    unless Map.get(value, "format") == "catena-standard-interface" and
             Map.get(value, "version") == "0.4" and digest == actual do
      raise Catena.TypeError,
        diagnostic:
          Diagnostic.new(
            "TRT008",
            "Catena standard hierarchy digest mismatch; reinstall the compiler toolchain"
          )
    end

    value
  end

  @spec digest(map()) :: String.t()
  def digest(payload),
    do: :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)
end
