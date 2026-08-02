defmodule Catena.Categorical.TypeTerm do
  @moduledoc "Codec for the kinded type terms used by Catena 0.4 trait evidence."

  alias Catena.{Diagnostic, Kind}

  @spec decode!(map(), keyword()) :: Catena.Type.Trait.type_term()
  def decode!(value, options \\ [])

  def decode!(%{"tag" => "variable", "name" => name, "kind" => kind}, options)
      when is_binary(name) and is_binary(kind) do
    {:variable, name, Kind.parse!(kind, Keyword.get(options, :path))}
  end

  def decode!(%{"tag" => "constructor", "id" => id, "kind" => kind} = value, options)
      when is_binary(id) and is_binary(kind) do
    {:constructor, id, Kind.parse!(kind, Keyword.get(options, :path)), Map.get(value, "owner")}
  end

  def decode!(%{"tag" => "application", "callee" => callee, "argument" => argument}, options) do
    {:application, decode!(callee, options), decode!(argument, options)}
  end

  def decode!(value, options) do
    raise Catena.TypeError,
      diagnostic:
        Diagnostic.new("TRT002", "malformed trait type term #{inspect(value)}",
          path: Keyword.get(options, :path)
        )
  end

  @spec encode(Catena.Type.Trait.type_term()) :: map()
  def encode({:variable, name, kind}),
    do: %{"tag" => "variable", "name" => name, "kind" => Kind.encode(kind)}

  def encode({:constructor, id, kind, owner}) do
    %{"tag" => "constructor", "id" => id, "kind" => Kind.encode(kind)}
    |> maybe_put("owner", owner)
  end

  def encode({:application, callee, argument}),
    do: %{"tag" => "application", "callee" => encode(callee), "argument" => encode(argument)}

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)
end
