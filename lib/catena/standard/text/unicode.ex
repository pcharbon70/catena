defmodule Catena.Standard.Text.Unicode do
  @moduledoc false
  @path Path.expand("../../../../priv/unicode/17.0.0/catena-text.etf", __DIR__)
  @external_resource @path
  @encoded File.read!(@path)
  @identity Base.encode16(:crypto.hash(:sha256, @encoded), case: :lower)
  @key {__MODULE__, @identity}
  def identity, do: @identity
  def manifest, do: tables().sources

  def tables do
    case :persistent_term.get(@key, nil) do
      nil ->
        tables = :erlang.binary_to_term(@encoded)
        :persistent_term.put(@key, tables)
        tables

      tables ->
        tables
    end
  end

  def property(kind, scalar, default) do
    ranges = Map.fetch!(tables(), kind)
    lookup(ranges, scalar, 0, tuple_size(ranges) - 1, default)
  end

  defp lookup(_, _, low, high, default) when low > high, do: default

  defp lookup(ranges, scalar, low, high, default) do
    mid = div(low + high, 2)
    {first, last, value} = elem(ranges, mid)

    cond do
      scalar < first -> lookup(ranges, scalar, low, mid - 1, default)
      scalar > last -> lookup(ranges, scalar, mid + 1, high, default)
      true -> value
    end
  end
end
