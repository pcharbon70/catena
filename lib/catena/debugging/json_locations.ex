defmodule Catena.Debugging.JSONLocations do
  @moduledoc false
  def index(source) do
    with {:ok, _} <- JSON.decode(source) do
      {_, entries} = value(source, ws(source, 0), "$", %{})
      {:ok, Catena.SourceSpan.from_ranges(source, entries)}
    end
  rescue
    _ -> {:error, :invalid_json_locations}
  end

  defp value(s, start, path, entries) do
    {finish, entries} =
      case :binary.at(s, start) do
        ?{ -> object(s, ws(s, start + 1), path, entries)
        ?[ -> array(s, ws(s, start + 1), path, 0, entries)
        ?" -> {string_end(s, start + 1), entries}
        _ -> {scalar_end(s, start), entries}
      end

    {finish, Map.put(entries, path, {start, finish})}
  end

  defp object(s, n, path, entries) do
    if :binary.at(s, n) == ?} do
      {n + 1, entries}
    else
      ending = string_end(s, n + 1)
      {:ok, key} = JSON.decode(binary_part(s, n, ending - n))
      colon = ws(s, ending)
      {finish, entries} = value(s, ws(s, colon + 1), path <> segment(key), entries)
      next = ws(s, finish)

      if :binary.at(s, next) == ?,,
        do: object(s, ws(s, next + 1), path, entries),
        else: {next + 1, entries}
    end
  end

  defp array(s, n, path, index, entries) do
    if :binary.at(s, n) == ?] do
      {n + 1, entries}
    else
      {finish, entries} = value(s, n, path <> "[#{index}]", entries)
      next = ws(s, finish)

      if :binary.at(s, next) == ?,,
        do: array(s, ws(s, next + 1), path, index + 1, entries),
        else: {next + 1, entries}
    end
  end

  defp string_end(s, n) do
    case :binary.at(s, n) do
      ?\\ -> string_end(s, n + 2)
      ?" -> n + 1
      _ -> string_end(s, n + 1)
    end
  end

  defp scalar_end(s, n) when n == byte_size(s), do: n

  defp scalar_end(s, n) do
    if :binary.at(s, n) in [32, 9, 10, 13, ?,, ?}, ?]], do: n, else: scalar_end(s, n + 1)
  end

  defp ws(s, n) when n == byte_size(s), do: n

  defp ws(s, n) do
    if :binary.at(s, n) in [32, 9, 10, 13], do: ws(s, n + 1), else: n
  end

  defp segment(key) do
    if Regex.match?(~r/^[A-Za-z_][A-Za-z0-9_]*$/, key),
      do: "." <> key,
      else: "[" <> JSON.encode!(key) <> "]"
  end
end
