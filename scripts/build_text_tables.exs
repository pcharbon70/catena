#!/usr/bin/env elixir
# Rebuild only from pinned local inputs. Refreshing upstream is a separate action.
defmodule Catena.TextTableBuilder do
  @root Path.expand("../priv/unicode/17.0.0", __DIR__)
  @sources [
    {"UnicodeData.txt", "https://www.unicode.org/Public/17.0.0/ucd/UnicodeData.txt"},
    {"DerivedNormalizationProps.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/DerivedNormalizationProps.txt"},
    {"NormalizationTest.txt", "https://www.unicode.org/Public/17.0.0/ucd/NormalizationTest.txt"},
    {"DerivedCoreProperties.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/DerivedCoreProperties.txt"},
    {"GraphemeBreakProperty.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/auxiliary/GraphemeBreakProperty.txt"},
    {"GraphemeBreakTest.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/auxiliary/GraphemeBreakTest.txt"},
    {"emoji-data.txt", "https://www.unicode.org/Public/17.0.0/ucd/emoji/emoji-data.txt"}
  ]
  def run do
    {classes, canonical, compatibility} =
      Enum.reduce(lines("UnicodeData.txt"), {%{}, %{}, %{}}, fn row, {ccc, canon, compat} ->
        scalar = hex(Enum.at(row, 0))
        class = String.to_integer(Enum.at(row, 3))
        parts = String.split(Enum.at(row, 5))
        ccc = if class == 0, do: ccc, else: Map.put(ccc, scalar, class)

        case parts do
          [] ->
            {ccc, canon, compat}

          ["<" <> _ | rest] ->
            {ccc, canon, Map.put(compat, scalar, Enum.map(rest, &hex/1))}

          values ->
            {ccc, Map.put(canon, scalar, Enum.map(values, &hex/1)),
             Map.put(compat, scalar, Enum.map(values, &hex/1))}
        end
      end)

    exclusions =
      lines("DerivedNormalizationProps.txt")
      |> Enum.flat_map(fn
        [range, "Full_Composition_Exclusion" | _] -> expand(range)
        _ -> []
      end)
      |> MapSet.new()

    compositions =
      Enum.reduce(canonical, %{}, fn
        {scalar, [a, b]}, acc ->
          if MapSet.member?(exclusions, scalar) or Map.get(classes, a, 0) != 0,
            do: acc,
            else: Map.put(acc, {a, b}, scalar)

        _, acc ->
          acc
      end)

    tables = %{
      version: "17.0.0",
      uax29_revision: 47,
      uax15_revision: 57,
      classes: classes,
      canonical: canonical,
      compatibility: compatibility,
      compositions: compositions,
      grapheme: ranges("GraphemeBreakProperty.txt", fn [_, value | _] -> value end),
      indic:
        ranges("DerivedCoreProperties.txt", fn
          [_, "InCB", value | _] -> value
          _ -> nil
        end),
      pictographic:
        ranges("emoji-data.txt", fn
          [_, "Extended_Pictographic" | _] -> true
          _ -> nil
        end),
      sources:
        Map.new(@sources, fn {name, url} ->
          {name,
           %{
             url: url,
             sha256:
               Base.encode16(:crypto.hash(:sha256, File.read!(Path.join(@root, name))),
                 case: :lower
               )
           }}
        end)
    }

    path = Path.join(@root, "catena-text.etf")
    File.write!(path, :erlang.term_to_binary(tables, [:deterministic, compressed: 9]))
    IO.puts("wrote #{path} (#{File.stat!(path).size} bytes)")
  end

  defp ranges(file, property) do
    lines(file)
    |> Enum.flat_map(fn [range | _] = row ->
      case property.(row) do
        nil ->
          []

        value ->
          {a, b} = range(range)
          [{a, b, value}]
      end
    end)
    |> Enum.sort()
    |> List.to_tuple()
  end

  defp lines(name),
    do:
      Path.join(@root, name)
      |> File.stream!()
      |> Stream.map(&(&1 |> String.split("#", parts: 2) |> hd() |> String.trim()))
      |> Stream.reject(&(&1 == ""))
      |> Enum.map(&(String.split(&1, ";") |> Enum.map(fn x -> String.trim(x) end)))

  defp range(value) do
    case String.split(value, "..") do
      [a] -> {hex(a), hex(a)}
      [a, b] -> {hex(a), hex(b)}
    end
  end

  defp expand(value), do: then(range(value), fn {a, b} -> Enum.to_list(a..b) end)
  defp hex(value), do: String.to_integer(value, 16)
end

Catena.TextTableBuilder.run()
