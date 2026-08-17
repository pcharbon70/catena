#!/usr/bin/env elixir

Mix.install([])

defmodule Catena.UnicodeTableBuilder do
  @version "17.0.0"
  @root Path.expand("../priv/unicode/#{@version}", __DIR__)
  @output Path.join(@root, "catena-unicode.etf")
  @sources [
    {"DerivedCoreProperties.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/DerivedCoreProperties.txt"},
    {"DerivedNormalizationProps.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/DerivedNormalizationProps.txt"},
    {"NormalizationTest.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/NormalizationTest.txt"},
    {"PropertyValueAliases.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/PropertyValueAliases.txt"},
    {"ScriptExtensions.txt",
     "https://www.unicode.org/Public/17.0.0/ucd/ScriptExtensions.txt"},
    {"Scripts.txt", "https://www.unicode.org/Public/17.0.0/ucd/Scripts.txt"},
    {"UnicodeData.txt", "https://www.unicode.org/Public/17.0.0/ucd/UnicodeData.txt"},
    {"IdentifierStatus.txt",
     "https://www.unicode.org/Public/17.0.0/security/IdentifierStatus.txt"},
    {"confusables.txt", "https://www.unicode.org/Public/17.0.0/security/confusables.txt"}
  ]

  def run(arguments) do
    File.mkdir_p!(@root)

    if "--download" in arguments do
      Enum.each(@sources, &download!/1)
    end

    Enum.each(@sources, fn {name, _url} ->
      unless File.regular?(Path.join(@root, name)), do: raise("missing Unicode source #{name}")
    end)

    aliases = script_aliases(path("PropertyValueAliases.txt"))
    unicode = unicode_data(path("UnicodeData.txt"))
    excluded = property_ranges(path("DerivedNormalizationProps.txt"), "Full_Composition_Exclusion")

    tables = %{
      unicode_version: @version,
      sources: source_manifest(),
      xid_start: property_ranges(path("DerivedCoreProperties.txt"), "XID_Start"),
      xid_continue: property_ranges(path("DerivedCoreProperties.txt"), "XID_Continue"),
      default_ignorable:
        property_ranges(path("DerivedCoreProperties.txt"), "Default_Ignorable_Code_Point"),
      identifier_allowed: value_ranges(path("IdentifierStatus.txt"), "Allowed"),
      scripts: scripts(path("Scripts.txt"), aliases),
      script_extensions: script_extensions(path("ScriptExtensions.txt")),
      combining_classes: unicode.combining_classes,
      decompositions: unicode.decompositions,
      compositions: compositions(unicode, excluded),
      confusables: confusables(path("confusables.txt"))
    }

    File.write!(@output, :erlang.term_to_binary(tables, compressed: 9))
    IO.puts("wrote #{@output} (#{File.stat!(@output).size} bytes)")
  end

  defp download!({name, url}) do
    destination = path(name)
    {_, status} = System.cmd("curl", ["-fsSL", url, "-o", destination], stderr_to_stdout: true)
    if status != 0, do: raise("failed to download #{url}")
    IO.puts("downloaded #{name}")
  end

  defp source_manifest do
    Map.new(@sources, fn {name, url} ->
      bytes = File.read!(path(name))
      {name, %{url: url, sha256: Base.encode16(:crypto.hash(:sha256, bytes), case: :lower)}}
    end)
  end

  defp script_aliases(file) do
    file
    |> data_lines()
    |> Enum.reduce(%{}, fn line, aliases ->
      case fields(line) do
        ["sc", short, long | _] -> Map.put(aliases, long, short)
        _ -> aliases
      end
    end)
  end

  defp unicode_data(file) do
    file
    |> File.stream!()
    |> Enum.reduce(%{combining_classes: %{}, decompositions: %{}}, fn line, result ->
      values = line |> String.trim() |> String.split(";")
      scalar = values |> Enum.at(0) |> hex()
      combining_class = values |> Enum.at(3) |> String.to_integer()
      decomposition = Enum.at(values, 5)

      result =
        if combining_class == 0 do
          result
        else
          put_in(result, [:combining_classes, scalar], combining_class)
        end

      case canonical_decomposition(decomposition) do
        [] -> result
        scalars -> put_in(result, [:decompositions, scalar], scalars)
      end
    end)
  end

  defp canonical_decomposition(""), do: []
  defp canonical_decomposition("<" <> _compatibility), do: []
  defp canonical_decomposition(value), do: value |> String.split() |> Enum.map(&hex/1)

  defp compositions(unicode, excluded_ranges) do
    excluded = expand_ranges(excluded_ranges)

    Enum.reduce(unicode.decompositions, %{}, fn
      {composite, [first, second]}, compositions ->
        if MapSet.member?(excluded, composite) or Map.get(unicode.combining_classes, first, 0) != 0 do
          compositions
        else
          Map.put(compositions, {first, second}, composite)
        end

      _, compositions ->
        compositions
    end)
  end

  defp scripts(file, aliases) do
    file
    |> data_lines()
    |> Enum.map(fn line ->
      [range, script | _] = fields(line)
      {first, last} = range(range)
      {first, last, Map.fetch!(aliases, script)}
    end)
    |> Enum.sort_by(&elem(&1, 0))
  end

  defp script_extensions(file) do
    file
    |> data_lines()
    |> Enum.map(fn line ->
      [range, scripts | _] = fields(line)
      {first, last} = range(range)
      {first, last, String.split(scripts)}
    end)
  end

  defp confusables(file) do
    file
    |> data_lines()
    |> Enum.reduce(%{}, fn line, mappings ->
      [source, target | _] = fields(line)

      case source |> String.split() |> Enum.map(&hex/1) do
        [scalar] -> Map.put(mappings, scalar, target |> String.split() |> Enum.map(&hex/1))
        _sequence -> mappings
      end
    end)
  end

  defp property_ranges(file, property) do
    file
    |> data_lines()
    |> Enum.flat_map(fn line ->
      case fields(line) do
        [range_value, ^property | _] -> [range(range_value)]
        _ -> []
      end
    end)
  end

  defp value_ranges(file, value) do
    file
    |> data_lines()
    |> Enum.flat_map(fn line ->
      case fields(line) do
        [range_value, ^value | _] -> [range(range_value)]
        _ -> []
      end
    end)
  end

  defp data_lines(file) do
    file
    |> File.stream!()
    |> Stream.map(&(&1 |> String.split("#", parts: 2) |> hd() |> String.trim()))
    |> Stream.reject(&(&1 == ""))
  end

  defp fields(line), do: line |> String.split(";") |> Enum.map(&String.trim/1)

  defp range(value) do
    case String.split(value, "..") do
      [scalar] -> value = hex(scalar); {value, value}
      [first, last] -> {hex(first), hex(last)}
    end
  end

  defp expand_ranges(ranges) do
    Enum.reduce(ranges, MapSet.new(), fn {first, last}, values ->
      Enum.reduce(first..last, values, &MapSet.put(&2, &1))
    end)
  end

  defp hex(value), do: String.to_integer(value, 16)
  defp path(name), do: Path.join(@root, name)
end

Catena.UnicodeTableBuilder.run(System.argv())
