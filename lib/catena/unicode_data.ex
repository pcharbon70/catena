defmodule Catena.UnicodeData do
  @moduledoc false

  @unicode_version "17.0.0"
  @table_file "catena-unicode.etf"
  @table_path Path.expand("../../priv/unicode/#{@unicode_version}/#{@table_file}", __DIR__)
  @external_resource @table_path
  @encoded_tables File.read!(@table_path)
  @persistent_key {__MODULE__, @unicode_version}

  @hangul_start 0xAC00
  @hangul_end 0xD7A3
  @l_start 0x1100
  @v_start 0x1161
  @t_start 0x11A7
  @l_count 19
  @v_count 21
  @t_count 28
  @l_end @l_start + @l_count - 1
  @v_end @v_start + @v_count - 1
  @t_first @t_start + 1
  @t_end @t_start + @t_count - 1
  @n_count @v_count * @t_count

  @type scalar :: non_neg_integer()

  @spec version() :: String.t()
  def version, do: @unicode_version

  @spec source_manifest() :: map()
  def source_manifest, do: tables().sources

  @spec xid_start?(scalar()) :: boolean()
  def xid_start?(scalar), do: in_ranges?(tables().xid_start, scalar)

  @spec xid_continue?(scalar()) :: boolean()
  def xid_continue?(scalar), do: in_ranges?(tables().xid_continue, scalar)

  @spec identifier_allowed?(scalar()) :: boolean()
  def identifier_allowed?(scalar), do: in_ranges?(tables().identifier_allowed, scalar)

  @spec nfc(String.t()) :: String.t()
  def nfc(string) when is_binary(string) do
    string
    |> String.to_charlist()
    |> canonical_decomposition()
    |> canonical_composition()
    |> List.to_string()
  end

  @spec nfc?(String.t()) :: boolean()
  def nfc?(string) when is_binary(string), do: nfc(string) == string

  @spec skeleton(String.t()) :: String.t()
  def skeleton(string) when is_binary(string) do
    mappings = tables().confusables

    string
    |> String.to_charlist()
    |> canonical_decomposition()
    |> Enum.reject(&default_ignorable?/1)
    |> Enum.flat_map(&Map.get(mappings, &1, [&1]))
    |> canonical_decomposition()
    |> List.to_string()
  end

  @spec scripts(String.t()) :: [String.t()]
  def scripts(string) when is_binary(string) do
    string
    |> String.to_charlist()
    |> Enum.flat_map(&script_set/1)
    |> Enum.reject(&(&1 in ~w(Zyyy Zinh)))
    |> Enum.uniq()
    |> Enum.sort()
  end

  @spec highly_restrictive?(String.t()) :: boolean()
  def highly_restrictive?(string) when is_binary(string) do
    scalars = String.to_charlist(string)

    if Enum.all?(scalars, &(&1 <= 0x7F)) do
      true
    else
      sets = Enum.map(scalars, &augmented_script_set/1)
      single_script?(sets) or Enum.any?(highly_restrictive_covers(), &covers?(sets, &1))
    end
  end

  defp canonical_decomposition(scalars) do
    scalars
    |> Enum.flat_map(&decompose_scalar/1)
    |> canonical_order()
  end

  defp decompose_scalar(scalar) when scalar in @hangul_start..@hangul_end do
    index = scalar - @hangul_start
    l = @l_start + div(index, @n_count)
    v = @v_start + div(rem(index, @n_count), @t_count)
    t = rem(index, @t_count)
    if t == 0, do: [l, v], else: [l, v, @t_start + t]
  end

  defp decompose_scalar(scalar) do
    case Map.get(tables().decompositions, scalar) do
      nil -> [scalar]
      decomposition -> Enum.flat_map(decomposition, &decompose_scalar/1)
    end
  end

  defp canonical_order(scalars) do
    {ordered, segment} =
      Enum.reduce(scalars, {[], []}, fn scalar, {ordered, segment} ->
        if combining_class(scalar) == 0 and segment != [] do
          {ordered ++ order_segment(segment), [scalar]}
        else
          {ordered, segment ++ [scalar]}
        end
      end)

    ordered ++ order_segment(segment)
  end

  defp order_segment([]), do: []

  defp order_segment([starter | marks]) do
    if combining_class(starter) == 0 do
      [starter | stable_combining_sort(marks)]
    else
      stable_combining_sort([starter | marks])
    end
  end

  defp stable_combining_sort(scalars) do
    scalars
    |> Enum.with_index()
    |> Enum.sort_by(fn {scalar, index} -> {combining_class(scalar), index} end)
    |> Enum.map(&elem(&1, 0))
  end

  defp canonical_composition([]), do: []

  defp canonical_composition([first | rest]) do
    {output, _starter, _starter_index, _last_class} =
      Enum.reduce(rest, {[first], first, 0, combining_class(first)}, fn scalar,
                                                                        {output, starter,
                                                                         starter_index,
                                                                         last_class} ->
        class = combining_class(scalar)
        composite = compose(starter, scalar)

        if composite != nil and (last_class < class or last_class == 0) do
          {List.replace_at(output, starter_index, composite), composite, starter_index,
           last_class}
        else
          next_output = output ++ [scalar]

          if class == 0 do
            {next_output, scalar, length(output), 0}
          else
            {next_output, starter, starter_index, class}
          end
        end
      end)

    output
  end

  defp compose(l, v) when l in @l_start..@l_end and v in @v_start..@v_end do
    @hangul_start + (l - @l_start) * @n_count + (v - @v_start) * @t_count
  end

  defp compose(lv, t)
       when lv in @hangul_start..@hangul_end and rem(lv - @hangul_start, @t_count) == 0 and
              t in @t_first..@t_end do
    lv + t - @t_start
  end

  defp compose(first, second), do: Map.get(tables().compositions, {first, second})

  defp combining_class(scalar), do: Map.get(tables().combining_classes, scalar, 0)

  defp default_ignorable?(scalar), do: in_ranges?(tables().default_ignorable, scalar)

  defp single_script?(sets) do
    sets
    |> Enum.reject(&(&1 == :all))
    |> case do
      [] ->
        true

      [first | rest] ->
        rest
        |> Enum.reduce(MapSet.new(first), &MapSet.intersection(&2, MapSet.new(&1)))
        |> MapSet.size() > 0
    end
  end

  defp covers?(sets, cover) do
    Enum.all?(sets, fn
      :all -> true
      scripts -> Enum.any?(scripts, &MapSet.member?(cover, &1))
    end)
  end

  defp highly_restrictive_covers do
    [
      MapSet.new(~w(Latn Jpan)),
      MapSet.new(~w(Latn Hanb)),
      MapSet.new(~w(Latn Kore))
    ]
  end

  defp augmented_script_set(scalar) do
    scripts = script_set(scalar)

    if Enum.any?(scripts, &(&1 in ~w(Zyyy Zinh))) do
      :all
    else
      scripts
      |> Enum.reduce(MapSet.new(scripts), fn
        "Hani", set -> set |> MapSet.put("Hanb") |> MapSet.put("Jpan") |> MapSet.put("Kore")
        "Hira", set -> MapSet.put(set, "Jpan")
        "Kana", set -> MapSet.put(set, "Jpan")
        "Hang", set -> MapSet.put(set, "Kore")
        "Bopo", set -> MapSet.put(set, "Hanb")
        _script, set -> set
      end)
      |> MapSet.to_list()
    end
  end

  defp script_set(scalar) do
    case range_value(tables().script_extensions, scalar) do
      nil -> [range_value(tables().scripts, scalar) || "Zzzz"]
      scripts -> scripts
    end
  end

  defp in_ranges?(ranges, scalar), do: range_value(ranges, scalar) != nil

  defp range_value(ranges, scalar) when is_tuple(ranges) do
    range_value(ranges, scalar, 0, tuple_size(ranges) - 1)
  end

  defp range_value(_ranges, _scalar, low, high) when low > high, do: nil

  defp range_value(ranges, scalar, low, high) do
    middle = div(low + high, 2)

    case elem(ranges, middle) do
      {first, _last} when scalar < first -> range_value(ranges, scalar, low, middle - 1)
      {_first, last} when scalar > last -> range_value(ranges, scalar, middle + 1, high)
      {_first, _last} -> true
      {first, _last, _value} when scalar < first -> range_value(ranges, scalar, low, middle - 1)
      {_first, last, _value} when scalar > last -> range_value(ranges, scalar, middle + 1, high)
      {_first, _last, value} -> value
    end
  end

  defp tables do
    :persistent_term.get(@persistent_key)
  rescue
    ArgumentError ->
      loaded = load_tables()
      :persistent_term.put(@persistent_key, loaded)
      loaded
  end

  defp load_tables do
    # The term is generated from pinned repository data and embedded in this
    # module at compile time; it is never read from runtime input.
    tables = :erlang.binary_to_term(@encoded_tables)

    if tables.unicode_version != @unicode_version do
      raise "Catena Unicode table version mismatch"
    end

    tables
    |> Map.update!(:xid_start, &List.to_tuple/1)
    |> Map.update!(:xid_continue, &List.to_tuple/1)
    |> Map.update!(:default_ignorable, &List.to_tuple/1)
    |> Map.update!(:identifier_allowed, &List.to_tuple/1)
    |> Map.update!(:scripts, &List.to_tuple/1)
    |> Map.update!(:script_extensions, &List.to_tuple/1)
  end
end
