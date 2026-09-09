defmodule Catena.Standard.Text.Normalization do
  @moduledoc false
  alias Catena.Standard.Text.Unicode

  def normalize(text, form) when form in [:nfc, :nfd, :nfkc, :nfkd] do
    tables = Unicode.tables()
    mappings = if form in [:nfc, :nfd], do: tables.canonical, else: tables.compatibility

    scalars =
      text
      |> String.to_charlist()
      |> Enum.flat_map(&decompose(&1, mappings))
      |> order(tables.classes)

    scalars = if form in [:nfc, :nfkc], do: compose_all(scalars, tables), else: scalars
    List.to_string(scalars)
  end

  defp decompose(scalar, _) when scalar in 0xAC00..0xD7A3 do
    index = scalar - 0xAC00
    first = 0x1100 + div(index, 588)
    second = 0x1161 + div(rem(index, 588), 28)

    case rem(index, 28) do
      0 -> [first, second]
      last -> [first, second, 0x11A7 + last]
    end
  end

  defp decompose(scalar, mappings) do
    case Map.get(mappings, scalar) do
      nil -> [scalar]
      values -> Enum.flat_map(values, &decompose(&1, mappings))
    end
  end

  defp order(values, classes) do
    {segments, current} =
      Enum.reduce(values, {[], []}, fn scalar, {segments, current} ->
        if Map.get(classes, scalar, 0) == 0 and current != [],
          do: {[sort(current, classes) | segments], [scalar]},
          else: {segments, [scalar | current]}
      end)

    [sort(current, classes) | segments] |> Enum.reverse() |> List.flatten()
  end

  defp sort(reversed, classes) do
    reversed |> Enum.reverse() |> Enum.sort_by(&Map.get(classes, &1, 0))
  end

  defp compose_all(values, tables) do
    {done, starter, marks, _last} =
      Enum.reduce(values, {[], nil, [], 0}, fn scalar, {done, starter, marks, last} ->
        class = Map.get(tables.classes, scalar, 0)
        composite = if starter, do: compose(starter, scalar, tables.compositions), else: nil

        cond do
          composite != nil and (last < class or last == 0) -> {done, composite, marks, last}
          class == 0 -> {flush(done, starter, marks), scalar, [], 0}
          starter == nil -> {[scalar | done], nil, [], class}
          true -> {done, starter, [scalar | marks], class}
        end
      end)

    flush(done, starter, marks) |> Enum.reverse()
  end

  defp flush(done, nil, []), do: done
  defp flush(done, starter, marks), do: marks ++ [starter | done]

  defp compose(l, v, _) when l in 0x1100..0x1112 and v in 0x1161..0x1175,
    do: 0xAC00 + (l - 0x1100) * 588 + (v - 0x1161) * 28

  defp compose(lv, t, _)
       when lv in 0xAC00..0xD7A3 and rem(lv - 0xAC00, 28) == 0 and t in 0x11A8..0x11C2,
       do: lv + t - 0x11A7

  defp compose(a, b, compositions), do: Map.get(compositions, {a, b})
end
