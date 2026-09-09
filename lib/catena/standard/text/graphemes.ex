defmodule Catena.Standard.Text.Graphemes do
  @moduledoc false
  alias Catena.Standard.Text.Unicode

  # UTF-8 is checked by the caller. Positions are original byte offsets.
  def boundaries(""), do: [0]
  def boundaries(text), do: scan(text, 0, nil, 0, :none, :none, [0])
  defp scan(<<>>, offset, _, _, _, _, acc), do: Enum.reverse([offset | acc])

  defp scan(<<scalar::utf8, rest::binary>>, offset, previous, regional, emoji, indic, acc) do
    property = Unicode.property(:grapheme, scalar, "Other")
    incb = Unicode.property(:indic, scalar, "None")
    pictographic = Unicode.property(:pictographic, scalar, false)

    boundary =
      previous != nil and break?(previous, property, regional, emoji, indic, incb, pictographic)

    acc = if boundary, do: [offset | acc], else: acc
    next_regional = if property == "Regional_Indicator", do: rem(regional + 1, 2), else: 0

    next_emoji =
      cond do
        pictographic -> :pictographic
        property == "Extend" and emoji == :pictographic -> :pictographic
        property == "ZWJ" and emoji == :pictographic -> :joined
        true -> :none
      end

    next_indic =
      cond do
        incb == "Consonant" -> :consonant
        incb == "Linker" and indic in [:consonant, :linked] -> :linked
        incb == "Extend" -> indic
        true -> :none
      end

    scan(
      rest,
      offset + byte_size(<<scalar::utf8>>),
      property,
      next_regional,
      next_emoji,
      next_indic,
      acc
    )
  end

  defp break?(previous, current, regional, emoji, indic, incb, pictographic) do
    cond do
      previous == "CR" and current == "LF" ->
        false

      previous in ["Control", "CR", "LF"] ->
        true

      current in ["Control", "CR", "LF"] ->
        true

      previous == "L" and current in ["L", "V", "LV", "LVT"] ->
        false

      previous in ["LV", "V"] and current in ["V", "T"] ->
        false

      previous in ["LVT", "T"] and current == "T" ->
        false

      current in ["Extend", "ZWJ", "SpacingMark"] ->
        false

      previous == "Prepend" ->
        false

      indic == :linked and incb == "Consonant" ->
        false

      emoji == :joined and pictographic ->
        false

      previous == "Regional_Indicator" and current == "Regional_Indicator" and
          rem(regional, 2) == 1 ->
        false

      true ->
        true
    end
  end
end
