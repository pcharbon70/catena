defmodule Catena.TextUnicodeContractTest do
  use ExUnit.Case, async: true
  @moduletag obligations: ~w(TB-OBL-001 TB-OBL-004 TB-OBL-005 TB-OBL-012)
  alias Catena.Standard.Text.{Graphemes, Normalization, Unicode}
  @root Path.expand("../../priv/unicode/17.0.0", __DIR__)

  test "every pinned extended grapheme conformance vector matches original byte offsets" do
    count =
      @root
      |> Path.join("GraphemeBreakTest.txt")
      |> File.stream!()
      |> Enum.reduce(0, fn line, count ->
        data = line |> String.split("#", parts: 2) |> hd() |> String.trim()

        if data == "" do
          count
        else
          {pieces, boundaries, _} =
            data
            |> String.split()
            |> Enum.reduce({[], [], 0}, fn
              "÷", {parts, breaks, offset} ->
                {parts, [offset | breaks], offset}

              "×", acc ->
                acc

              hex, {parts, breaks, offset} ->
                binary = <<String.to_integer(hex, 16)::utf8>>
                {[binary | parts], breaks, offset + byte_size(binary)}
            end)

          text = pieces |> Enum.reverse() |> IO.iodata_to_binary()
          assert Graphemes.boundaries(text) == Enum.reverse(boundaries), data
          count + 1
        end
      end)

    assert count > 700
    assert Graphemes.boundaries("") == [0]
  end

  @tag timeout: 120_000
  test "all four forms satisfy every pinned normalization vector equation" do
    count =
      @root
      |> Path.join("NormalizationTest.txt")
      |> File.stream!()
      |> Enum.reduce(0, fn line, count ->
        data = line |> String.split("#", parts: 2) |> hd() |> String.trim()

        if data == "" or String.starts_with?(data, "@") do
          count
        else
          [c1, c2, c3, c4, c5] =
            data
            |> String.split(";")
            |> Enum.take(5)
            |> Enum.map(fn column ->
              column |> String.split() |> Enum.map(&String.to_integer(&1, 16)) |> List.to_string()
            end)

          for value <- [c1, c2, c3], do: assert(Normalization.normalize(value, :nfc) == c2)
          for value <- [c4, c5], do: assert(Normalization.normalize(value, :nfc) == c4)
          for value <- [c1, c2, c3], do: assert(Normalization.normalize(value, :nfd) == c3)
          for value <- [c4, c5], do: assert(Normalization.normalize(value, :nfd) == c5)

          for value <- [c1, c2, c3, c4, c5] do
            assert Normalization.normalize(value, :nfkc) == c4
            assert Normalization.normalize(value, :nfkd) == c5
          end

          count + 1
        end
      end)

    assert count > 19000
  end

  test "long combining and contextual runs preserve exact content with honest boundaries" do
    run = "a" <> String.duplicate("\u0301", 50000)
    assert Graphemes.boundaries(run) == [0, byte_size(run)]
    normalized = Normalization.normalize(run, :nfc)
    assert normalized == "á" <> String.duplicate("\u0301", 49999)
    assert Normalization.normalize(normalized, :nfd) == run

    assert Graphemes.boundaries(String.duplicate("🇦", 101)) ==
             Enum.to_list(0..50) |> Enum.map(&(&1 * 8)) |> Kernel.++([404])

    assert Unicode.tables().version == "17.0.0"
    assert Unicode.tables().uax29_revision == 47

    for {name, %{sha256: digest}} <- Unicode.manifest() do
      assert Base.encode16(:crypto.hash(:sha256, File.read!(Path.join(@root, name))),
               case: :lower
             ) == digest
    end
  end

  @tag timeout: 120_000
  test "normalization leaves all scalar values outside the official Part 1 cases unchanged" do
    {excluded, _} =
      @root
      |> Path.join("NormalizationTest.txt")
      |> File.stream!()
      |> Enum.reduce({MapSet.new(), false}, fn line, {excluded, part1} ->
        data = line |> String.split("#", parts: 2) |> hd() |> String.trim()

        cond do
          String.starts_with?(data, "@Part1") ->
            {excluded, true}

          String.starts_with?(data, "@Part") ->
            {excluded, false}

          part1 and data != "" ->
            [source | _] = String.split(data, ";")
            scalars = source |> String.split() |> Enum.map(&String.to_integer(&1, 16))
            {Enum.reduce(scalars, excluded, &MapSet.put(&2, &1)), true}

          true ->
            {excluded, part1}
        end
      end)

    for scalar <- 0..0x10FFFF,
        scalar not in 0xD800..0xDFFF,
        not MapSet.member?(excluded, scalar) do
      text = <<scalar::utf8>>

      for form <- [:nfc, :nfd, :nfkc, :nfkd],
          do: assert(Normalization.normalize(text, form) == text)
    end
  end
end
