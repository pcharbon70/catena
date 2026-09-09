defmodule Catena.TextBinaryContractTest do
  use ExUnit.Case, async: true
  @moduletag obligations: ~w(TB-OBL-002 TB-OBL-003 TB-OBL-006 TB-OBL-007 TB-OBL-008 TB-OBL-011)
  alias Catena.Standard.Text, as: T
  alias Catena.Standard.Outcomes, as: O
  alias Catena.Standard.Binary.Pattern
  @limits %{nodes: 10000, bytes: 1_000_000, depth: 1000}
  defp index(unit, n), do: elem(T.index(unit, n), 1)

  test "byte scalar and extended grapheme units stay distinct" do
    text = "a\u0301😀"
    assert {:ok, 7} = T.measure(text, :byte, @limits)
    assert {:ok, 3} = T.measure(text, :scalar, @limits)
    assert {:ok, 2} = T.measure(text, :grapheme, @limits)
    assert {:ok, result} = T.slice(text, index(:grapheme, 0), index(:grapheme, 1), @limits)
    assert result == O.success("a\u0301")
    assert {:ok, result} = T.slice(text, index(:scalar, 1), index(:scalar, 2), @limits)
    assert result == O.success("\u0301")
    assert {:ok, failed} = T.slice(text, index(:byte, 1), index(:scalar, 2), @limits)
    assert failed == O.failure(T.failure(:mixed_index_units))
    assert {:ok, failed} = T.slice(text, index(:byte, 1), index(:byte, 2), @limits)
    assert failed == O.failure(T.failure(:split_scalar))
    assert {:ok, bytes} = T.slice_bytes(text, index(:byte, 1), index(:byte, 2), @limits)
    assert bytes == O.success(<<0xCC>>)
    assert {:ok, failed} = T.slice(text, index(:scalar, -1), index(:scalar, 2), @limits)
    assert failed == O.failure(T.failure({:slice_bounds, -1, 2, 3}))

    assert {:ok, failed} =
             T.slice(text, index(:grapheme, 0), index(:grapheme, Integer.pow(10, 100)), @limits)

    assert match?({:catena_adt, _, 0, _}, failed)
    assert {:ok, empty} = T.slice("", index(:grapheme, 0), index(:grapheme, 0), @limits)
    assert empty == O.success("")
  end

  test "normalization is explicit and all strict encodings round trip scalars without BOM guessing" do
    for text <- ["", "a\u0301", "😀", "\uFEFFx", <<0::utf8, 0xFFFF::utf8, 0x10FFFF::utf8>>],
        encoding <- [:utf8, :utf16_be, :utf16_le, :utf32_be, :utf32_le] do
      assert {:ok, bytes} = T.encode(text, encoding, @limits)
      assert {:ok, decoded} = T.decode(bytes, encoding, @limits)
      assert decoded == O.success(text)
    end

    assert {:ok, "á"} = T.normalize("a\u0301", :nfc, @limits)
    assert {:ok, "fi"} = T.normalize("ﬁ", :nfkc, @limits)
    assert {:ok, "ﬁ"} = T.normalize("ﬁ", :nfc, @limits)

    for {bytes, encoding, offset} <- [
          {<<97, 0xC0, 0xAF>>, :utf8, 1},
          {<<0xF0, 0x9F>>, :utf8, 0},
          {<<0xED, 0xA0, 0x80>>, :utf8, 0},
          {<<0xD8, 0x00>>, :utf16_be, 0},
          {<<0x00, 0xDC>>, :utf16_le, 0},
          {<<0, 97, 0>>, :utf16_be, 2},
          {<<0x00, 0x11, 0x00, 0x00>>, :utf32_be, 0},
          {<<0, 0xD8, 0, 0>>, :utf32_le, 0}
        ] do
      assert {:ok, failed} = T.decode(bytes, encoding, @limits)
      assert failed == O.failure(T.failure({:malformed_encoding, offset}))
    end
  end

  test "pure composition formats only explicitly typed roles and validates complete outputs" do
    assert {:ok, "x😀-42ab00"} =
             T.format(
               [{:text, "x"}, {:character, 0x1F600}, {:integer, -42}, {:bytes_hex, <<0xAB, 0>>}],
               @limits
             )

    assert {:error, :unsupported_format_role} = T.format([{:float, 1.0}], @limits)
    assert {:ok, "a\u0301"} = T.concatenate(["a", "\u0301"], @limits)
    assert {:error, _} = T.decode(<<>>, :utf8, %{nodes: 10, bytes: 1, depth: 10})
    assert {:error, :invalid_text} = T.measure(<<0xFF>>, :scalar, @limits)

    assert {:error, :invalid_text_index} =
             T.verify_index(%{unit: :byte, offset: 1, version: "0.1.65"})
  end

  test "binary segments have explicit width signedness endian and whole-input matching" do
    {:ok, pattern} =
      Pattern.describe([
        {:literal, <<0xAA>>},
        {:integer, 4, :unsigned, :big},
        {:integer, 4, :signed, :big},
        {:integer, 16, :unsigned, :little},
        {:bytes, 2},
        :rest_bytes
      ])

    assert {:ok, result} = Pattern.match(pattern, <<0xAA, 0x3F, 0x34, 0x12, 1, 2, 3>>, @limits)
    assert result == O.present({3, -1, 0x1234, <<1, 2>>, <<3>>})
    assert {:ok, absent} = Pattern.match(pattern, <<0xAA>>, @limits)
    assert absent == O.absent()
    {:ok, fixed} = Pattern.describe([{:integer, 8, :unsigned, :big}])
    assert {:ok, absent} = Pattern.match(fixed, <<1, 2>>, @limits)
    assert absent == O.absent()

    assert {:error, :unaligned_little_integer_width} =
             Pattern.describe([{:integer, 3, :unsigned, :little}])

    assert {:error, _} = Pattern.describe([:rest_bytes, {:bytes, 2}])
    {:ok, huge} = Pattern.describe([{:bytes, Integer.pow(10, 100)}])
    assert {:ok, absent} = Pattern.match(huge, <<1>>, @limits)
    assert absent == O.absent()
  end

  test "bit-field capture agrees with independent integer arithmetic" do
    {:ok, pattern} =
      Pattern.describe([{:integer, 3, :signed, :big}, {:integer, 5, :unsigned, :big}])

    for byte <- 0..255 do
      upper = div(byte, 32)
      signed = if upper >= 4, do: upper - 8, else: upper
      assert {:ok, result} = Pattern.match(pattern, <<byte>>, @limits)
      assert result == O.present({signed, rem(byte, 32)})
    end

    for count <- [1, 2, 8, 16], endian <- [:big, :little] do
      bytes = :binary.copy(<<0xA5>>, count)
      unsigned = Enum.reduce(:binary.bin_to_list(bytes), 0, fn byte, acc -> acc * 256 + byte end)
      {:ok, pattern} = Pattern.describe([{:integer, count * 8, :signed, endian}])
      assert {:ok, result} = Pattern.match(pattern, bytes, @limits)
      assert result == O.present({unsigned - Integer.pow(2, count * 8)})
    end
  end

  test "binary segment and field-width caps have executed boundary witnesses" do
    {:ok, maximum} = Pattern.describe(List.duplicate({:bytes, 0}, 253))
    assert {:ok, result} = Pattern.match(maximum, <<>>, @limits)
    assert result == O.present(List.to_tuple(List.duplicate(<<>>, 253)))
    assert {:error, :invalid_binary_pattern} = Pattern.describe(List.duplicate({:bytes, 0}, 254))
    {:ok, wide} = Pattern.describe([{:integer, 4096, :unsigned, :big}])
    assert {:ok, result} = Pattern.match(wide, :binary.copy(<<0>>, 512), @limits)
    assert result == O.present({0})

    for bits <- [0, 4097],
        do:
          assert(
            {:error, :invalid_binary_pattern} =
              Pattern.describe([{:integer, bits, :unsigned, :big}])
          )
  end
end
