defmodule Catena.SourceText do
  @moduledoc """
  A strictly decoded Catena 0.1.9 source-text envelope.

  The original bytes and a logical LF-normalized stream are both retained.
  Each logical Unicode scalar carries a span into the original byte stream so
  later lexical work can preserve exact diagnostics without treating byte,
  UTF-16, grapheme, or display-cell counts as language columns.
  """

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion, SourceSpan}

  defmodule Unit do
    @moduledoc "A logical Unicode scalar and its half-open original-byte span."

    @enforce_keys [:scalar, :span]
    defstruct @enforce_keys

    @type t :: %__MODULE__{scalar: non_neg_integer(), span: SourceSpan.t()}
  end

  @enforce_keys [:source, :text, :units, :eof_span, :selection]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          source: binary(),
          text: binary(),
          units: [Unit.t()],
          eof_span: SourceSpan.t(),
          selection: LanguageSelection.t()
        }

  @source_revision "0.1.9"
  @utf8_bom <<0xEF, 0xBB, 0xBF>>
  @encoding_signatures [
    {<<0x00, 0x00, 0xFE, 0xFF>>, "UTF-32BE"},
    {<<0xFF, 0xFE, 0x00, 0x00>>, "UTF-32LE"},
    {<<0xFE, 0xFF>>, "UTF-16BE"},
    {<<0xFF, 0xFE>>, "UTF-16LE"}
  ]

  @spec decode(binary(), keyword()) :: {:ok, t()} | {:error, Diagnostic.t()}
  def decode(source, options \\ []) when is_binary(source) and is_list(options) do
    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)),
         :ok <- validate_initial_signature(source),
         {:ok, units, text, eof_span} <-
           scan(source, %{index: 0, offset: 0, line: 1, column: 1}, [], []) do
      {:ok,
       %__MODULE__{
         source: source,
         text: text,
         units: units,
         eof_span: eof_span,
         selection: selection
       }}
    end
  end

  defp resolve_selection(nil), do: require_source_revision(LanguageVersion.current_selection())

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_source_revision(resolved)
    end
  end

  defp require_source_revision(
         %LanguageSelection{language_revision: @source_revision} = selection
       ),
       do: {:ok, selection}

  defp require_source_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "source-text decoding requires language revision #{@source_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @source_revision,
         frontend: "source-text"
       }
     )}
  end

  defp validate_initial_signature(source) do
    cond do
      String.starts_with?(source, @utf8_bom) ->
        source_error(
          "SRC002",
          "Catena source text must not begin with a UTF-8 byte-order mark",
          initial_span(3),
          %{reason: "leading_bom"}
        )

      signature =
          Enum.find(@encoding_signatures, fn {bytes, _encoding} ->
            String.starts_with?(source, bytes)
          end) ->
        {bytes, encoding} = signature

        source_error(
          "SRC001",
          "Catena source text must use UTF-8, not #{encoding}",
          initial_span(byte_size(bytes)),
          %{reason: "unsupported_encoding", detected: encoding}
        )

      true ->
        :ok
    end
  end

  defp scan(source, state, units, text) when state.index == byte_size(source) do
    eof = SourceSpan.new(position(state), position(state))
    {:ok, Enum.reverse(units), text |> Enum.reverse() |> IO.iodata_to_binary(), eof}
  end

  defp scan(source, state, units, text) do
    case decode_scalar(source, state.index) do
      {:ok, ?\r, 1} ->
        if state.index + 1 < byte_size(source) and :binary.at(source, state.index + 1) == ?\n do
          finish = %{
            state
            | index: state.index + 2,
              offset: state.offset + 2,
              line: state.line + 1,
              column: 1
          }

          unit = %Unit{scalar: ?\n, span: SourceSpan.new(position(state), position(finish))}
          scan(source, finish, [unit | units], ["\n" | text])
        else
          source_error(
            "SRC003",
            "Catena source text contains a lone carriage return",
            error_span(state, 1),
            %{reason: "lone_carriage_return"}
          )
        end

      {:ok, ?\n, 1} ->
        finish = %{
          state
          | index: state.index + 1,
            offset: state.offset + 1,
            line: state.line + 1,
            column: 1
        }

        unit = %Unit{scalar: ?\n, span: SourceSpan.new(position(state), position(finish))}
        scan(source, finish, [unit | units], ["\n" | text])

      {:ok, scalar, length} ->
        finish = %{
          state
          | index: state.index + length,
            offset: state.offset + length,
            column: state.column + 1
        }

        unit = %Unit{scalar: scalar, span: SourceSpan.new(position(state), position(finish))}
        bytes = binary_part(source, state.index, length)
        scan(source, finish, [unit | units], [bytes | text])

      {:error, length, reason} ->
        source_error(
          "SRC001",
          "Catena source text contains malformed UTF-8",
          error_span(state, length),
          %{reason: reason}
        )
    end
  end

  defp decode_scalar(source, index) do
    first = :binary.at(source, index)

    cond do
      first <= 0x7F ->
        {:ok, first, 1}

      first in 0xC2..0xDF ->
        decode_multibyte(source, index, 2, [{0x80, 0xBF}])

      first == 0xE0 ->
        decode_multibyte(source, index, 3, [{0xA0, 0xBF}, {0x80, 0xBF}])

      first in 0xE1..0xEC or first in 0xEE..0xEF ->
        decode_multibyte(source, index, 3, [{0x80, 0xBF}, {0x80, 0xBF}])

      first == 0xED ->
        decode_multibyte(source, index, 3, [{0x80, 0x9F}, {0x80, 0xBF}])

      first == 0xF0 ->
        decode_multibyte(source, index, 4, [{0x90, 0xBF}, {0x80, 0xBF}, {0x80, 0xBF}])

      first in 0xF1..0xF3 ->
        decode_multibyte(source, index, 4, [{0x80, 0xBF}, {0x80, 0xBF}, {0x80, 0xBF}])

      first == 0xF4 ->
        decode_multibyte(source, index, 4, [{0x80, 0x8F}, {0x80, 0xBF}, {0x80, 0xBF}])

      first in 0x80..0xBF ->
        {:error, 1, "unexpected_continuation_byte"}

      first in 0xC0..0xC1 ->
        {:error, 1, "overlong_leading_byte"}

      true ->
        {:error, 1, "invalid_leading_byte"}
    end
  end

  defp decode_multibyte(source, index, length, ranges) do
    available = byte_size(source) - index

    if available < length do
      {:error, available, "truncated_sequence"}
    else
      bytes = for offset <- 0..(length - 1), do: :binary.at(source, index + offset)
      tails = tl(bytes)

      if Enum.zip(tails, ranges) |> Enum.all?(fn {byte, {low, high}} -> byte in low..high end) do
        <<scalar::utf8>> = :binary.list_to_bin(bytes)
        {:ok, scalar, length}
      else
        {:error, 1, "invalid_continuation_or_scalar_range"}
      end
    end
  end

  defp position(state),
    do: %{offset: state.offset, line: state.line, column: state.column}

  defp initial_span(byte_length) do
    SourceSpan.new(
      %{offset: 0, line: 1, column: 1},
      %{offset: byte_length, line: 1, column: 2}
    )
  end

  defp error_span(state, byte_length) do
    SourceSpan.new(
      position(state),
      %{offset: state.offset + byte_length, line: state.line, column: state.column + 1}
    )
  end

  defp source_error(id, message, span, details),
    do: {:error, Diagnostic.new(id, message, span: span, details: details)}
end
