defmodule Catena.Literal do
  @moduledoc """
  The source-only Catena 0.1.13 atomic literal scanner.

  One literal is selected by logical source-unit index. The scanner retains
  original-byte spans while exposing the decoded payload and a lossless split
  between verbatim source and escape contributions. It is not a whole lexer,
  parser, renderer, or runtime-value decoder.
  """

  alias Catena.{Diagnostic, ImplementationLimits, LanguageSelection, LanguageVersion}
  alias Catena.{SourceSpan, SourceText, UnicodeData}

  @literal_revision "0.1.13"

  @enforce_keys [
    :kind,
    :form,
    :lexeme,
    :units,
    :span,
    :payload,
    :pieces,
    :owned_line_breaks
  ]
  defstruct @enforce_keys

  @type kind :: :boolean | :integer | :float | :text | :character | :bytes
  @type form ::
          :keyword
          | {:integer, 2 | 8 | 10 | 16}
          | :decimal_float
          | :cooked
          | {:raw, non_neg_integer()}

  @type t :: %__MODULE__{
          kind: kind(),
          form: form(),
          lexeme: String.t(),
          units: [SourceText.Unit.t()],
          span: SourceSpan.t(),
          payload: boolean() | non_neg_integer() | binary() | Numeric.t(),
          pieces: [Piece.t()],
          owned_line_breaks: [SourceText.Unit.t()]
        }

  defmodule Numeric do
    @moduledoc "Normalized, exact syntax metadata for an integer or decimal float."

    @enforce_keys [
      :base,
      :integral_digits,
      :fractional_digits,
      :exponent_sign,
      :exponent_digits,
      :integer_value
    ]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            base: 2 | 8 | 10 | 16,
            integral_digits: String.t(),
            fractional_digits: String.t() | nil,
            exponent_sign: :none | :plus | :minus,
            exponent_digits: String.t() | nil,
            integer_value: non_neg_integer() | nil
          }
  end

  defmodule Piece do
    @moduledoc "One verbatim or escaped decoded contribution and its exact source units."

    @enforce_keys [:kind, :units, :span, :payload]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            kind: :verbatim | :escape,
            units: [SourceText.Unit.t()],
            span: SourceSpan.t(),
            payload: binary()
          }
  end

  defmodule ScanResult do
    @moduledoc "A literal, the next unconsumed logical-unit index, and exact selection."

    @enforce_keys [:literal, :next_unit_index, :selection]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            literal: Catena.Literal.t(),
            next_unit_index: non_neg_integer(),
            selection: LanguageSelection.t()
          }
  end

  @spec scan(binary(), keyword()) :: {:ok, ScanResult.t()} | {:error, Diagnostic.t()}
  def scan(source, options \\ []) when is_binary(source) and is_list(options) do
    unit_index = Keyword.get(options, :unit_index, 0)

    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)),
         {:ok, decoded} <- SourceText.decode(source, language_selection: selection),
         :ok <- validate_unit_index(unit_index, decoded),
         {:ok, literal, next_unit_index} <- scan_at(decoded, unit_index) do
      {:ok,
       %ScanResult{
         literal: literal,
         next_unit_index: next_unit_index,
         selection: selection
       }}
    end
  end

  defp resolve_selection(nil),
    do: require_literal_revision(LanguageVersion.legacy_selection(@literal_revision))

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_literal_revision(resolved)
    end
  end

  defp require_literal_revision(
         %LanguageSelection{language_revision: @literal_revision} = selection
       ),
       do: {:ok, selection}

  defp require_literal_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "literal scanning requires language revision #{@literal_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @literal_revision,
         frontend: "literal-grammar"
       }
     )}
  end

  defp validate_unit_index(index, decoded)
       when is_integer(index) and index >= 0 and index <= length(decoded.units),
       do: :ok

  defp validate_unit_index(index, _decoded) do
    {:error,
     Diagnostic.new("LIT001", "literal unit_index is outside the decoded source stream",
       path: "$.unit_index",
       details: %{reason: "invalid_unit_index", observed: inspect(index)}
     )}
  end

  defp scan_at(decoded, index) do
    units = List.to_tuple(decoded.units)
    size = tuple_size(units)

    cond do
      raw_opener(units, size, index, [?b, ?r]) != :no ->
        {:ok, hashes, content_start} = raw_opener(units, size, index, [?b, ?r])
        scan_raw(decoded, units, size, index, content_start, hashes, :bytes)

      starts_with?(units, size, index, [?b, ?\"]) ->
        scan_cooked(decoded, units, size, index, index + 2, :bytes, ?\")

      raw_opener(units, size, index, [?r]) != :no ->
        {:ok, hashes, content_start} = raw_opener(units, size, index, [?r])
        scan_raw(decoded, units, size, index, content_start, hashes, :text)

      scalar_at(units, size, index) == ?\" ->
        scan_cooked(decoded, units, size, index, index + 1, :text, ?\")

      scalar_at(units, size, index) == ?' ->
        scan_cooked(decoded, units, size, index, index + 1, :character, ?')

      keyword_at?(units, size, index, "true") ->
        build_keyword(decoded, index, index + 4, true)

      keyword_at?(units, size, index, "false") ->
        build_keyword(decoded, index, index + 5, false)

      ascii_digit?(scalar_at(units, size, index)) ->
        scan_number(decoded, units, size, index)

      true ->
        {:error,
         Diagnostic.new("LIT001", "the selected source position does not begin an atomic literal",
           span: unit_span_or_eof(decoded, index),
           details: %{reason: "not_a_literal", unit_index: index}
         )}
    end
  end

  defp build_keyword(decoded, start, finish, value) do
    units = Enum.slice(decoded.units, start, finish - start)

    literal = %__MODULE__{
      kind: :boolean,
      form: :keyword,
      lexeme: units_to_text(units),
      units: units,
      span: units_span(units),
      payload: value,
      pieces: [],
      owned_line_breaks: []
    }

    {:ok, literal, finish}
  end

  defp keyword_at?(units, size, index, keyword) do
    scalars = String.to_charlist(keyword)
    finish = index + length(scalars)

    starts_with?(units, size, index, scalars) and
      not xid_continue?(scalar_at(units, size, finish))
  end

  defp scan_raw(decoded, units, size, start, content_start, hashes, kind) do
    case scan_raw_loop(units, size, content_start, hashes, kind, [], [], []) do
      {:ok, finish, payload_parts, pieces, line_breaks} ->
        payload = payload_parts |> Enum.reverse() |> IO.iodata_to_binary()
        all_units = Enum.slice(decoded.units, start, finish - start)
        span = units_span(all_units)

        with :ok <- validate_string_payload(kind, payload, span) do
          literal = %__MODULE__{
            kind: kind,
            form: {:raw, hashes},
            lexeme: units_to_text(all_units),
            units: all_units,
            span: span,
            payload: payload,
            pieces: Enum.reverse(pieces),
            owned_line_breaks: Enum.reverse(line_breaks)
          }

          {:ok, literal, finish}
        end

      {:error, reason, span} ->
        literal_error("LIT003", "raw byte literal contains a non-ASCII scalar", span, reason)

      :unterminated ->
        opener_finish = content_start
        opener_units = Enum.slice(decoded.units, start, opener_finish - start)

        literal_error(
          "LIT002",
          "raw literal delimiter is not closed before end of input",
          units_span(opener_units),
          "unterminated_raw_literal",
          %{hash_count: hashes}
        )
    end
  end

  defp scan_raw_loop(_units, size, index, _hashes, _kind, _payload, _pieces, _breaks)
       when index >= size,
       do: :unterminated

  defp scan_raw_loop(units, size, index, hashes, kind, payload, pieces, breaks) do
    if raw_closer?(units, size, index, hashes) do
      {:ok, index + 1 + hashes, payload, pieces, breaks}
    else
      unit = elem(units, index)

      cond do
        kind == :bytes and unit.scalar > 0x7F ->
          {:error, "non_ascii_raw_byte", unit.span}

        true ->
          contribution = scalar_binary(unit.scalar)
          piece = piece(:verbatim, [unit], contribution)
          next_breaks = if unit.scalar == ?\n, do: [unit | breaks], else: breaks

          scan_raw_loop(
            units,
            size,
            index + 1,
            hashes,
            kind,
            [contribution | payload],
            [piece | pieces],
            next_breaks
          )
      end
    end
  end

  defp scan_cooked(decoded, units, size, start, content_start, kind, delimiter) do
    case scan_cooked_loop(units, size, content_start, kind, delimiter, [], []) do
      {:ok, finish, payload_parts, pieces} ->
        payload = payload_parts |> Enum.reverse() |> IO.iodata_to_binary()
        all_units = Enum.slice(decoded.units, start, finish - start)
        span = units_span(all_units)

        with {:ok, final_payload} <- validate_cooked_payload(kind, payload, span),
             :ok <- validate_string_payload(kind, payload, span) do
          literal = %__MODULE__{
            kind: kind,
            form: :cooked,
            lexeme: units_to_text(all_units),
            units: all_units,
            span: span,
            payload: final_payload,
            pieces: Enum.reverse(pieces),
            owned_line_breaks: []
          }

          {:ok, literal, finish}
        end

      {:error, reason, span} ->
        literal_error("LIT003", cooked_error_message(reason), span, reason)

      :unterminated ->
        opener_units = Enum.slice(decoded.units, start, content_start - start)

        literal_error(
          "LIT002",
          "cooked literal delimiter is not closed before end of input",
          units_span(opener_units),
          "unterminated_cooked_literal"
        )
    end
  end

  defp scan_cooked_loop(_units, size, index, _kind, _delimiter, _payload, _pieces)
       when index >= size,
       do: :unterminated

  defp scan_cooked_loop(units, size, index, kind, delimiter, payload, pieces) do
    unit = elem(units, index)

    cond do
      unit.scalar == delimiter ->
        {:ok, index + 1, payload, pieces}

      unit.scalar == ?\n ->
        {:error, "cooked_line_break", unit.span}

      unit.scalar == ?\\ ->
        case scan_escape(units, size, index, kind) do
          {:ok, finish, contribution, escape_units} ->
            scan_cooked_loop(
              units,
              size,
              finish,
              kind,
              delimiter,
              [contribution | payload],
              [piece(:escape, escape_units, contribution) | pieces]
            )

          {:error, reason, span} ->
            {:error, reason, span}
        end

      kind == :bytes and unit.scalar > 0x7F ->
        {:error, "non_ascii_cooked_byte", unit.span}

      true ->
        contribution = scalar_binary(unit.scalar)

        scan_cooked_loop(
          units,
          size,
          index + 1,
          kind,
          delimiter,
          [contribution | payload],
          [piece(:verbatim, [unit], contribution) | pieces]
        )
    end
  end

  defp scan_escape(units, size, start, kind) do
    escape = scalar_at(units, size, start + 1)

    simple =
      case escape do
        ?0 -> 0
        ?t -> ?\t
        ?n -> ?\n
        ?r -> ?\r
        ?\\ -> ?\\
        ?\" -> ?\"
        ?' -> ?'
        _ -> nil
      end

    cond do
      is_integer(simple) ->
        escape_success(units, start, start + 2, simple, kind)

      escape == ?x ->
        scan_hex_escape(units, size, start, kind)

      escape == ?u and kind == :bytes ->
        {:error, "unicode_escape_in_byte_literal", range_span(units, size, start, start + 2)}

      escape == ?u ->
        scan_unicode_escape(units, size, start)

      escape == ?\n ->
        {:error, "backslash_line_continuation", range_span(units, size, start, start + 2)}

      true ->
        {:error, "unknown_escape", range_span(units, size, start, min(start + 2, size))}
    end
  end

  defp scan_hex_escape(units, size, start, kind) do
    first = scalar_at(units, size, start + 2)
    second = scalar_at(units, size, start + 3)

    if hex_digit?(first) and hex_digit?(second) do
      scalar = hex_value(first) * 16 + hex_value(second)

      if kind != :bytes and scalar > 0x7F do
        {:error, "non_ascii_text_hex_escape", range_span(units, size, start, start + 4)}
      else
        escape_success(units, start, start + 4, scalar, kind)
      end
    else
      {:error, "invalid_hex_escape", range_span(units, size, start, min(start + 4, size))}
    end
  end

  defp scan_unicode_escape(units, size, start) do
    if scalar_at(units, size, start + 2) == ?{ do
      finish = consume_while(units, size, start + 3, &hex_digit?/1)
      digit_count = finish - (start + 3)

      cond do
        digit_count not in 1..6 ->
          {:error, "invalid_unicode_escape_length", range_span(units, size, start, finish)}

        scalar_at(units, size, finish) != ?} ->
          {:error, "invalid_unicode_escape",
           range_span(units, size, start, min(finish + 1, size))}

        true ->
          digits = range_text(units, start + 3, finish)
          scalar = String.to_integer(digits, 16)

          if unicode_scalar?(scalar) do
            escape_success(units, start, finish + 1, scalar, :text)
          else
            {:error, "invalid_unicode_scalar", range_span(units, size, start, finish + 1)}
          end
      end
    else
      {:error, "invalid_unicode_escape", range_span(units, size, start, min(start + 3, size))}
    end
  end

  defp escape_success(units, start, finish, scalar, kind) do
    escape_units = tuple_slice(units, start, finish)
    contribution = if kind == :bytes, do: <<scalar>>, else: scalar_binary(scalar)
    {:ok, finish, contribution, escape_units}
  end

  defp validate_cooked_payload(:character, payload, span) do
    case String.to_charlist(payload) do
      [scalar] ->
        {:ok, scalar}

      scalars ->
        literal_error(
          "LIT003",
          "a character literal must decode to one Unicode scalar",
          span,
          "invalid_character_arity",
          %{observed: length(scalars)}
        )
    end
  end

  defp validate_cooked_payload(_kind, payload, _span), do: {:ok, payload}

  defp validate_string_payload(kind, payload, span) when kind in [:text, :bytes],
    do: ImplementationLimits.validate_decoded_literal_bytes(payload, span)

  defp validate_string_payload(_kind, _payload, _span), do: :ok

  defp scan_number(decoded, units, size, start) do
    if scalar_at(units, size, start) == ?0 and
         scalar_at(units, size, start + 1) in [?b, ?o, ?x] do
      scan_based_integer(decoded, units, size, start)
    else
      scan_decimal_number(decoded, units, size, start)
    end
  end

  defp scan_based_integer(decoded, units, size, start) do
    prefix = scalar_at(units, size, start + 1)
    base = %{?b => 2, ?o => 8, ?x => 16}[prefix]
    digits_start = start + 2
    finish = consume_while(units, size, digits_start, &ascii_alnum_or_underscore?/1)

    with {:ok, digits} <- validate_digit_sequence(units, size, digits_start, finish, base),
         :ok <- reject_numeric_suffix(decoded, units, size, start, finish),
         false <- scalar_at(units, size, finish) == ?. do
      value = String.to_integer(digits, base)
      build_integer(decoded, start, finish, base, digits, value)
    else
      true ->
        numeric_error(decoded, start, min(finish + 1, size), "based_float_not_supported")

      {:error, reason} when is_binary(reason) ->
        numeric_error(decoded, start, max(finish, digits_start), reason)

      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}
    end
  end

  defp scan_decimal_number(decoded, units, size, start) do
    integral_finish = consume_while(units, size, start, &ascii_digit_or_underscore?/1)

    with {:ok, integral} <- validate_digit_sequence(units, size, start, integral_finish, 10),
         :ok <- validate_decimal_leading_zero(integral) do
      dot = scalar_at(units, size, integral_finish)
      after_dot = scalar_at(units, size, integral_finish + 1)

      cond do
        dot == ?. and ascii_digit?(after_dot) ->
          fraction_start = integral_finish + 1

          fraction_finish =
            consume_while(units, size, fraction_start, &ascii_digit_or_underscore?/1)

          with {:ok, fraction} <-
                 validate_digit_sequence(units, size, fraction_start, fraction_finish, 10) do
            scan_decimal_exponent(
              decoded,
              units,
              size,
              start,
              fraction_finish,
              integral,
              fraction
            )
          else
            {:error, reason} -> numeric_error(decoded, start, fraction_finish, reason)
          end

        dot == ?. and after_dot == ?_ ->
          numeric_error(decoded, start, min(integral_finish + 2, size), "invalid_fraction_digits")

        scalar_at(units, size, integral_finish) in [?e, ?E] ->
          scan_decimal_exponent(decoded, units, size, start, integral_finish, integral, nil)

        true ->
          with :ok <- reject_numeric_suffix(decoded, units, size, start, integral_finish) do
            value = String.to_integer(integral, 10)
            build_integer(decoded, start, integral_finish, 10, integral, value)
          end
      end
    else
      {:error, reason} -> numeric_error(decoded, start, integral_finish, reason)
    end
  end

  defp scan_decimal_exponent(decoded, units, size, start, index, integral, fraction) do
    if scalar_at(units, size, index) in [?e, ?E] do
      sign_index = index + 1

      {sign, digits_start} =
        case scalar_at(units, size, sign_index) do
          ?+ -> {:plus, sign_index + 1}
          ?- -> {:minus, sign_index + 1}
          _ -> {:none, sign_index}
        end

      finish = consume_while(units, size, digits_start, &ascii_digit_or_underscore?/1)

      with {:ok, exponent} <- validate_digit_sequence(units, size, digits_start, finish, 10),
           :ok <- reject_numeric_suffix(decoded, units, size, start, finish) do
        build_float(decoded, start, finish, integral, fraction, sign, exponent)
      else
        {:error, reason} when is_binary(reason) ->
          numeric_error(decoded, start, max(finish, digits_start), reason)

        {:error, %Diagnostic{} = diagnostic} ->
          {:error, diagnostic}
      end
    else
      with :ok <- reject_numeric_suffix(decoded, units, size, start, index) do
        build_float(decoded, start, index, integral, fraction, :none, nil)
      end
    end
  end

  defp build_integer(decoded, start, finish, base, digits, value) do
    all_units = Enum.slice(decoded.units, start, finish - start)
    span = units_span(all_units)

    with :ok <- ImplementationLimits.validate_integer_magnitudes(value, span) do
      numeric = %Numeric{
        base: base,
        integral_digits: digits,
        fractional_digits: nil,
        exponent_sign: :none,
        exponent_digits: nil,
        integer_value: value
      }

      {:ok, numeric_literal(:integer, {:integer, base}, all_units, span, numeric), finish}
    end
  end

  defp build_float(decoded, start, finish, integral, fraction, sign, exponent) do
    all_units = Enum.slice(decoded.units, start, finish - start)
    span = units_span(all_units)

    numeric = %Numeric{
      base: 10,
      integral_digits: integral,
      fractional_digits: fraction,
      exponent_sign: sign,
      exponent_digits: exponent,
      integer_value: nil
    }

    {:ok, numeric_literal(:float, :decimal_float, all_units, span, numeric), finish}
  end

  defp numeric_literal(kind, form, units, span, numeric) do
    %__MODULE__{
      kind: kind,
      form: form,
      lexeme: units_to_text(units),
      units: units,
      span: span,
      payload: numeric,
      pieces: [],
      owned_line_breaks: []
    }
  end

  defp validate_digit_sequence(_units, _size, start, finish, _base) when start == finish,
    do: {:error, "missing_numeric_digits"}

  defp validate_digit_sequence(units, size, start, finish, base) do
    scalars = for index <- start..(finish - 1), do: scalar_at(units, size, index)
    predicate = &valid_digit?(&1, base)

    cond do
      hd(scalars) == ?_ or List.last(scalars) == ?_ ->
        {:error, "misplaced_numeric_separator"}

      Enum.any?(Enum.chunk_every(scalars, 2, 1, :discard), &(&1 == [?_, ?_])) ->
        {:error, "repeated_numeric_separator"}

      Enum.any?(scalars, fn scalar -> scalar != ?_ and not predicate.(scalar) end) ->
        {:error, "invalid_digit_for_base"}

      true ->
        {:ok, scalars |> Enum.reject(&(&1 == ?_)) |> List.to_string()}
    end
  end

  defp validate_decimal_leading_zero("0"), do: :ok

  defp validate_decimal_leading_zero(<<"0", _rest::binary>>),
    do: {:error, "redundant_leading_zero"}

  defp validate_decimal_leading_zero(_digits), do: :ok

  defp reject_numeric_suffix(decoded, units, size, start, finish) do
    if xid_continue?(scalar_at(units, size, finish)) do
      numeric_error(decoded, start, min(finish + 1, size), "invalid_numeric_suffix")
    else
      :ok
    end
  end

  defp numeric_error(decoded, start, finish, reason) do
    span = range_span_from_list(decoded.units, start, finish, decoded.eof_span)
    literal_error("LIT003", "numeric literal has an invalid spelling", span, reason)
  end

  defp raw_opener(units, size, index, prefix) do
    if starts_with?(units, size, index, prefix) do
      hashes_start = index + length(prefix)
      quote_index = consume_while(units, size, hashes_start, &(&1 == ?#))

      if scalar_at(units, size, quote_index) == ?\" do
        {:ok, quote_index - hashes_start, quote_index + 1}
      else
        :no
      end
    else
      :no
    end
  end

  defp raw_closer?(units, size, index, hashes) do
    scalar_at(units, size, index) == ?\" and
      hashes_match?(units, size, index + 1, hashes)
  end

  defp hashes_match?(_units, _size, _index, 0), do: true

  defp hashes_match?(units, size, index, remaining) do
    scalar_at(units, size, index) == ?# and
      hashes_match?(units, size, index + 1, remaining - 1)
  end

  defp starts_with?(_units, size, index, scalars)
       when index < 0 or index + length(scalars) > size,
       do: false

  defp starts_with?(units, _size, index, scalars) do
    scalars
    |> Enum.with_index(index)
    |> Enum.all?(fn {scalar, unit_index} -> elem(units, unit_index).scalar == scalar end)
  end

  defp consume_while(units, size, index, predicate) do
    if index < size and predicate.(elem(units, index).scalar) do
      consume_while(units, size, index + 1, predicate)
    else
      index
    end
  end

  defp scalar_at(_units, size, index) when index < 0 or index >= size, do: nil
  defp scalar_at(units, _size, index), do: elem(units, index).scalar

  defp piece(kind, units, payload),
    do: %Piece{kind: kind, units: units, span: units_span(units), payload: payload}

  defp tuple_slice(units, start, finish),
    do: for(index <- start..(finish - 1), do: elem(units, index))

  defp range_text(units, start, finish),
    do: units |> tuple_slice(start, finish) |> units_to_text()

  defp range_span(units, size, start, finish) do
    bounded_finish = min(max(finish, start + 1), size)

    if start < size and bounded_finish > start do
      units |> tuple_slice(start, bounded_finish) |> units_span()
    else
      nil
    end
  end

  defp range_span_from_list(units, start, finish, eof_span) do
    selected = Enum.slice(units, start, max(finish - start, 0))

    if selected == [],
      do: Enum.at(units, start, %{span: eof_span}).span,
      else: units_span(selected)
  end

  defp units_span([first | _] = units), do: SourceSpan.merge(first.span, List.last(units).span)

  defp unit_span_or_eof(decoded, index) do
    case Enum.at(decoded.units, index) do
      nil -> decoded.eof_span
      unit -> unit.span
    end
  end

  defp units_to_text(units),
    do: units |> Enum.map(&scalar_binary(&1.scalar)) |> IO.iodata_to_binary()

  defp scalar_binary(scalar), do: <<scalar::utf8>>

  defp ascii_digit?(scalar), do: is_integer(scalar) and scalar in ?0..?9
  defp ascii_digit_or_underscore?(scalar), do: ascii_digit?(scalar) or scalar == ?_

  defp ascii_alnum_or_underscore?(scalar),
    do: ascii_digit?(scalar) or scalar in ?a..?z or scalar in ?A..?Z or scalar == ?_

  defp valid_digit?(scalar, 2), do: scalar in ?0..?1
  defp valid_digit?(scalar, 8), do: scalar in ?0..?7
  defp valid_digit?(scalar, 10), do: scalar in ?0..?9
  defp valid_digit?(scalar, 16), do: hex_digit?(scalar)

  defp hex_digit?(scalar),
    do: is_integer(scalar) and (scalar in ?0..?9 or scalar in ?a..?f or scalar in ?A..?F)

  defp hex_value(scalar) when scalar in ?0..?9, do: scalar - ?0
  defp hex_value(scalar) when scalar in ?a..?f, do: scalar - ?a + 10
  defp hex_value(scalar) when scalar in ?A..?F, do: scalar - ?A + 10

  defp xid_continue?(nil), do: false
  defp xid_continue?(scalar), do: UnicodeData.xid_continue?(scalar)

  defp unicode_scalar?(scalar),
    do: scalar >= 0 and scalar <= 0x10FFFF and scalar not in 0xD800..0xDFFF

  defp cooked_error_message("cooked_line_break"),
    do: "a cooked literal must not contain a source line break"

  defp cooked_error_message("non_ascii_cooked_byte"),
    do: "a cooked byte literal must contain direct ASCII only"

  defp cooked_error_message("non_ascii_text_hex_escape"),
    do: "a text or character hex escape must be ASCII"

  defp cooked_error_message("unicode_escape_in_byte_literal"),
    do: "a byte literal must not contain a Unicode escape"

  defp cooked_error_message(_reason), do: "cooked literal contains an invalid escape"

  defp literal_error(id, message, span, reason, extra \\ %{}) do
    {:error,
     Diagnostic.new(id, message,
       span: span,
       details: Map.merge(%{reason: reason}, extra)
     )}
  end
end
