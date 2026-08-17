defmodule Catena.QualifiedName do
  @moduledoc "A standalone Catena 0.1.10 dot-qualified lexical name."

  alias Catena.{Diagnostic, Identifier, LanguageSelection, SourceSpan, SourceText, UnicodeData}

  @identifier_revision "0.1.10"
  @keywords ~w(
    as condition derives effect exists false fn forall handle handler import let match or
    request resume returns true type uses when where with
  )

  @enforce_keys [:source, :segments, :canonical, :span, :skeleton, :selection]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          source: String.t(),
          segments: [Identifier.t()],
          canonical: String.t(),
          span: SourceSpan.t(),
          skeleton: String.t(),
          selection: LanguageSelection.t()
        }

  @spec keywords() :: [String.t()]
  def keywords, do: @keywords

  @spec parse(binary(), keyword()) :: {:ok, t()} | {:error, Diagnostic.t()}
  def parse(source, options \\ []) when is_binary(source) and is_list(options) do
    with {:ok, source_text} <- SourceText.decode(source, options),
         :ok <- require_identifier_revision(source_text.selection),
         {:ok, pieces} <- split_segments(source_text.units, source_text.eof_span),
         {:ok, segments} <- validate_segments(pieces, source_text.selection) do
      {:ok,
       %__MODULE__{
         source: source,
         segments: segments,
         canonical: Enum.map_join(segments, ".", & &1.canonical),
         span: name_span(source_text.units, source_text.eof_span),
         skeleton: Enum.map_join(segments, ".", & &1.skeleton),
         selection: source_text.selection
       }}
    end
  end

  defp require_identifier_revision(%LanguageSelection{language_revision: @identifier_revision}),
    do: :ok

  defp require_identifier_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "identifier validation requires language revision #{@identifier_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @identifier_revision,
         frontend: "identifiers"
       }
     )}
  end

  defp split_segments([], eof_span),
    do: qualification_error("a qualified name must not be empty", eof_span, "empty_name")

  defp split_segments(units, eof_span), do: split_segments(units, eof_span, [])

  defp split_segments([], _eof_span, pieces), do: {:ok, Enum.reverse(pieces)}

  defp split_segments([%SourceText.Unit{scalar: ?.} = dot | _rest], _eof_span, _pieces),
    do:
      qualification_error("a qualified name contains an empty segment", dot.span, "empty_segment")

  defp split_segments([%SourceText.Unit{scalar: ?`} = opening | rest], eof_span, pieces) do
    case Enum.split_while(rest, &(&1.scalar != ?`)) do
      {_content, []} ->
        escape_error(
          "an escaped identifier is missing its closing backtick",
          opening.span,
          "unclosed"
        )

      {[], [closing | _tail]} ->
        escape_error(
          "an escaped identifier must contain a name",
          SourceSpan.merge(opening.span, closing.span),
          "empty"
        )

      {content, [closing | tail]} ->
        piece = %{
          units: content,
          escaped: true,
          span: SourceSpan.merge(opening.span, closing.span)
        }

        continue_after_segment(tail, eof_span, [piece | pieces])
    end
  end

  defp split_segments(units, eof_span, pieces) do
    {content, tail} = Enum.split_while(units, &(&1.scalar != ?.))

    case Enum.find(content, &(&1.scalar == ?`)) do
      nil ->
        piece = %{units: content, escaped: false, span: units_span(content, eof_span)}
        continue_after_segment(tail, eof_span, [piece | pieces])

      backtick ->
        escape_error(
          "a backtick may appear only as an identifier-segment delimiter",
          backtick.span,
          "misplaced"
        )
    end
  end

  defp continue_after_segment([], _eof_span, pieces), do: {:ok, Enum.reverse(pieces)}

  defp continue_after_segment([%SourceText.Unit{scalar: ?.} = dot], _eof_span, _pieces),
    do: qualification_error("a qualified name must not end with a dot", dot.span, "trailing_dot")

  defp continue_after_segment([%SourceText.Unit{scalar: ?.} | rest], eof_span, pieces),
    do: split_segments(rest, eof_span, pieces)

  defp continue_after_segment([unit | _rest], _eof_span, _pieces),
    do:
      escape_error(
        "a closing backtick must be followed by a dot or the end of the name",
        unit.span,
        "trailing_content"
      )

  defp validate_segments(pieces, selection) do
    pieces
    |> Enum.reduce_while({:ok, []}, fn piece, {:ok, identifiers} ->
      case validate_segment(piece, selection) do
        {:ok, identifier} -> {:cont, {:ok, [identifier | identifiers]}}
        {:error, _diagnostic} = error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, identifiers} -> {:ok, Enum.reverse(identifiers)}
      error -> error
    end
  end

  defp validate_segment(%{units: units} = piece, selection) do
    scalars = Enum.map(units, & &1.scalar)
    canonical = List.to_string(scalars)
    first = List.first(units)

    cond do
      units == [] ->
        qualification_error(
          "a qualified name contains an empty segment",
          piece.span,
          "empty_segment"
        )

      not UnicodeData.xid_start?(first.scalar) ->
        identifier_error(
          "IDN001",
          "an identifier must begin with a Unicode XID_Start character",
          first.span,
          %{reason: "invalid_start", scalar: scalar_label(first.scalar)}
        )

      invalid_continue =
          Enum.find(Enum.drop(units, 1), &(not UnicodeData.xid_continue?(&1.scalar))) ->
        identifier_error(
          "IDN001",
          "an identifier contains a character outside Unicode XID_Continue",
          invalid_continue.span,
          %{reason: "invalid_continue", scalar: scalar_label(invalid_continue.scalar)}
        )

      not UnicodeData.nfc?(canonical) ->
        normalized = UnicodeData.nfc(canonical)
        span = units_span(units, piece.span)

        {:error,
         Diagnostic.new("IDN002", "an identifier must be written in Unicode NFC",
           span: span,
           details: %{normalization: "NFC", replacement: normalized},
           fixes: [
             %{
               "kind" => "source-edit",
               "operation" => "replace",
               "range" => SourceSpan.to_map(span),
               "text" => normalized,
               "applicability" => "machine-applicable"
             }
           ]
         )}

      restricted = Enum.find(units, &(not UnicodeData.identifier_allowed?(&1.scalar))) ->
        identifier_error(
          "IDN003",
          "an identifier contains a character excluded by the Unicode security profile",
          restricted.span,
          %{reason: "identifier_status_restricted", scalar: scalar_label(restricted.scalar)}
        )

      not UnicodeData.highly_restrictive?(canonical) ->
        identifier_error(
          "IDN004",
          "an identifier does not satisfy the Highly Restrictive script profile",
          units_span(units, piece.span),
          %{reason: "mixed_script", scripts: UnicodeData.scripts(canonical)}
        )

      not piece.escaped and canonical in @keywords ->
        {:error,
         Diagnostic.new("IDN005", "#{canonical} is a reserved Catena word",
           span: units_span(units, piece.span),
           details: %{reason: "reserved_word", keyword: canonical},
           fixes: [
             %{
               "kind" => "source-edit",
               "operation" => "replace",
               "range" => SourceSpan.to_map(units_span(units, piece.span)),
               "text" => "`#{canonical}`",
               "applicability" => "machine-applicable"
             }
           ]
         )}

      true ->
        {:ok,
         %Identifier{
           source: if(piece.escaped, do: "`#{canonical}`", else: canonical),
           canonical: canonical,
           escaped: piece.escaped,
           span: piece.span,
           scripts: UnicodeData.scripts(canonical),
           skeleton: UnicodeData.skeleton(canonical),
           selection: selection
         }}
    end
  end

  defp units_span([], fallback), do: fallback
  defp units_span([unit], _fallback), do: unit.span
  defp units_span(units, _fallback), do: SourceSpan.merge(hd(units).span, List.last(units).span)

  defp name_span(units, _eof_span), do: SourceSpan.merge(hd(units).span, List.last(units).span)

  defp identifier_error(id, message, span, details),
    do: {:error, Diagnostic.new(id, message, span: span, details: details)}

  defp qualification_error(message, span, reason),
    do: identifier_error("IDN006", message, span, %{reason: reason})

  defp escape_error(message, span, reason),
    do: identifier_error("IDN005", message, span, %{reason: reason})

  defp scalar_label(scalar),
    do: "U+" <> (scalar |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(4, "0"))
end
