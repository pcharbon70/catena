defmodule Catena.Tokenizer do
  @moduledoc """
  The source-only Catena 0.1.15 whole-source tokenizer.

  One C013 source is scanned into the complete, lossless, ordered stream of
  C014 name, C016 comment, C017 literal, and 0.1.15 operator/punctuation
  tokens with original-byte spans, continuation capabilities, and delimiter
  frame events. It does not type-check, resolve names, parse declarations,
  or evaluate.
  """

  alias Catena.{
    Diagnostic,
    LanguageSelection,
    LanguageVersion,
    SourceSpan,
    SourceText,
    UnicodeData
  }

  @tokenizer_revision "0.1.15"

  defmodule Token do
    @moduledoc "One significant token with span, capabilities, and frame behavior."

    @enforce_keys [:kind, :text, :span, :join_before, :join_after, :frame]
    defstruct @enforce_keys

    @type kind ::
            :name
            | :qualified_name
            | :literal
            | :comment
            | :operator
            | :punctuation

    @type t :: %__MODULE__{
            kind: kind(),
            text: String.t(),
            span: SourceSpan.t(),
            join_before: boolean(),
            join_after: boolean(),
            frame: nil | {:open, atom(), :continued | :block} | {:close, atom()}
          }
  end

  defmodule Trivia do
    @moduledoc "One inter-token whitespace run with its original-byte span."

    @enforce_keys [:text, :span]
    defstruct @enforce_keys

    @type t :: %__MODULE__{text: String.t(), span: SourceSpan.t()}
  end

  defmodule Result do
    @moduledoc "The whole-source token stream, trivia, exact selection, and EOF span."

    @enforce_keys [:tokens, :trivia, :selection, :eof_span]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            tokens: [Token.t()],
            trivia: [Trivia.t()],
            selection: LanguageSelection.t(),
            eof_span: SourceSpan.t()
          }
  end

  @operator_spellings [
    {"->", :operator},
    {"|>", :operator},
    {"<=", :operator},
    {">=", :operator},
    {"==", :operator},
    {"!=", :operator},
    {"&&", :operator},
    {"||", :operator},
    {"+", :operator},
    {"-", :operator},
    {"*", :operator},
    {"<", :operator},
    {">", :operator},
    {"!", :operator},
    {"(", :punctuation},
    {")", :punctuation},
    {"[", :punctuation},
    {"]", :punctuation},
    {"{", :punctuation},
    {"}", :punctuation},
    {",", :punctuation},
    {";", :punctuation},
    {".", :punctuation}
  ]

  @by_length Enum.sort_by(@operator_spellings, fn {text, _} -> -byte_size(text) end)

  @binary_join_both ~w[+ - * < <= > >= == != && || |>]
  @prefix_union ~w(- !)

  @frames %{
    "(" => {:paren, :continued},
    "[" => {:bracket, :continued},
    "{" => {:brace, :block}
  }

  @frame_closers %{"}" => :brace, ")" => :paren, "]" => :bracket}

  @spec tokenize(binary(), keyword()) :: {:ok, Result.t()} | {:error, Diagnostic.t()}
  def tokenize(source, options \\ []) when is_binary(source) and is_list(options) do
    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)),
         {:ok, decoded} <- SourceText.decode(source, language_selection: selection) do
      walk(source, decoded, selection)
    end
  end

  defp resolve_selection(nil),
    do: require_tokenizer_revision(LanguageVersion.legacy_selection(@tokenizer_revision))

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_tokenizer_revision(resolved)
    end
  end

  defp require_tokenizer_revision(
         %LanguageSelection{language_revision: @tokenizer_revision} = selection
       ),
       do: {:ok, selection}

  defp require_tokenizer_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "source tokenization requires language revision #{@tokenizer_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @tokenizer_revision,
         frontend: "operators-and-punctuation"
       }
     )}
  end

  defp walk(source, decoded, selection) do
    units = List.to_tuple(decoded.units)
    size = tuple_size(units)

    walk(source, decoded, units, size, selection, 0, [], [], [])
  end

  defp walk(_source, decoded, _units, size, selection, index, tokens, trivia, frames)
       when index >= size do
    case frames do
      [] ->
        {:ok,
         %Result{
           tokens: Enum.reverse(tokens),
           trivia: Enum.reverse(trivia),
           selection: selection,
           eof_span: decoded.eof_span
         }}

      [{family, _, span} | _] ->
        {:error,
         Diagnostic.new("LAY002", "an open delimiter frame reaches end of input",
           span: span,
           details: %{reason: "unclosed_frame", family: family}
         )}
    end
  end

  defp walk(source, decoded, units, size, selection, index, tokens, trivia, frames) do
    unit = elem(units, index)
    scalar = unit.scalar

    cond do
      scalar in [?\s, ?\t, ?\n] ->
        {finish, _run} = consume_while_unit(units, size, index, &(&1.scalar in [?\s, ?\t, ?\n]))

        walk(
          source,
          decoded,
          units,
          size,
          selection,
          finish,
          tokens,
          [
            trivia_entry(units, index, finish) | trivia
          ],
          frames
        )

      scalar == ?/ and next_scalar(units, size, index + 1) in [?/, ?*] ->
        with {:ok, comment, next_index} <- scan_comment(source, index) do
          walk(
            source,
            decoded,
            units,
            size,
            selection,
            next_index,
            [
              token(:comment, comment_units_text(comment), comment.span) | tokens
            ],
            trivia,
            frames
          )
        end

      true ->
        with {:ok, token, next_index, frames} <-
               scan_significant(source, decoded, units, size, index, scalar, frames) do
          walk(
            source,
            decoded,
            units,
            size,
            selection,
            next_index,
            [token | tokens],
            trivia,
            frames
          )
        end
    end
  end

  defp scan_significant(source, _decoded, units, size, index, scalar, frames) do
    cond do
      ascii_digit?(scalar) or scalar in [?", ?'] ->
        with {:ok, token, next_index} <- scan_literal(source, index) do
          {:ok, token, next_index, frames}
        end

      scalar in [?r, ?b, ?t, ?f] ->
        case scan_literal(source, index) do
          {:ok, token, next_index} -> {:ok, token, next_index, frames}
          {:error, :not_a_literal} -> scan_name(source, units, size, index, frames)
          {:error, %Diagnostic{}} = error -> error
        end

      UnicodeData.xid_start?(scalar) ->
        scan_name(source, units, size, index, frames)

      true ->
        munch_operator(units, size, index, frames)
    end
  end

  defp scan_literal(source, index) do
    case Catena.Literal.scan(source,
           unit_index: index,
           language_selection: LanguageVersion.legacy_selection("0.1.13")
         ) do
      {:ok, result} ->
        literal = result.literal
        {:ok, token(:literal, literal.lexeme, literal.span), result.next_unit_index}

      {:error, %{id: "LIT001", details: %{reason: "not_a_literal"}}} ->
        {:error, :not_a_literal}

      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}
    end
  end

  defp scan_comment(source, index) do
    case Catena.Comment.scan(source,
           unit_index: index,
           language_selection: LanguageVersion.legacy_selection("0.1.12")
         ) do
      {:ok, result} -> {:ok, result.comment, result.next_unit_index}
      {:error, diagnostic} -> {:error, diagnostic}
    end
  end

  defp scan_name(_source, units, size, index, frames) do
    {seg_finish, _} =
      consume_while_unit(units, size, index, &UnicodeData.xid_continue?(&1.scalar))

    {finish, segments} = consume_qualified(units, size, seg_finish, [index])

    text = units_text(units, index, finish)

    validation =
      if length(segments) == 1 do
        Catena.Identifier.parse(text,
          language_selection: LanguageVersion.legacy_selection("0.1.10")
        )
      else
        Catena.parse_qualified_name(text,
          language_selection: LanguageVersion.legacy_selection("0.1.10")
        )
      end

    case validation do
      {:ok, _} ->
        kind = if length(segments) == 1, do: :name, else: :qualified_name
        span = units_span(units, index, finish)
        {:ok, token(kind, text, span), finish, frames}

      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}
    end
  end

  defp consume_qualified(units, size, index, segments) do
    if index < size and elem(units, index).scalar == ?. and index + 1 < size and
         UnicodeData.xid_start?(elem(units, index + 1).scalar) do
      {seg_finish, _} =
        consume_while_unit(units, size, index + 1, &UnicodeData.xid_continue?(&1.scalar))

      consume_qualified(units, size, seg_finish, [index + 1 | segments])
    else
      {index, segments}
    end
  end

  defp munch_operator(units, size, index, frames) do
    case match_inventory(units, size, index) do
      nil ->
        {:error,
         Diagnostic.new(
           "OPR001",
           "the symbol position matches no Catena 0.1.15 operator, punctuation, or atom spelling",
           span: elem(units, index).span,
           details: %{reason: "reserved_or_invalid_spelling"}
         )}

      {text, kind} ->
        apply_operator(text, kind, units, size, index, frames)
    end
  end

  defp apply_operator(text, kind, units, _size, index, frames) do
    span = units_span(units, index, index + string_length(text))

    cond do
      frame = Map.get(@frames, text) ->
        {:ok, token(kind, text, span, {false, false}, {:open, elem(frame, 0), elem(frame, 1)}),
         index + string_length(text), [{elem(frame, 0), elem(frame, 1), span} | frames]}

      family = Map.get(@frame_closers, text) ->
        close_frame(family, text, kind, span, index, frames)

      text in @binary_join_both ->
        {:ok, token(kind, text, span, {true, true}), index + string_length(text), frames}

      text in @prefix_union ->
        {:ok, token(kind, text, span, {true, true}), index + string_length(text), frames}

      true ->
        {:ok, token(kind, text, span, {false, false}), index + string_length(text), frames}
    end
  end

  defp close_frame(family, text, kind, span, index, frames) do
    case pop_family(frames, family) do
      {:ok, remaining} ->
        {:ok, token(kind, text, span, {true, false}, {:close, family}),
         index + string_length(text), remaining}

      :error ->
        {:error,
         Diagnostic.new("LAY002", "a closing delimiter does not match the innermost open frame",
           span: span,
           details: %{reason: "unmatched_or_mismatched_close", family: family}
         )}
    end
  end

  defp pop_family([{family, _, _} | rest], family), do: {:ok, rest}
  defp pop_family(_frames, _family), do: :error

  defp match_inventory(units, size, index) do
    Enum.find(@by_length, fn {text, _kind} ->
      matches_text?(units, size, index, text)
    end)
  end

  defp matches_text?(units, size, index, text) do
    scalars = String.to_charlist(text)

    index + length(scalars) <= size and
      Enum.with_index(scalars, index)
      |> Enum.all?(fn {scalar, unit_index} -> elem(units, unit_index).scalar == scalar end)
  end

  defp token(kind, text, span, capabilities \\ {false, false}, frame \\ nil) do
    {join_before, join_after} = capabilities

    %Token{
      kind: kind,
      text: text,
      span: span,
      join_before: join_before,
      join_after: join_after,
      frame: frame
    }
  end

  defp comment_units_text(comment) do
    comment.units |> Enum.map(&<<&1.scalar::utf8>>) |> IO.iodata_to_binary()
  end

  defp trivia_entry(units, start, finish) do
    %Trivia{text: units_text(units, start, finish), span: units_span(units, start, finish)}
  end

  defp consume_while_unit(units, size, index, predicate) do
    if index < size and predicate.(elem(units, index)) do
      consume_while_unit(units, size, index + 1, predicate)
    else
      {index, index}
    end
  end

  defp next_scalar(_units, size, index) when index >= size, do: nil
  defp next_scalar(units, _size, index), do: elem(units, index).scalar

  defp units_text(units, start, finish) do
    for index <- start..(finish - 1), reduce: "" do
      acc -> acc <> <<elem(units, index).scalar::utf8>>
    end
  end

  defp units_span(units, start, finish) do
    units
    |> tuple_slice(start, finish)
    |> case do
      [] -> nil
      list -> SourceSpan.merge(hd(list).span, List.last(list).span)
    end
  end

  defp tuple_slice(units, start, finish),
    do: for(index <- start..(finish - 1), do: elem(units, index))

  defp string_length(text), do: text |> String.to_charlist() |> length()

  defp ascii_digit?(scalar), do: is_integer(scalar) and scalar in ?0..?9
end
