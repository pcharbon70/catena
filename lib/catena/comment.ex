defmodule Catena.Comment do
  @moduledoc """
  Catena 0.1.12 comment scanning, layout integration, and outer-documentation
  attachment over lexer- and parser-supplied events.

  This module is deliberately an abstract source frontend. It does not scan a
  whole file, parse declarations, render Markdown, or execute documentation
  examples.
  """

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion, SourceSpan, SourceText}
  alias Catena.Layout.{LineBreak, Semicolon, Token, Whitespace}

  @comment_revision "0.1.12"
  @markdown_profile "commonmark-0.31.2"
  @doctest_info_string "catena doctest"

  @enforce_keys [:kind, :form, :units, :span, :body_units, :body, :line_breaks]
  defstruct @enforce_keys

  @type kind :: :ordinary | :documentation
  @type form :: :line | :block

  @type t :: %__MODULE__{
          kind: kind(),
          form: form(),
          units: [SourceText.Unit.t()],
          span: SourceSpan.t(),
          body_units: [SourceText.Unit.t()],
          body: String.t(),
          line_breaks: [LineBreak.t()]
        }

  defmodule ScanResult do
    @moduledoc "A scanned comment, the next unconsumed logical-unit index, and selection."

    @enforce_keys [:comment, :next_unit_index, :selection]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            comment: Catena.Comment.t(),
            next_unit_index: non_neg_integer(),
            selection: LanguageSelection.t()
          }
  end

  defmodule Target do
    @moduledoc "A parser-supplied declaration target eligible for outer documentation."

    @enforce_keys [:id, :span]
    defstruct @enforce_keys

    @type t :: %__MODULE__{id: term(), span: SourceSpan.t()}
  end

  defmodule Attachment do
    @moduledoc "Normalized documentation attached to one declaration target."

    @enforce_keys [
      :target_id,
      :target_span,
      :comments,
      :body,
      :markdown_profile,
      :raw_html_policy,
      :doctest_policy,
      :doctest_info_string
    ]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            target_id: term(),
            target_span: SourceSpan.t(),
            comments: [Catena.Comment.t()],
            body: String.t(),
            markdown_profile: String.t(),
            raw_html_policy: :preserve_source_never_execute_unsanitized,
            doctest_policy: :explicit_only,
            doctest_info_string: String.t()
          }
  end

  defmodule Result do
    @moduledoc "A lossless classified event stream, documentation attachments, and selection."

    @enforce_keys [:events, :attachments, :selection]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            events: [Catena.Comment.event()],
            attachments: [Catena.Comment.Attachment.t()],
            selection: LanguageSelection.t()
          }
  end

  @type event ::
          t() | Target.t() | Token.t() | Whitespace.t() | LineBreak.t() | Semicolon.t()

  @spec scan(binary(), keyword()) :: {:ok, ScanResult.t()} | {:error, Diagnostic.t()}
  def scan(source, options \\ []) when is_binary(source) and is_list(options) do
    unit_index = Keyword.get(options, :unit_index, 0)

    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)),
         {:ok, decoded} <- SourceText.decode(source, language_selection: selection),
         :ok <- validate_unit_index(unit_index, decoded),
         {:ok, form, kind, next_unit_index} <- scan_units(decoded, unit_index) do
      units = Enum.slice(decoded.units, unit_index, next_unit_index - unit_index)
      comment = build_comment(form, kind, units)

      {:ok,
       %ScanResult{
         comment: comment,
         next_unit_index: next_unit_index,
         selection: selection
       }}
    end
  end

  @spec resolve([event()], keyword()) :: {:ok, Result.t()} | {:error, Diagnostic.t()}
  def resolve(events, options \\ []) when is_list(events) and is_list(options) do
    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)),
         :ok <- validate_events(events),
         {:ok, classified_events} <- classify_layout(events),
         {:ok, attachments} <- attach_documentation(classified_events) do
      {:ok,
       %Result{
         events: classified_events,
         attachments: attachments,
         selection: selection
       }}
    end
  end

  defp resolve_selection(nil), do: require_comment_revision(LanguageVersion.current_selection())

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_comment_revision(resolved)
    end
  end

  defp require_comment_revision(
         %LanguageSelection{language_revision: @comment_revision} = selection
       ),
       do: {:ok, selection}

  defp require_comment_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "comment resolution requires language revision #{@comment_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @comment_revision,
         frontend: "comments-and-documentation-comments"
       }
     )}
  end

  defp validate_unit_index(index, decoded)
       when is_integer(index) and index >= 0 and index <= length(decoded.units),
       do: :ok

  defp validate_unit_index(index, _decoded) do
    {:error,
     Diagnostic.new("CMT001", "comment unit_index is outside the decoded source stream",
       path: "$.unit_index",
       details: %{reason: "invalid_unit_index", observed: inspect(index)}
     )}
  end

  defp scan_units(decoded, index) do
    units = decoded.units

    cond do
      starts_with?(units, index, [?/, ?/]) ->
        kind =
          if starts_with?(units, index, [?/, ?/, ?/]) and
               scalar_at(units, index + 3) != ?/,
             do: :documentation,
             else: :ordinary

        {:ok, :line, kind, scan_line(units, index + 2)}

      starts_with?(units, index, [?/, ?*]) ->
        kind =
          if starts_with?(units, index, [?/, ?*, ?*]) and
               scalar_at(units, index + 3) not in [?*, ?/],
             do: :documentation,
             else: :ordinary

        scan_block(decoded, index, kind)

      true ->
        span = unit_span_or_eof(decoded, index)

        {:error,
         Diagnostic.new("CMT001", "the selected source position does not begin a comment",
           span: span,
           details: %{reason: "not_a_comment", unit_index: index}
         )}
    end
  end

  defp scan_line(units, index) do
    case Enum.find_index(Enum.drop(units, index), &(&1.scalar == ?\n)) do
      nil -> length(units)
      relative -> index + relative
    end
  end

  defp scan_block(decoded, start, kind) do
    units = decoded.units
    unit_tuple = List.to_tuple(units)

    case scan_block_loop(unit_tuple, tuple_size(unit_tuple), start + 2, 1) do
      {:ok, next_unit_index} ->
        {:ok, :block, kind, next_unit_index}

      {:error, depth} ->
        opener_span = SourceSpan.merge(Enum.at(units, start).span, Enum.at(units, start + 1).span)

        {:error,
         Diagnostic.new("CMT002", "a nested block comment is not closed before end of input",
           span: opener_span,
           details: %{reason: "unterminated_block_comment", remaining_depth: depth}
         )}
    end
  end

  defp scan_block_loop(_units, size, index, depth) when index >= size,
    do: {:error, depth}

  defp scan_block_loop(units, size, index, depth) do
    cond do
      tuple_starts_with?(units, size, index, ?/, ?*) ->
        scan_block_loop(units, size, index + 2, depth + 1)

      tuple_starts_with?(units, size, index, ?*, ?/) and depth == 1 ->
        {:ok, index + 2}

      tuple_starts_with?(units, size, index, ?*, ?/) ->
        scan_block_loop(units, size, index + 2, depth - 1)

      true ->
        scan_block_loop(units, size, index + 1, depth)
    end
  end

  defp tuple_starts_with?(units, size, index, first, second) when index + 1 < size,
    do: elem(units, index).scalar == first and elem(units, index + 1).scalar == second

  defp tuple_starts_with?(_units, _size, _index, _first, _second), do: false

  defp build_comment(form, kind, units) do
    body_units = extract_body(form, kind, units)
    line_breaks = for %SourceText.Unit{scalar: ?\n} = unit <- units, do: %LineBreak{unit: unit}

    %__MODULE__{
      kind: kind,
      form: form,
      units: units,
      span: SourceSpan.merge(hd(units).span, List.last(units).span),
      body_units: body_units,
      body: units_to_text(body_units),
      line_breaks: line_breaks
    }
  end

  defp extract_body(:line, :ordinary, units), do: Enum.drop(units, 2)

  defp extract_body(:line, :documentation, units) do
    units
    |> Enum.drop(3)
    |> drop_one_leading_space()
  end

  defp extract_body(:block, :ordinary, units) do
    units |> Enum.drop(2) |> Enum.drop(-2)
  end

  defp extract_body(:block, :documentation, units) do
    units
    |> Enum.drop(3)
    |> Enum.drop(-2)
    |> drop_one_leading_space()
    |> drop_one_trailing_space()
    |> trim_leading_blank_lines()
    |> trim_trailing_blank_lines()
    |> remove_common_margin()
  end

  defp drop_one_leading_space([%SourceText.Unit{scalar: ?\s} | rest]), do: rest
  defp drop_one_leading_space(units), do: units

  defp drop_one_trailing_space(units) do
    case List.last(units) do
      %SourceText.Unit{scalar: ?\s} -> Enum.drop(units, -1)
      _other -> units
    end
  end

  defp trim_leading_blank_lines(units) do
    {line, rest} = Enum.split_while(units, &(&1.scalar != ?\n))

    if rest != [] and whitespace_only?(line) do
      trim_leading_blank_lines(tl(rest))
    else
      units
    end
  end

  defp trim_trailing_blank_lines(units) do
    case last_line_break_index(units) do
      nil ->
        if whitespace_only?(units), do: [], else: units

      index ->
        trailing = Enum.drop(units, index + 1)

        if whitespace_only?(trailing) do
          units |> Enum.take(index) |> trim_trailing_blank_lines()
        else
          units
        end
    end
  end

  defp last_line_break_index(units) do
    units
    |> Enum.with_index()
    |> Enum.reduce(nil, fn
      {%SourceText.Unit{scalar: ?\n}, index}, _last -> index
      {_unit, _index}, last -> last
    end)
  end

  defp whitespace_only?(units), do: Enum.all?(units, &(&1.scalar in [?\s, ?\t]))

  defp remove_common_margin([]), do: []

  defp remove_common_margin(units) do
    lines = split_lines(units)

    margins =
      for {content, _line_break} <- lines,
          not whitespace_only?(content),
          do: Enum.take_while(content, &(&1.scalar in [?\s, ?\t]))

    common = Enum.reduce(margins, nil, &common_prefix/2) || []

    Enum.flat_map(lines, fn {content, line_break} ->
      trimmed =
        if starts_with_units?(content, common),
          do: Enum.drop(content, length(common)),
          else: content

      if line_break == nil, do: trimmed, else: trimmed ++ [line_break]
    end)
  end

  defp split_lines(units), do: split_lines(units, [], [])

  defp split_lines([], current, lines),
    do: Enum.reverse([{Enum.reverse(current), nil} | lines])

  defp split_lines([%SourceText.Unit{scalar: ?\n} = line_break | rest], current, lines),
    do: split_lines(rest, [], [{Enum.reverse(current), line_break} | lines])

  defp split_lines([unit | rest], current, lines),
    do: split_lines(rest, [unit | current], lines)

  defp common_prefix(left, nil), do: left

  defp common_prefix(left, right) do
    left
    |> Enum.zip(right)
    |> Enum.take_while(fn {left_unit, right_unit} -> left_unit.scalar == right_unit.scalar end)
    |> Enum.map(&elem(&1, 0))
  end

  defp starts_with_units?(_units, []), do: true

  defp starts_with_units?(units, prefix) do
    Enum.map(Enum.take(units, length(prefix)), & &1.scalar) == Enum.map(prefix, & &1.scalar)
  end

  defp validate_events(events) do
    Enum.reduce_while(events, :ok, fn
      %__MODULE__{kind: kind, form: form, line_breaks: line_breaks}, :ok
      when kind in [:ordinary, :documentation] and form in [:line, :block] ->
        if Enum.all?(line_breaks, &match?(%LineBreak{unit: %SourceText.Unit{scalar: ?\n}}, &1)) do
          {:cont, :ok}
        else
          raise ArgumentError, "invalid Catena comment line-break record"
        end

      %Target{span: %SourceSpan{}}, :ok ->
        {:cont, :ok}

      event, :ok
      when is_struct(event, Token) or is_struct(event, Whitespace) or
             is_struct(event, LineBreak) or is_struct(event, Semicolon) ->
        {:cont, :ok}

      event, :ok ->
        raise ArgumentError, "invalid Catena comment event: #{inspect(event)}"
    end)
  end

  defp classify_layout(events) do
    flattened =
      Enum.flat_map(events, fn
        %__MODULE__{line_breaks: line_breaks} -> line_breaks
        %Target{} -> []
        event -> [event]
      end)

    with {:ok, classified} <- Catena.Layout.classify(flattened) do
      classified_lines = for %LineBreak{} = line_break <- classified, do: line_break
      {rebuilt, []} = rebuild_classifications(events, classified_lines, [])
      {:ok, Enum.reverse(rebuilt)}
    end
  end

  defp rebuild_classifications([], remaining, rebuilt), do: {rebuilt, remaining}

  defp rebuild_classifications([%LineBreak{} | rest], [line_break | lines], rebuilt),
    do: rebuild_classifications(rest, lines, [line_break | rebuilt])

  defp rebuild_classifications([%__MODULE__{} = comment | rest], lines, rebuilt) do
    {comment_lines, remaining} = Enum.split(lines, length(comment.line_breaks))
    rebuild_classifications(rest, remaining, [%{comment | line_breaks: comment_lines} | rebuilt])
  end

  defp rebuild_classifications([event | rest], lines, rebuilt),
    do: rebuild_classifications(rest, lines, [event | rebuilt])

  defp attach_documentation(events), do: attach_documentation(events, nil, [])

  defp attach_documentation([], nil, attachments), do: {:ok, Enum.reverse(attachments)}

  defp attach_documentation([], pending, _attachments),
    do:
      documentation_error(
        pending,
        "end_of_input",
        "documentation is not followed by a declaration"
      )

  defp attach_documentation(
         [%__MODULE__{kind: :documentation} = comment | rest],
         nil,
         attachments
       ),
       do: attach_documentation(rest, %{comments: [comment], line_breaks: 0}, attachments)

  defp attach_documentation(
         [%__MODULE__{kind: :documentation} = comment | rest],
         pending,
         attachments
       ),
       do:
         attach_documentation(
           rest,
           %{pending | comments: pending.comments ++ [comment], line_breaks: 0},
           attachments
         )

  defp attach_documentation(
         [%__MODULE__{kind: :ordinary} = comment | _rest],
         pending,
         _attachments
       )
       when pending != nil,
       do:
         documentation_error(
           pending,
           "ordinary_comment_before_target",
           "an ordinary comment interrupts documentation attachment",
           comment.span
         )

  defp attach_documentation([%LineBreak{} = line_break | rest], pending, attachments)
       when pending != nil do
    count = pending.line_breaks + 1

    if count > 1 do
      documentation_error(
        pending,
        "blank_line_before_target",
        "a blank line interrupts documentation attachment",
        line_break.unit.span
      )
    else
      attach_documentation(rest, %{pending | line_breaks: count}, attachments)
    end
  end

  defp attach_documentation([%Whitespace{} | rest], pending, attachments)
       when pending != nil,
       do: attach_documentation(rest, pending, attachments)

  defp attach_documentation([%Target{} = target | rest], %{line_breaks: 1} = pending, attachments) do
    attachment = %Attachment{
      target_id: target.id,
      target_span: target.span,
      comments: pending.comments,
      body: Enum.map_join(pending.comments, "\n", & &1.body),
      markdown_profile: @markdown_profile,
      raw_html_policy: :preserve_source_never_execute_unsanitized,
      doctest_policy: :explicit_only,
      doctest_info_string: @doctest_info_string
    }

    attach_documentation(rest, nil, [attachment | attachments])
  end

  defp attach_documentation([%Target{} = target | _rest], pending, _attachments)
       when pending != nil,
       do:
         documentation_error(
           pending,
           "missing_line_break_before_target",
           "documentation must be separated from its declaration by one logical line break",
           target.span
         )

  defp attach_documentation([event | _rest], pending, _attachments) when pending != nil,
    do:
      documentation_error(
        pending,
        "intervening_event",
        "a significant event interrupts documentation attachment",
        event_span(event)
      )

  defp attach_documentation([_event | rest], nil, attachments),
    do: attach_documentation(rest, nil, attachments)

  defp documentation_error(pending, reason, message, span \\ nil) do
    first = hd(pending.comments)

    {:error,
     Diagnostic.new("DOC001", message,
       span: span || first.span,
       details: %{reason: reason, documentation_span: SourceSpan.to_map(first.span)}
     )}
  end

  defp event_span(%Token{span: span}), do: span
  defp event_span(%Semicolon{unit: unit}), do: unit.span
  defp event_span(%LineBreak{unit: unit}), do: unit.span
  defp event_span(%Whitespace{units: [unit | _rest]}), do: unit.span
  defp event_span(%Target{span: span}), do: span
  defp event_span(%__MODULE__{span: span}), do: span

  defp starts_with?(units, index, scalars) do
    units
    |> Enum.slice(index, length(scalars))
    |> Enum.map(& &1.scalar)
    |> Kernel.==(scalars)
  end

  defp scalar_at(units, index) do
    case Enum.at(units, index) do
      nil -> nil
      unit -> unit.scalar
    end
  end

  defp unit_span_or_eof(decoded, index) do
    case Enum.at(decoded.units, index) do
      nil -> decoded.eof_span
      unit -> unit.span
    end
  end

  defp units_to_text(units),
    do: units |> Enum.map(&<<&1.scalar::utf8>>) |> IO.iodata_to_binary()
end
