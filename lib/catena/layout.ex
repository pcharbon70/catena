defmodule Catena.Layout do
  @moduledoc """
  Catena 0.1.11 whitespace, separator, and line-continuation resolution.

  The layout engine consumes events produced by a future lexer. Token payloads
  remain opaque: only line-joining capabilities and delimiter frames affect
  layout. This keeps indentation non-semantic and lets later syntax work assign
  concrete operators and delimiters without changing the layout algorithm.
  """

  alias Catena.{Diagnostic, LanguageSelection, LanguageVersion, SourceSpan, SourceText}

  @layout_revision "0.1.11"

  defmodule Token do
    @moduledoc "A significant opaque token with layout capabilities."

    @enforce_keys [:value, :span]
    defstruct [:value, :span, join_before: false, join_after: false, delimiter: :none]

    @type delimiter ::
            :none
            | {:open, term(), :continued | :block}
            | {:close, term()}

    @type t :: %__MODULE__{
            value: term(),
            span: SourceSpan.t(),
            join_before: boolean(),
            join_after: boolean(),
            delimiter: delimiter()
          }
  end

  defmodule Whitespace do
    @moduledoc "A nonempty run of horizontal source-text units."

    @enforce_keys [:units]
    defstruct @enforce_keys

    @type t :: %__MODULE__{units: [SourceText.Unit.t()]}
  end

  defmodule LineBreak do
    @moduledoc "A C013 logical LF and its C015 layout classification."

    @enforce_keys [:unit]
    defstruct [:unit, classification: nil]

    @type classification :: nil | :soft | :separator | :blank
    @type t :: %__MODULE__{unit: SourceText.Unit.t(), classification: classification()}
  end

  defmodule Semicolon do
    @moduledoc "A concrete explicit separator."

    @enforce_keys [:unit]
    defstruct @enforce_keys

    @type t :: %__MODULE__{unit: SourceText.Unit.t()}
  end

  defmodule Result do
    @moduledoc "A lossless classified layout stream and exact language selection."

    @enforce_keys [:events, :selection]
    defstruct @enforce_keys

    @type t :: %__MODULE__{
            events: [Catena.Layout.event()],
            selection: LanguageSelection.t()
          }
  end

  @type event :: Token.t() | Whitespace.t() | LineBreak.t() | Semicolon.t()
  @type frame :: %{family: term(), mode: :continued | :block, span: SourceSpan.t()}

  @spec resolve([event()], keyword()) :: {:ok, Result.t()} | {:error, Diagnostic.t()}
  def resolve(events, options \\ []) when is_list(events) and is_list(options) do
    with {:ok, selection} <- resolve_selection(Keyword.get(options, :language_selection)),
         :ok <- validate_events(events),
         {:ok, resolved, frames, previous} <- walk(events, [], nil, []),
         :ok <- finish(frames, previous) do
      {:ok, %Result{events: Enum.reverse(resolved), selection: selection}}
    end
  end

  defp resolve_selection(nil), do: require_layout_revision(LanguageVersion.current_selection())

  defp resolve_selection(selection) do
    with {:ok, resolved} <- LanguageVersion.resolve_selection(selection) do
      require_layout_revision(resolved)
    end
  end

  defp require_layout_revision(
         %LanguageSelection{language_revision: @layout_revision} = selection
       ),
       do: {:ok, selection}

  defp require_layout_revision(%LanguageSelection{} = selection) do
    {:error,
     Diagnostic.new(
       "EDN001",
       "layout resolution requires language revision #{@layout_revision}",
       path: "$.language_revision",
       details: %{
         selected: selection.language_revision,
         required: @layout_revision,
         frontend: "whitespace-and-layout"
       }
     )}
  end

  defp validate_events(events) do
    Enum.reduce_while(events, :ok, fn event, :ok ->
      case validate_event(event) do
        :ok -> {:cont, :ok}
        {:error, _diagnostic} = error -> {:halt, error}
      end
    end)
  end

  defp validate_event(%Whitespace{units: []}) do
    {:error,
     Diagnostic.new("LAY001", "a horizontal-whitespace event must not be empty",
       details: %{reason: "empty_whitespace_event"}
     )}
  end

  defp validate_event(%Whitespace{units: units}) do
    case Enum.find(units, &(&1.scalar not in [?\s, ?\t])) do
      nil ->
        :ok

      unit ->
        layout_error(
          "LAY001",
          "Catena layout whitespace is limited to ASCII space, tab, and logical LF",
          unit.span,
          %{
            reason: "prohibited_whitespace",
            scalar: scalar_label(unit.scalar)
          }
        )
    end
  end

  defp validate_event(%LineBreak{unit: %SourceText.Unit{scalar: ?\n}}), do: :ok

  defp validate_event(%LineBreak{unit: unit}) do
    layout_error(
      "LAY001",
      "a layout line-break event must contain the C013 logical LF",
      unit.span,
      %{reason: "invalid_line_break", scalar: scalar_label(unit.scalar)}
    )
  end

  defp validate_event(%Semicolon{unit: %SourceText.Unit{scalar: ?;}}), do: :ok
  defp validate_event(%Token{}), do: :ok

  defp validate_event(event) do
    raise ArgumentError, "invalid Catena layout event: #{inspect(event)}"
  end

  defp walk([], frames, previous, resolved),
    do: {:ok, resolved, frames, previous}

  defp walk([%Whitespace{} | _] = events, frames, previous, resolved),
    do: walk_gap(events, frames, previous, resolved)

  defp walk([%LineBreak{} | _] = events, frames, previous, resolved),
    do: walk_gap(events, frames, previous, resolved)

  defp walk([%Semicolon{} = semicolon | rest], frames, previous, resolved) do
    if previous != nil and previous.join_after do
      layout_error(
        "LAY003",
        "a semicolon cannot interrupt a token that requires a following expression",
        semicolon.unit.span,
        %{reason: "semicolon_interrupts_continuation"}
      )
    else
      walk(rest, frames, nil, [semicolon | resolved])
    end
  end

  defp walk([%Token{} = token | rest], frames, previous, resolved) do
    cond do
      token.join_before and previous == nil ->
        layout_error(
          "LAY003",
          "a token requiring a preceding expression cannot begin after a separator",
          token.span,
          %{reason: "missing_left_expression"}
        )

      true ->
        with {:ok, next_frames} <- update_frames(frames, token) do
          walk(rest, next_frames, token, [token | resolved])
        end
    end
  end

  defp walk_gap(events, frames, previous, resolved) do
    {gap, rest} =
      Enum.split_while(events, &(match?(%Whitespace{}, &1) or match?(%LineBreak{}, &1)))

    next = List.first(rest)
    {classified, separated?} = classify_gap(gap, frames, previous, next)
    next_previous = if separated?, do: nil, else: previous
    walk(rest, frames, next_previous, Enum.reverse(classified, resolved))
  end

  defp classify_gap(gap, frames, previous, next) do
    has_line_break? = Enum.any?(gap, &match?(%LineBreak{}, &1))

    cond do
      not has_line_break? ->
        {gap, false}

      previous == nil ->
        {classify_line_breaks(gap, :blank), false}

      soft_gap?(frames, previous, next) ->
        {classify_line_breaks(gap, :soft), false}

      true ->
        {classify_hard_gap(gap), true}
    end
  end

  defp soft_gap?(frames, previous, next) do
    continued_frame?(frames) or previous.join_after or
      match?(%Token{join_before: true}, next)
  end

  defp continued_frame?([%{mode: :continued} | _rest]), do: true
  defp continued_frame?(_frames), do: false

  defp classify_line_breaks(events, classification) do
    Enum.map(events, fn
      %LineBreak{} = line_break -> %{line_break | classification: classification}
      event -> event
    end)
  end

  defp classify_hard_gap(events) do
    {classified, _seen_separator} =
      Enum.map_reduce(events, false, fn
        %LineBreak{} = line_break, false ->
          {%{line_break | classification: :separator}, true}

        %LineBreak{} = line_break, true ->
          {%{line_break | classification: :blank}, true}

        event, seen_separator ->
          {event, seen_separator}
      end)

    classified
  end

  defp update_frames(frames, %Token{delimiter: :none}), do: {:ok, frames}

  defp update_frames(frames, %Token{delimiter: {:open, family, mode}, span: span})
       when mode in [:continued, :block],
       do: {:ok, [%{family: family, mode: mode, span: span} | frames]}

  defp update_frames([], %Token{delimiter: {:close, family}, span: span}) do
    layout_error(
      "LAY002",
      "a closing delimiter has no matching open delimiter",
      span,
      %{reason: "unexpected_close", observed: inspect(family)}
    )
  end

  defp update_frames([%{family: family} | rest], %Token{delimiter: {:close, family}}),
    do: {:ok, rest}

  defp update_frames([%{family: expected} | _rest], %Token{
         delimiter: {:close, observed},
         span: span
       }) do
    layout_error(
      "LAY002",
      "a closing delimiter does not match the innermost open delimiter",
      span,
      %{reason: "mismatched_close", expected: inspect(expected), observed: inspect(observed)}
    )
  end

  defp update_frames(_frames, %Token{delimiter: delimiter}) do
    raise ArgumentError, "invalid Catena delimiter capability: #{inspect(delimiter)}"
  end

  defp finish([frame | _rest], _previous) do
    layout_error(
      "LAY002",
      "an open delimiter is not closed before the end of input",
      frame.span,
      %{reason: "unclosed_delimiter", expected: inspect(frame.family)}
    )
  end

  defp finish([], %Token{join_after: true} = token) do
    layout_error(
      "LAY003",
      "the end of input interrupts a token that requires a following expression",
      token.span,
      %{reason: "eof_interrupts_continuation"}
    )
  end

  defp finish([], _previous), do: :ok

  defp layout_error(id, message, span, details),
    do: {:error, Diagnostic.new(id, message, span: span, details: details)}

  defp scalar_label(scalar),
    do: "U+" <> (scalar |> Integer.to_string(16) |> String.upcase() |> String.pad_leading(4, "0"))
end
