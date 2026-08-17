defmodule Catena.Kernel.SExpression do
  @moduledoc "Strict, source-spanned parser for the canonical 0.1.8 S-expression envelope."

  alias Catena.{Diagnostic, ImplementationLimits, SourceSpan}
  alias Catena.Kernel.Node

  @default_node_limit ImplementationLimits.configured(:kernel_parser_nodes)
  @default_depth_limit ImplementationLimits.configured(:kernel_parser_depth)

  @type token :: {:open | :close, SourceSpan.t()} | {:node, Node.t()}

  @spec parse(binary(), keyword()) :: {:ok, Node.t()} | {:error, Diagnostic.t()}
  def parse(source, options \\ []) when is_binary(source) do
    node_limit = Keyword.get(options, :node_limit, @default_node_limit)
    depth_limit = Keyword.get(options, :depth_limit, @default_depth_limit)

    with :ok <- validate_limits(node_limit, depth_limit),
         :ok <- validate_encoding(source),
         {:ok, tokens} <- tokenize(source, node_limit),
         {:ok, node, []} <- parse_node(tokens, 0, depth_limit) do
      {:ok, node}
    else
      {:ok, _node, [token | _]} ->
        syntax_error("kernel input contains a trailing form", token_span(token))

      {:error, %Diagnostic{} = diagnostic} ->
        {:error, diagnostic}

      {:error, message, span} ->
        syntax_error(message, span)
    end
  end

  defp validate_limits(node_limit, depth_limit)
       when is_integer(node_limit) and node_limit > 0 and is_integer(depth_limit) and
              depth_limit > 0,
       do: :ok

  defp validate_limits(_node_limit, _depth_limit),
    do: syntax_error("parser limits must be positive integers", nil)

  defp validate_encoding(source) do
    cond do
      not String.valid?(source) ->
        syntax_error("kernel input is not valid UTF-8", nil)

      String.starts_with?(source, <<0xEF, 0xBB, 0xBF>>) ->
        syntax_error("kernel input must not begin with a byte-order mark", initial_span())

      true ->
        validate_bytes(source, 0, %{offset: 0, line: 1, column: 1})
    end
  end

  defp validate_bytes(source, index, _position) when index == byte_size(source), do: :ok

  defp validate_bytes(source, index, position) do
    byte = :binary.at(source, index)

    cond do
      byte == ?\r and index + 1 < byte_size(source) and :binary.at(source, index + 1) == ?\n ->
        validate_bytes(source, index + 2, %{
          offset: position.offset + 2,
          line: position.line + 1,
          column: 1
        })

      byte == ?\r ->
        syntax_error("kernel input contains a lone carriage return", one_byte_span(position))

      byte == ?\n ->
        validate_bytes(source, index + 1, %{
          offset: position.offset + 1,
          line: position.line + 1,
          column: 1
        })

      byte in [?\t] or byte in 0x20..0x7E ->
        validate_bytes(source, index + 1, %{
          position
          | offset: position.offset + 1,
            column: position.column + 1
        })

      true ->
        syntax_error("kernel input contains a non-ASCII character", one_byte_span(position))
    end
  end

  defp tokenize(source, node_limit) do
    scan(source, %{index: 0, offset: 0, line: 1, column: 1, nodes: 0}, [], node_limit)
  end

  defp scan(source, state, tokens, _node_limit) when state.index == byte_size(source),
    do: {:ok, Enum.reverse(tokens)}

  defp scan(source, state, tokens, node_limit) do
    byte = :binary.at(source, state.index)

    cond do
      byte in [?\s, ?\t, ?\n, ?\r] ->
        scan(source, advance_whitespace(source, state), tokens, node_limit)

      byte == ?( ->
        {span, state} = punctuation_span(state)

        with {:ok, counted} <- count_node(state, node_limit, span) do
          scan(source, counted, [{:open, span} | tokens], node_limit)
        end

      byte == ?) ->
        {span, state} = punctuation_span(state)
        scan(source, state, [{:close, span} | tokens], node_limit)

      byte == ?\" ->
        with {:ok, node, next_state} <- scan_string(source, state),
             {:ok, counted} <- count_node(next_state, node_limit, node.span) do
          scan(source, counted, [{:node, node} | tokens], node_limit)
        end

      true ->
        with {:ok, node, next_state} <- scan_atom(source, state),
             {:ok, counted} <- count_node(next_state, node_limit, node.span) do
          scan(source, counted, [{:node, node} | tokens], node_limit)
        end
    end
  end

  defp advance_whitespace(source, state) do
    byte = :binary.at(source, state.index)

    cond do
      byte == ?\r ->
        %{
          state
          | index: state.index + 2,
            offset: state.offset + 2,
            line: state.line + 1,
            column: 1
        }

      byte == ?\n ->
        %{
          state
          | index: state.index + 1,
            offset: state.offset + 1,
            line: state.line + 1,
            column: 1
        }

      true ->
        %{state | index: state.index + 1, offset: state.offset + 1, column: state.column + 1}
    end
  end

  defp punctuation_span(state) do
    start = position(state)
    next = %{state | index: state.index + 1, offset: state.offset + 1, column: state.column + 1}
    {SourceSpan.new(start, position(next)), next}
  end

  defp scan_atom(source, state) do
    start = state
    next = take_atom(source, state)

    if next.index == start.index do
      syntax_error("invalid kernel token", one_byte_span(position(state)))
    else
      value = binary_part(source, start.index, next.index - start.index)

      {:ok,
       %Node{kind: :atom, value: value, span: SourceSpan.new(position(start), position(next))},
       next}
    end
  end

  defp take_atom(source, state) when state.index == byte_size(source), do: state

  defp take_atom(source, state) do
    byte = :binary.at(source, state.index)

    if byte in [?\s, ?\t, ?\n, ?\r, ?(, ?), ?\"] do
      state
    else
      take_atom(source, %{
        state
        | index: state.index + 1,
          offset: state.offset + 1,
          column: state.column + 1
      })
    end
  end

  defp scan_string(source, state) do
    start = state
    next = %{state | index: state.index + 1, offset: state.offset + 1, column: state.column + 1}
    take_string(source, start, next, false)
  end

  defp take_string(source, start, state, _escaped) when state.index == byte_size(source),
    do:
      syntax_error(
        "unterminated metadata string",
        SourceSpan.new(position(start), position(state))
      )

  defp take_string(source, start, state, escaped) do
    byte = :binary.at(source, state.index)

    cond do
      byte in [?\n, ?\r] ->
        syntax_error(
          "metadata strings cannot contain literal line breaks",
          one_byte_span(position(state))
        )

      escaped ->
        take_string(source, start, advance_byte(state), false)

      byte == ?\\ ->
        take_string(source, start, advance_byte(state), true)

      byte == ?\" ->
        finish = advance_byte(state)
        raw = binary_part(source, start.index, finish.index - start.index)

        case JSON.decode(raw) do
          {:ok, value} when is_binary(value) ->
            {:ok,
             %Node{
               kind: :string,
               value: value,
               span: SourceSpan.new(position(start), position(finish))
             }, finish}

          _ ->
            syntax_error(
              "metadata string uses an invalid JSON escape",
              SourceSpan.new(position(start), position(finish))
            )
        end

      true ->
        take_string(source, start, advance_byte(state), false)
    end
  end

  defp advance_byte(state),
    do: %{state | index: state.index + 1, offset: state.offset + 1, column: state.column + 1}

  defp count_node(state, limit, span) do
    nodes = state.nodes + 1

    if nodes > limit,
      do:
        limit_error(
          "kernel input exceeds the published parser node limit",
          span,
          :kernel_parser_nodes,
          nodes,
          limit
        ),
      else: {:ok, %{state | nodes: nodes}}
  end

  defp parse_node([], _depth, _limit), do: {:error, "kernel input is empty or incomplete", nil}

  defp parse_node([{:close, span} | _], _depth, _limit),
    do: {:error, "unexpected closing parenthesis", span}

  defp parse_node([{:node, node} | rest], _depth, _limit), do: {:ok, node, rest}

  defp parse_node([{:open, open_span} | rest], depth, limit) do
    if depth + 1 > limit do
      limit_error(
        "kernel input exceeds the published parser nesting limit",
        open_span,
        :kernel_parser_depth,
        depth + 1,
        limit
      )
    else
      parse_list(rest, depth + 1, limit, open_span, [])
    end
  end

  defp parse_list([], _depth, _limit, open_span, _nodes),
    do: {:error, "unclosed parenthesis", open_span}

  defp parse_list([{:close, close_span} | rest], _depth, _limit, open_span, nodes) do
    {:ok,
     %Node{
       kind: :list,
       value: Enum.reverse(nodes),
       span: SourceSpan.merge(open_span, close_span)
     }, rest}
  end

  defp parse_list(tokens, depth, limit, open_span, nodes) do
    with {:ok, node, rest} <- parse_node(tokens, depth, limit) do
      parse_list(rest, depth, limit, open_span, [node | nodes])
    end
  end

  defp position(state), do: %{offset: state.offset, line: state.line, column: state.column}

  defp initial_span do
    position = %{offset: 0, line: 1, column: 1}
    SourceSpan.new(position, position)
  end

  defp one_byte_span(position) do
    SourceSpan.new(position, %{
      position
      | offset: position.offset + 1,
        column: position.column + 1
    })
  end

  defp token_span({:node, node}), do: node.span
  defp token_span({_, span}), do: span

  defp syntax_error(message, span), do: {:error, Diagnostic.new("SYN001", message, span: span)}

  defp limit_error(message, span, limit_id, observed, configured) do
    {:error,
     Diagnostic.new("SYN003", message,
       span: span,
       details: ImplementationLimits.details(limit_id, observed, configured: configured)
     )}
  end
end
