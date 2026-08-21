defmodule Catena.Operator do
  @moduledoc """
  The source-only Catena 0.1.15 operator-expression layer.

  One token-stream region is resolved into an expression tree over atomic
  operands using the fixed precedence ladder, or rejected with exactly one
  stable diagnostic. It does not type-check, resolve names, evaluate, or
  claim declaration grammar.
  """

  alias Catena.{Diagnostic, Tokenizer}

  defmodule Expression do
    @moduledoc "One resolved operator-expression node."

    @enforce_keys [:node]
    defstruct @enforce_keys

    @type expression_node ::
            {:atom, Tokenizer.Token.t()}
            | {:group, Tokenizer.Token.t(), t(), Tokenizer.Token.t()}
            | {:prefix, Tokenizer.Token.t(), t()}
            | {:binary, Tokenizer.Token.t(), t(), t()}

    @type t :: %__MODULE__{node: expression_node()}
  end

  @prefix_operands %{"-" => 40, "!" => 40}
  @binary_levels %{
    "|>" => {1, :left, :pipe},
    "||" => {6, :left, nil},
    "&&" => {7, :left, nil},
    "==" => {10, :none, :comparison},
    "!=" => {10, :none, :comparison},
    "<" => {15, :none, :comparison},
    "<=" => {15, :none, :comparison},
    ">" => {15, :none, :comparison},
    ">=" => {15, :none, :comparison},
    "+" => {20, :left, nil},
    "-" => {20, :left, nil},
    "*" => {30, :left, nil}
  }
  @reserved_in_expressions %{
    "," => "separator",
    ";" => "hard_separator",
    "." => "qualification",
    "->" => "reserved_arrow",
    "[" => "bracket",
    "]" => "bracket",
    "{" => "brace",
    "}" => "brace"
  }

  @spec parse([Tokenizer.Token.t()]) ::
          {:ok, Expression.t()} | {:error, Diagnostic.t()}
  def parse(tokens) when is_list(tokens) do
    tokens = Enum.reject(tokens, &(&1.kind == :comment))

    case parse_expression(tokens, 0) do
      {%Expression{} = expression, []} ->
        {:ok, expression}

      {%Expression{}, [token | _]} ->
        {:error,
         unexpected_token(token, "an operator-expression region ends with an unconsumed token")}

      {:error, %Diagnostic{}} = error ->
        error
    end
  end

  defp parse_expression([], min), do: missing_operand(min)

  defp parse_expression([%Tokenizer.Token{} = token | rest], min) do
    with {:ok, lhs, rest} <- parse_operand(token, rest) do
      continue(lhs, rest, min)
    end
  end

  defp continue(lhs, [], _min), do: {lhs, []}

  defp continue(lhs, [%Tokenizer.Token{} = token | rest] = all, min) do
    if token.frame != nil and elem(token.frame, 0) == :close do
      {lhs, all}
    else
      continue_binary(lhs, token, rest, all, min)
    end
  end

  defp continue_binary(lhs, %Tokenizer.Token{} = token, rest, all, min) do
    case Map.fetch(@binary_levels, token.text) do
      {:ok, {binding, _associativity, _class}} when binding >= min ->
        case parse_expression(rest, binding + 1) do
          {%Expression{} = rhs, rest} ->
            case reject_chain(rest, token) do
              :ok ->
                continue(%Expression{node: {:binary, token, lhs, rhs}}, rest, min)

              {:error, %Diagnostic{}} = error ->
                error
            end

          {:error, %Diagnostic{}} = error ->
            error
        end

      {:ok, _} ->
        {lhs, all}

      :error ->
        case Map.fetch(@reserved_in_expressions, token.text) do
          {:ok, class} ->
            {:error, reserved_token(token, class)}

          :error ->
            {:error,
             unexpected_token(token, "the token does not participate in an operator expression")}
        end
    end
  end

  defp reject_chain([%Tokenizer.Token{} = next | _], %Tokenizer.Token{} = just_parsed) do
    just_class = comparison_class(just_parsed.text)
    next_class = comparison_class(next.text)

    if just_class != nil and next_class != nil do
      {:error, chained_comparison(next)}
    else
      :ok
    end
  end

  defp reject_chain([], _just_parsed), do: :ok

  defp comparison_class(text) do
    case Map.fetch(@binary_levels, text) do
      {:ok, {_, _, :comparison}} -> :comparison
      _ -> nil
    end
  end

  defp parse_operand(%Tokenizer.Token{} = token, rest) do
    case token.kind do
      kind when kind in [:name, :qualified_name, :literal] ->
        {:ok, %Expression{node: {:atom, token}}, rest}

      :operator ->
        case Map.fetch(@prefix_operands, token.text) do
          {:ok, binding} ->
            case parse_expression(rest, binding) do
              {%Expression{} = operand, rest} ->
                {:ok, %Expression{node: {:prefix, token, operand}}, rest}

              {:error, %Diagnostic{}} = error ->
                error
            end

          :error ->
            {:error,
             unexpected_token(token, "a binary operator appears where an operand is required")}
        end

      :punctuation ->
        case token.frame do
          {:open, family, _mode} when family in [:paren] ->
            parse_group(token, rest)

          _ ->
            {:error,
             reserved_token(token, Map.get(@reserved_in_expressions, token.text, "punctuation"))}
        end

      _ ->
        {:error, unexpected_token(token, "the token does not begin an operand")}
    end
  end

  defp parse_group(open, rest) do
    case parse_expression(rest, 0) do
      {%Expression{} = inner, [%Tokenizer.Token{frame: {:close, :paren}} = closer | rest]} ->
        {:ok, %Expression{node: {:group, open, inner, closer}}, rest}

      {%Expression{}, other} ->
        {:error, unclosed_group(other, open)}

      {:error, %Diagnostic{}} = error ->
        error
    end
  end

  defp missing_operand(min) when is_integer(min) do
    {:error,
     Diagnostic.new("OPR002", "an operator-expression form is missing an operand",
       details: %{reason: "missing_operand"}
     )}
  end

  defp unexpected_token(token, message) do
    Diagnostic.new("OPR002", message,
      span: token.span,
      details: %{reason: "unexpected_token", token: token.text}
    )
  end

  defp reserved_token(token, class) do
    Diagnostic.new(
      "OPR002",
      "the token is reserved outside 0.1.15 operator expressions",
      span: token.span,
      details: %{reason: "reserved_in_operator_expression", class: class, token: token.text}
    )
  end

  defp chained_comparison(token) do
    Diagnostic.new(
      "OPR002",
      "comparisons and equalities do not chain; parenthesize the inner result",
      span: token.span,
      details: %{reason: "chained_comparison", token: token.text}
    )
  end

  defp unclosed_group(tokens, open) do
    span =
      case tokens do
        [token | _] -> token.span
        [] -> open.span
      end

    Diagnostic.new("OPR002", "a grouping is not closed before the region ends",
      span: span,
      details: %{reason: "unclosed_group"}
    )
  end
end
