defmodule Catena.C019OperatorsTest do
  use ExUnit.Case, async: false

  alias Catena.{LanguageLifecycle, LanguageVersion, Tokenizer}
  alias Catena.Tokenizer.Trivia

  @tag obligations: ~w(OP-OBL-001 OP-OBL-016)
  test "0.1.15 is an exact deterministic source-structure revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.35"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35)

    refute "0.1.15" in LanguageVersion.compilable_revisions()
    refute "0.1.15" in LanguageVersion.interface_versions()
    refute "0.1.15" in LanguageVersion.artifact_versions()
    refute "0.1.15" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("operators-and-punctuation", "0.1.15")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-15-operators-and-punctuation")
      )

    assert change["affects"] == ~w(source-acceptance static-meaning diagnostics)

    assert String.contains?(
             change["specification"],
             "operators-and-punctuation/token-inventory-and-maximal-munch.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "operators-and-punctuation", required: "0.1.15"}
            }} = Catena.tokenize_source("1", language_selection: selection("0.1.14"))

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")
    assert {:ok, %{selection: %{language_revision: "0.1.12"}}} = Catena.scan_comment("// c")

    assert {:ok, %Catena.Numeric.Meaning{type: :Int, value: 1}} =
             Catena.elaborate_numeric_literal(numeric!("1"))

    assert {:ok, %{selection: %{language_revision: "0.1.10"}}} = Catena.parse_identifier("x")

    refute function_exported?(Catena, :parse_source_declarations, 1)
    refute function_exported?(Catena, :check_tokens, 1)
    refute function_exported?(Catena, :resolve_qualified_name, 1)
  end

  @tag obligations: ~w(OP-OBL-002 OP-OBL-004)
  test "the closed inventory is recognized and reserved spellings fail as OPR001" do
    source = "(a + b - c * d < e <= f > g >= h == i != j && k || l |> m -> n , o ; p . q)"

    assert texts!(source) == [
             "(",
             "a",
             "+",
             "b",
             "-",
             "c",
             "*",
             "d",
             "<",
             "e",
             "<=",
             "f",
             ">",
             "g",
             ">=",
             "h",
             "==",
             "i",
             "!=",
             "j",
             "&&",
             "k",
             "||",
             "l",
             "|>",
             "m",
             "->",
             "n",
             ",",
             "o",
             ";",
             "p",
             ".",
             "q",
             ")"
           ]

    assert Enum.all?(tokens!(source), &(&1.kind in [:operator, :punctuation, :name]))

    for spelling <- ["/", "%", "=", "=?", ":=", "^", "&", "~", "?", "×", "→", "@"] do
      assert {:error, %{id: "OPR001", details: %{reason: "reserved_or_invalid_spelling"}}} =
               Catena.tokenize_source("a " <> spelling <> " b"),
             "expected OPR001 for #{inspect(spelling)}"
    end

    for valid_pieces <- ["a << b", "a ++ b", "1 ** 2", "a .. b"] do
      assert {:ok, result} = Catena.tokenize_source(valid_pieces)
      assert {:error, %{id: "OPR002"}} = Catena.parse_operator_expression(result.tokens)
    end

    assert shape!("a -- b") == "(a - -(b))"
    assert shape!("a <- b") == "(a < -(b))"
  end

  @tag obligations: ~w(OP-OBL-003)
  test "maximal munch and spacing invariance hold against every atom" do
    assert ["1.0e3"] = texts!("1.0e3")
    assert ["1", "."] = texts!("1.")
    assert ["x.y.z"] = texts!("x.y.z")
    assert ["a.b", "+", "c.d"] = texts!("a.b + c.d")
    assert ["!=", "a"] |> then(&(&1 == texts!("!=a")))
    assert ["!", "!", "a"] = texts!("!!a")
    assert ["->", "x"] = texts!("->x")
    assert ["|>", "x"] = texts!("|>x")
    assert ["-", "-", "x"] = texts!("--x")

    assert ["1", ".", "x"] = texts!("1.x")

    for variant <- ["a-1", "a - 1", "a -1", "a- 1"] do
      assert Enum.map(tokens!(variant), & &1.text) == ["a", "-", "1"]

      assert Enum.map(tokens!(variant), &{&1.join_before, &1.join_after}) ==
               [{false, false}, {true, true}, {false, false}]
    end

    assert [{_text, %{byte_start: 0, byte_end: 1}}, {_t2, %{byte_start: 2, byte_end: 3}}] =
             Enum.map(tokens!("a +"), &{&1.text, Map.from_struct(&1.span)})

    assert [first, second] = trivias!("a  + b")
    assert first.text == "  "
    assert second.text == " "
    assert [%Trivia{} | _] = trivias!("a\n+ b")
  end

  @tag obligations: ~w(OP-OBL-005)
  test "every token carries its exact capability pair" do
    both = ~w(+ - * < <= > >= == != && || |>)

    for text <- both do
      token = Enum.find(tokens!("a " <> text <> " b"), &(&1.text == text))

      assert {token.join_before, token.join_after} == {true, true},
             "expected both capabilities for #{text}"
    end

    for text <- ~w(! -) do
      token = Enum.find(tokens!("a " <> text <> " b"), &(&1.text == text))
      assert {token.join_before, token.join_after} == {true, true}
    end

    for text <- [")", "]", "}"] do
      token = closing_token!(text)
      assert {token.join_before, token.join_after} == {true, false}
    end

    for {text, source} <- [
          {"(", "( a )"},
          {"[", "[ a ]"},
          {"{", "{ a }"},
          {",", "a , b"},
          {";", "a ; b"},
          {".", "a . b"},
          {"->", "a -> b"}
        ] do
      token = Enum.find(tokens!(source), &(&1.text == text))

      assert {token.join_before, token.join_after} == {false, false},
             "expected no capabilities for #{text}"
    end

    name = one_token!("placeholder more")
    assert {name.join_before, name.join_after} == {false, false}

    literal = one_token!("1 more")
    assert {literal.join_before, literal.join_after} == {false, false}
  end

  @tag obligations: ~w(OP-OBL-006)
  test "frames push the assigned families and modes and close innermost matching" do
    tokens = tokens!("( [ x ] )")
    open = Enum.find(tokens, &(&1.text == "("))
    inner = Enum.find(tokens, &(&1.text == "["))
    close = Enum.find(tokens, &(&1.text == "]"))
    outer_close = Enum.find(tokens, &(&1.text == ")"))

    assert open.frame == {:open, :paren, :continued}
    assert inner.frame == {:open, :bracket, :continued}
    assert close.frame == {:close, :bracket}
    assert outer_close.frame == {:close, :paren}

    brace = Enum.find(tokens!("{ x }"), &(&1.text == "{"))
    assert brace.frame == {:open, :brace, :block}

    assert {:error, %{id: "LAY002", details: %{reason: "unclosed_frame"}}} =
             Catena.tokenize_source("(a + b")

    assert {:error, %{id: "LAY002", details: %{reason: "unmatched_or_mismatched_close"}}} =
             Catena.tokenize_source("( ]")

    assert {:error, %{id: "LAY002", details: %{reason: "unmatched_or_mismatched_close"}}} =
             Catena.tokenize_source(")")

    assert {:ok, result} = Catena.tokenize_source("( a\n+ b ) * c")
    assert texts!(result) == ["(", "a", "+", "b", ")", "*", "c"]
  end

  @tag obligations: ~w(OP-OBL-007)
  test "the fixed ladder resolves grouping and associativity exactly" do
    assert shape!("a - b * c") == "(a - (b * c))"
    assert shape!("a * b + c") == "((a * b) + c)"
    assert shape!("a - b - c") == "((a - b) - c)"
    assert shape!("a * b * c") == "((a * b) * c)"
    assert shape!("(a + b) * c") == "((a + b) * c)"
    assert shape!("a && b || c") == "((a && b) || c)"
    assert shape!("!a && b") == "(!(a) && b)"
    assert shape!("a * -b") == "(a * -(b))"
    assert shape!("\"s\" == \"t\"") == "(s == t)"
    assert shape!("true && false") == "(true && false)"
  end

  @tag obligations: ~w(OP-OBL-008)
  test "comparison and equality chains are rejected, regroupings accepted" do
    for source <- ["a < b < c", "a <= b >= c", "a == b == c", "a != b == c", "a < b == c"] do
      assert {:error, %{id: "OPR002", details: %{reason: "chained_comparison"}}} =
               parse!(source)
    end

    assert shape!("(a < b) == c") == "((a < b) == c)"
    assert shape!("a < (b == c)") == "(a < (b == c))"
    assert shape!("a < b && b < c") == "((a < b) && (b < c))"
  end

  @tag obligations: ~w(OP-OBL-009)
  test "prefix minus and not sit above the ladder and never enter literals" do
    assert shape!("-1") == "-(1)"
    assert shape!("--x") == "-(-(x))"
    assert shape!("!-x") == "!(-(x))"
    assert shape!("-1.5") == "-(1.5)"
    assert shape!("- x") == "-(x)"
    assert {:error, %{id: "LIT001"}} = Catena.scan_literal("-1")

    assert {:error, %{id: "OPR002", details: %{reason: "missing_operand"}}} =
             parse!("a - ")
  end

  @tag obligations: ~w(OP-OBL-010)
  test "the pipe binds loosest, groups left, and applies right to left" do
    assert shape!("x |> f") == "(x |> f)"
    assert shape!("x |> f |> g") == "((x |> f) |> g)"
    assert shape!("a + b |> f") == "((a + b) |> f)"
    assert shape!("a |> f + g") == "(a |> (f + g))"
  end

  @tag obligations: ~w(OP-OBL-011 OP-OBL-012)
  test "the arrow and dot stay outside 0.1.15 expression rules" do
    assert ["a", "->", "b"] = texts!("a -> b")

    assert {:error, %{id: "OPR002", details: %{reason: "reserved_in_operator_expression"}}} =
             parse!("a -> b")

    assert {:error, %{id: "OPR002", details: %{reason: "reserved_in_operator_expression"}}} =
             parse!("1 . x")

    qualified = one_token!("m.field rest")
    assert qualified.kind == :qualified_name
    assert shape!("m.field + n") == "(m.field + n)"
  end

  @tag obligations: ~w(OP-OBL-013 OP-OBL-014)
  test "the stream is lossless and rejection is transactional with one diagnostic" do
    source = "parcel |> map normalize // route\n(x\n  + 1) >= 3.5e2"

    {:ok, result} = Catena.tokenize_source(source)

    assert texts!(result) == [
             "parcel",
             "|>",
             "map",
             "normalize",
             "(",
             "x",
             "+",
             "1",
             ")",
             ">=",
             "3.5e2"
           ]

    token_text = result.tokens |> Enum.reject(&(&1.kind == :comment)) |> Enum.map(& &1.text)
    comment = Enum.find(result.tokens, &(&1.kind == :comment))

    for token <- result.tokens do
      slice =
        binary_part(
          String.reverse(source) |> String.reverse(),
          token.span.byte_start,
          token.span.byte_end - token.span.byte_start
        )

      assert slice == token.text,
             "token #{inspect(token.text)} does not match its original-byte span"
    end

    assert comment.span.byte_start > Enum.at(result.tokens, 3).span.byte_start

    assert {:error, %{id: "OPR001"}} = Catena.tokenize_source("a + b\n% 3")

    assert {:error, %{id: "LAY002", details: %{reason: "unclosed_frame"}}} =
             Catena.tokenize_source("(a")

    assert {:error, %{id: "OPR002"}} = Catena.parse_operator_expression(tokens!("a b"))
    assert {:error, %{id: "OPR002"}} = Catena.parse_operator_expression(tokens!("+"))

    assert {:error, %{id: "OPR002", details: %{reason: "unclosed_group"}}} =
             Catena.parse_operator_expression(Enum.take(tokens!("(a)"), 2))

    assert {:error, %{id: "OPR002", details: %{reason: "unexpected_token"}}} =
             Catena.parse_operator_expression(tokens!("a b"))
  end

  @tag obligations: ~w(OP-OBL-015 OP-OBL-016)
  test "tokenization and parsing are deterministic and stay source-only" do
    source = "// done\na |> f && (b || c) && !d"

    first = Catena.tokenize_source(source)
    second = Catena.tokenize_source(source)
    assert first == second

    {:ok, result} = first
    assert {:ok, tree} = Catena.parse_operator_expression(result.tokens)
    assert Catena.parse_operator_expression(result.tokens) == {:ok, tree}

    assert function_exported?(Catena, :tokenize_source, 2)
    assert function_exported?(Catena, :parse_operator_expression, 1)
    refute function_exported?(Catena, :type_check_tokens, 1)
    refute function_exported?(Catena, :compile_tokens, 1)
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp numeric!(source) do
    assert {:ok, %{literal: %{payload: numeric}}} = Catena.scan_literal(source)
    numeric
  end

  defp tokens!(source) when is_binary(source) do
    assert {:ok, result} = Catena.tokenize_source(source)
    Enum.reject(result.tokens, &(&1.kind == :comment))
  end

  defp tokens!(%{tokens: tokens}), do: Enum.reject(tokens, &(&1.kind == :comment))

  defp texts!(result_or_source),
    do: result_or_source |> tokens!() |> Enum.map(& &1.text)

  defp trivias!(source) do
    assert {:ok, result} = Catena.tokenize_source(source)
    result.trivia
  end

  defp one_token!(source) do
    [token | _] = tokens!(source)
    token
  end

  defp closing_token!(source) do
    opener = %{")" => "(", "]" => "[", "}" => "{"}

    tokens!(opener[source] <> "x" <> source)
    |> Enum.reverse()
    |> hd()
  end

  defp parse!(source) do
    Catena.parse_operator_expression(tokens!(source))
  end

  defp shape!(source) do
    assert {:ok, tree} = parse!(source)
    shape(tree.node)
  end

  defp shape({:atom, %{kind: :literal, text: text}}), do: String.trim(text, "\"")
  defp shape({:atom, token}), do: token.text
  defp shape({:prefix, token, inner}), do: "#{token.text}(#{shape(inner.node)})"

  defp shape({:binary, token, left, right}),
    do: "(#{shape(left.node)} #{token.text} #{shape(right.node)})"

  defp shape({:group, _open, %{node: {:binary, _, _, _} = inner}, _close}), do: shape(inner)
  defp shape({:group, _open, inner, _close}), do: "(#{shape(inner.node)})"
end
