defmodule Catena.FormatterToolTest do
  use ExUnit.Case, async: false

  alias Catena.Tool.Formatter

  @moduletag :tmp_dir

  test "groups choose deterministic scalar-width layouts and nesting" do
    document =
      Formatter.group(
        Formatter.concat([
          Formatter.text("alpha"),
          Formatter.nest(2, Formatter.concat([Formatter.line(), Formatter.text("βeta")]))
        ])
      )

    assert {:ok, wide} = Formatter.preview("", document, width: 10)
    assert Base.decode64!(wide["result"]) == "alpha βeta"

    assert {:ok, narrow} = Formatter.preview("", document, width: 9)
    assert Base.decode64!(narrow["result"]) == "alpha\n  βeta"
    assert {:ok, ^narrow} = Formatter.preview("", document, width: 9)
    assert :ok = Formatter.verify_preview(narrow)
  end

  test "tokenizer-origin comments, raw literals, Unicode, and attachments remain exact" do
    source = "α /* block\ncomment */ r\"raw\" // tail"
    assert {:ok, tokenized} = Catena.Tokenizer.tokenize(source)
    [name, block, literal, tail] = tokenized.tokens

    document =
      Formatter.concat([
        Formatter.verbatim(source, name),
        Formatter.text(" "),
        Formatter.verbatim(source, block, "declaration:alpha"),
        Formatter.hardline(),
        Formatter.verbatim(source, literal),
        Formatter.text(" "),
        Formatter.verbatim(source, tail, "trailing:alpha")
      ])

    assert {:ok, preview} = Formatter.preview(source, document, width: 240)
    assert Base.decode64!(preview["result"]) == "α /* block\ncomment */\nr\"raw\" // tail"

    assert Enum.map(preview["source_map"], & &1["role"]) ==
             ~w(token comment literal comment)

    assert Enum.map(preview["source_map"], & &1["attachment"]) ==
             [nil, "declaration:alpha", nil, "trailing:alpha"]

    Enum.each(preview["source_map"], fn mapping ->
      source_range = mapping["source"]
      output_range = mapping["output"]

      assert binary_part(
               source,
               source_range["byte_start"],
               source_range["byte_end"] - source_range["byte_start"]
             ) ==
               binary_part(
                 Base.decode64!(preview["result"]),
                 output_range["byte_start"],
                 output_range["byte_end"] - output_range["byte_start"]
               )
    end)
  end

  test "forged origins and altered previews are rejected" do
    source = "name"
    assert {:ok, tokenized} = Catena.Tokenizer.tokenize(source)
    [token] = tokenized.tokens
    document = Formatter.verbatim(source, token)
    forged = %{document | value: "other"}
    assert {:error, :invalid_verbatim_source} = Formatter.preview(source, forged)

    mislabeled = Formatter.verbatim(source, %{token | kind: :comment}, "forged")
    assert {:error, :invalid_verbatim_source} = Formatter.preview(source, mislabeled)

    assert {:ok, preview} = Formatter.preview(source, document)
    tampered = Map.put(preview, "result_digest", String.duplicate("0", 64))
    assert {:error, :invalid_format_preview} = Formatter.verify_preview(tampered)
  end

  test "width and structural limits fail explicitly" do
    assert {:error, :format_limit_exceeded} =
             Formatter.preview("", Formatter.text("ok"), width: 241)

    too_deep = Enum.reduce(1..66, Formatter.text("x"), &Formatter.nest(&1, &2))
    assert {:error, :format_limit_exceeded} = Formatter.preview("", too_deep)
  end

  test "public-source application remains held and never overwrites an input", %{tmp_dir: root} do
    source = "name"
    path = Path.join(root, "sample.cat")
    File.write!(path, source)
    document = Formatter.group(Formatter.concat([Formatter.text("changed"), Formatter.line()]))
    assert {:ok, preview} = Formatter.preview(source, document)

    assert {:error, :format_not_authorized_or_stale} = Formatter.apply(preview, path)
    assert File.read!(path) == source

    assert {:error, :public_source_formatting_held_for_p109} =
             Formatter.apply(preview, path, authorized: true)

    assert File.read!(path) == source
    File.write!(path, "stale")

    assert {:error, :format_not_authorized_or_stale} =
             Formatter.apply(preview, path, authorized: true)

    assert File.read!(path) == "stale"
  end

  test "the preparatory formatter slice is versioned and reports the P109 gate" do
    assert Catena.LanguageVersion.introduced(:formatter_tool) == "0.1.94"
    assert {:ok, :stable} = Catena.LanguageLifecycle.state("formatter-tool", "0.1.94")
    profile = Catena.ConformanceInfo.document()["formatter_tool"]
    assert profile["public_source_formatting"] == "held_for_p109"
    assert profile["style_options"] == []
    assert profile["width_unit"] == "unicode_scalar"
  end
end
