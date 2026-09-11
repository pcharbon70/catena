defmodule Catena.Tool.Formatter do
  @moduledoc "Syntax-independent document algebra and retained-source formatting preview."

  alias Catena.{CanonicalJCS, LanguageVersion, SourceSpan, SourceText, Tokenizer}

  @version "0.1.94"
  @default_width 100
  @maximum_width 240
  @maximum_nodes 65_536
  @maximum_depth 64
  @maximum_input_bytes 16_777_216
  @maximum_output_bytes 16_777_216
  @verbatim_roles ~w(comment literal token)a

  defmodule Document do
    @moduledoc "One validated syntax-independent layout document."
    @enforce_keys [:form]
    defstruct [:form, :value, :children, :indent, :origin, :role, :attachment]
  end

  def profile do
    %{
      version: @version,
      input: :syntax_independent_document,
      public_source_formatting: :held_for_p109,
      width_unit: :unicode_scalar,
      default_width: @default_width,
      maximum_width: @maximum_width,
      maximum_nodes: @maximum_nodes,
      maximum_depth: @maximum_depth,
      maximum_input_bytes: @maximum_input_bytes,
      maximum_output_bytes: @maximum_output_bytes,
      comments: :exact_verbatim_with_attachment_identity,
      literals: :exact_verbatim,
      source_mapping: :exact_original_and_output_byte_spans,
      edit_application: :held_for_p109,
      style_options: []
    }
  end

  def text(value) when is_binary(value), do: %Document{form: :text, value: value}
  def line, do: %Document{form: :line}
  def hardline, do: %Document{form: :hardline}
  def concat(children) when is_list(children), do: %Document{form: :concat, children: children}
  def nest(indent, child), do: %Document{form: :nest, indent: indent, children: [child]}
  def group(child), do: %Document{form: :group, children: [child]}

  def verbatim(source, %Tokenizer.Token{} = token, attachment \\ nil)
      when is_binary(source) do
    role = if token.kind in [:comment, :literal], do: token.kind, else: :token

    %Document{
      form: :verbatim,
      value: token.text,
      origin: token.span,
      role: role,
      attachment: attachment
    }
  end

  def preview(source, document, options \\ [])

  def preview(source, %Document{} = document, options)
      when is_binary(source) and is_list(options) do
    width = Keyword.get(options, :width, @default_width)

    with :ok <- byte_bound(source, @maximum_input_bytes),
         {:ok, _decoded} <-
           SourceText.decode(source,
             language_selection: LanguageVersion.legacy_selection("0.1.9")
           ),
         {:ok, tokenized} <- Tokenizer.tokenize(source),
         :ok <- width_bound(width),
         {:ok, _count} <- validate(document, source, 0, 0),
         {:ok, _output_bound} <- output_size_bound(document, 0, 0),
         token_index = MapSet.new(tokenized.tokens, &token_key/1),
         :ok <- validate_verbatim_tokens(document, token_index),
         {:ok, output, source_map} <- render(document, width),
         :ok <- byte_bound(output, @maximum_output_bytes) do
      body = %{
        "format" => "catena-format-preview",
        "version" => @version,
        "width" => width,
        "width_unit" => "unicode-scalar",
        "preimage" => Base.encode64(source),
        "preimage_digest" => digest(source),
        "result" => Base.encode64(output),
        "result_digest" => digest(output),
        "source_map" => source_map,
        "applicability" => "machine-applicable-after-p109",
        "public_source_formatting" => "held-for-p109"
      }

      {:ok, Map.put(body, "digest", CanonicalJCS.digest(body))}
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_format_document}
    end
  rescue
    _ -> {:error, :invalid_format_document}
  end

  def preview(_, _, _), do: {:error, :invalid_format_document}

  def verify_preview(%{"digest" => digest_value} = preview) do
    with true <-
           Enum.sort(Map.keys(preview)) ==
             ~w(applicability digest format preimage preimage_digest public_source_formatting result result_digest source_map version width width_unit),
         true <- preview["format"] == "catena-format-preview" and preview["version"] == @version,
         true <- preview["width_unit"] == "unicode-scalar",
         true <- preview["applicability"] == "machine-applicable-after-p109",
         true <- preview["public_source_formatting"] == "held-for-p109",
         :ok <- width_bound(preview["width"]),
         true <- digest?(digest_value),
         true <- digest_value == CanonicalJCS.digest(Map.delete(preview, "digest")),
         {:ok, preimage} <- Base.decode64(preview["preimage"]),
         {:ok, result} <- Base.decode64(preview["result"]),
         :ok <- byte_bound(preimage, @maximum_input_bytes),
         :ok <- byte_bound(result, @maximum_output_bytes),
         true <- digest(preimage) == preview["preimage_digest"],
         true <- digest(result) == preview["result_digest"],
         true <- valid_source_map?(preview["source_map"], preimage, result) do
      :ok
    else
      {:error, _} = error -> error
      _ -> {:error, :invalid_format_preview}
    end
  rescue
    _ -> {:error, :invalid_format_preview}
  end

  def verify_preview(_), do: {:error, :invalid_format_preview}

  def apply(preview, path, options \\ [])

  def apply(preview, path, options) when is_binary(path) and is_list(options) do
    with true <- Keyword.get(options, :authorized, false),
         :ok <- verify_preview(preview),
         {:ok, current} <- File.read(path),
         true <- digest(current) == preview["preimage_digest"] do
      {:error, :public_source_formatting_held_for_p109}
    else
      false -> {:error, :format_not_authorized_or_stale}
      {:error, _} = error -> error
      _ -> {:error, :format_not_authorized_or_stale}
    end
  end

  def apply(_, _, _), do: {:error, :invalid_format_application}

  defp validate(%Document{form: :text, value: value}, _source, depth, count)
       when is_binary(value) do
    if String.valid?(value) and not String.contains?(value, ["\n", "\r"]) do
      node_bound(depth, count)
    else
      {:error, :invalid_format_text}
    end
  end

  defp validate(%Document{form: form}, _source, depth, count)
       when form in [:line, :hardline],
       do: node_bound(depth, count)

  defp validate(%Document{form: :concat, children: children}, source, depth, count)
       when is_list(children) and children != [] do
    with {:ok, next} <- node_bound(depth, count) do
      validate_children(children, source, depth + 1, next)
    end
  end

  defp validate(
         %Document{form: form, children: [child]} = document,
         source,
         depth,
         count
       )
       when form in [:nest, :group] do
    with {:ok, next} <- node_bound(depth, count),
         :ok <- validate_wrapper(document),
         do: validate(child, source, depth + 1, next)
  end

  defp validate(
         %Document{form: :verbatim, value: value, origin: %SourceSpan{} = span, role: role},
         source,
         depth,
         count
       )
       when is_binary(value) and role in @verbatim_roles do
    with {:ok, next} <- node_bound(depth, count),
         true <- valid_span?(span, source),
         true <- binary_part(source, span.byte_start, span.byte_end - span.byte_start) == value,
         true <- String.valid?(value),
         do: {:ok, next},
         else: (_ -> {:error, :invalid_verbatim_source})
  end

  defp validate(_, _, _, _), do: {:error, :invalid_format_document}

  defp validate_verbatim_tokens(%Document{form: :verbatim} = document, token_index) do
    attachment_valid =
      is_nil(document.attachment) or
        (document.role == :comment and is_binary(document.attachment) and
           byte_size(document.attachment) <= 256)

    token_valid =
      MapSet.member?(
        token_index,
        {document.value, document.origin, document.role}
      )

    if attachment_valid and token_valid,
      do: :ok,
      else: {:error, :invalid_verbatim_source}
  end

  defp validate_verbatim_tokens(%Document{form: :concat, children: children}, token_index),
    do: validate_verbatim_children(children, token_index)

  defp validate_verbatim_tokens(%Document{form: form, children: [child]}, token_index)
       when form in [:nest, :group],
       do: validate_verbatim_tokens(child, token_index)

  defp validate_verbatim_tokens(%Document{}, _tokens), do: :ok

  defp validate_verbatim_children(children, token_index) do
    Enum.reduce_while(children, :ok, fn child, :ok ->
      case validate_verbatim_tokens(child, token_index) do
        :ok -> {:cont, :ok}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp verbatim_role(kind) when kind in [:comment, :literal], do: kind
  defp verbatim_role(_), do: :token

  defp token_key(token), do: {token.text, token.span, verbatim_role(token.kind)}

  defp validate_children([], _source, _depth, count), do: {:ok, count}

  defp validate_children([child | rest], source, depth, count) do
    with {:ok, next} <- validate(child, source, depth, count),
         do: validate_children(rest, source, depth, next)
  end

  defp validate_wrapper(%Document{form: :group}), do: :ok

  defp validate_wrapper(%Document{form: :nest, indent: indent})
       when is_integer(indent) and indent >= 0 and indent <= @maximum_width,
       do: :ok

  defp validate_wrapper(_), do: {:error, :invalid_format_document}

  defp node_bound(depth, count) do
    cond do
      depth > @maximum_depth -> {:error, :format_limit_exceeded}
      count + 1 > @maximum_nodes -> {:error, :format_limit_exceeded}
      true -> {:ok, count + 1}
    end
  end

  defp output_size_bound(%Document{form: form, value: value}, _indent, size)
       when form in [:text, :verbatim],
       do: add_output_size(size, byte_size(value))

  defp output_size_bound(%Document{form: form}, indent, size)
       when form in [:line, :hardline],
       do: add_output_size(size, 1 + indent)

  defp output_size_bound(%Document{form: :concat, children: children}, indent, size) do
    Enum.reduce_while(children, {:ok, size}, fn child, {:ok, current} ->
      case output_size_bound(child, indent, current) do
        {:ok, next} -> {:cont, {:ok, next}}
        {:error, _} = error -> {:halt, error}
      end
    end)
  end

  defp output_size_bound(%Document{form: :nest, indent: amount, children: [child]}, indent, size),
    do: output_size_bound(child, indent + amount, size)

  defp output_size_bound(%Document{form: :group, children: [child]}, indent, size),
    do: output_size_bound(child, indent, size)

  defp add_output_size(size, addition) do
    if size + addition <= @maximum_output_bytes,
      do: {:ok, size + addition},
      else: {:error, :format_limit_exceeded}
  end

  defp render(document, width) do
    state = %{chunks: [], mappings: [], offset: 0, column: 0}
    rendered = layout(document, :break, 0, width, state)

    {:ok, rendered.chunks |> Enum.reverse() |> IO.iodata_to_binary(),
     Enum.reverse(rendered.mappings)}
  rescue
    _ -> {:error, :invalid_format_document}
  end

  defp layout(%Document{form: :text, value: value}, _mode, _indent, _width, state),
    do: append(state, value, nil)

  defp layout(%Document{form: :verbatim} = document, _mode, _indent, _width, state),
    do: append(state, document.value, document)

  defp layout(%Document{form: :line}, :flat, _indent, _width, state), do: append(state, " ", nil)

  defp layout(%Document{form: :line}, :break, indent, _width, state),
    do: append(state, "\n" <> String.duplicate(" ", indent), nil)

  defp layout(%Document{form: :hardline}, _mode, indent, _width, state),
    do: append(state, "\n" <> String.duplicate(" ", indent), nil)

  defp layout(%Document{form: :concat, children: children}, mode, indent, width, state),
    do: Enum.reduce(children, state, &layout(&1, mode, indent, width, &2))

  defp layout(
         %Document{form: :nest, indent: amount, children: [child]},
         mode,
         indent,
         width,
         state
       ),
       do: layout(child, mode, indent + amount, width, state)

  defp layout(%Document{form: :group, children: [child]}, _mode, indent, width, state) do
    mode = if flat_width(child) <= width - state.column, do: :flat, else: :break
    layout(child, mode, indent, width, state)
  end

  defp flat_width(%Document{form: :text, value: value}), do: scalar_tail_width(value)
  defp flat_width(%Document{form: :verbatim, value: value}), do: scalar_tail_width(value)
  defp flat_width(%Document{form: :line}), do: 1
  defp flat_width(%Document{form: :hardline}), do: @maximum_output_bytes

  defp flat_width(%Document{form: :concat, children: children}),
    do: Enum.sum(Enum.map(children, &flat_width/1))

  defp flat_width(%Document{form: form, children: [child]}) when form in [:nest, :group],
    do: flat_width(child)

  defp append(state, value, origin) do
    finish = state.offset + byte_size(value)

    mapping =
      if origin do
        %{
          "role" => Atom.to_string(origin.role),
          "attachment" => origin.attachment,
          "source" => %{
            "byte_start" => origin.origin.byte_start,
            "byte_end" => origin.origin.byte_end
          },
          "output" => %{"byte_start" => state.offset, "byte_end" => finish}
        }
      end

    %{
      chunks: [value | state.chunks],
      mappings: if(mapping, do: [mapping | state.mappings], else: state.mappings),
      offset: finish,
      column: next_column(state.column, value)
    }
  end

  defp next_column(column, value) do
    case String.split(value, "\n") do
      [only] -> column + String.length(only)
      parts -> parts |> List.last() |> String.length()
    end
  end

  defp scalar_tail_width(value) do
    if String.contains?(value, "\n"), do: @maximum_output_bytes, else: String.length(value)
  end

  defp valid_source_map?(items, source, output) when is_list(items) do
    Enum.all?(items, fn item ->
      with true <- Enum.sort(Map.keys(item)) == ~w(attachment output role source),
           true <- item["role"] in Enum.map(@verbatim_roles, &Atom.to_string/1),
           true <-
             is_nil(item["attachment"]) or
               (item["role"] == "comment" and is_binary(item["attachment"]) and
                  byte_size(item["attachment"]) <= 256),
           %{"byte_start" => sa, "byte_end" => sb} <- item["source"],
           %{"byte_start" => oa, "byte_end" => ob} <- item["output"],
           true <- valid_range?(sa, sb, source),
           true <- valid_range?(oa, ob, output),
           true <- binary_part(source, sa, sb - sa) == binary_part(output, oa, ob - oa) do
        true
      else
        _ -> false
      end
    end)
  end

  defp valid_source_map?(_, _, _), do: false

  defp valid_span?(span, source), do: valid_range?(span.byte_start, span.byte_end, source)

  defp valid_range?(first, last, bytes),
    do:
      is_integer(first) and is_integer(last) and first >= 0 and last >= first and
        last <= byte_size(bytes)

  defp width_bound(width) when is_integer(width) and width >= 1 and width <= @maximum_width,
    do: :ok

  defp width_bound(_), do: {:error, :format_limit_exceeded}

  defp byte_bound(bytes, limit) do
    if byte_size(bytes) <= limit,
      do: :ok,
      else: {:error, :format_limit_exceeded}
  end

  defp digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
  defp digest?(value), do: is_binary(value) and Regex.match?(~r/\A[0-9a-f]{64}\z/, value)
end
