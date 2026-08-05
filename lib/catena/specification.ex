defmodule Catena.Specification do
  @moduledoc "Catena typed claims, bounded examples, semantic digests, and erasure checks."

  alias Catena.{CanonicalJCS, Diagnostic, LanguageVersion}
  alias Catena.Effect.Row
  alias Catena.Reference.Evaluator
  alias Catena.Type

  @name ~r/^[a-z][A-Za-z0-9_]*$/
  @subject_kinds ~w(value datatype trait instance effect handler module output interface action profile)
  @budget 20_000
  @versions LanguageVersion.from(:specifications_and_governance)

  @spec decode_sections(map(), String.t()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode_sections(value, version) when version in @versions do
    specifications = Map.get(value, "specifications", [])

    with true <- is_list(specifications),
         {:ok, decoded} <- decode_list(specifications, &decode_specification/2),
         true <- unique?(decoded, & &1.name) do
      {:ok, %{specifications: decoded}}
    else
      false -> error("SPC002", "specifications must have unique names", "$.specifications")
      {:error, _} = result -> result
    end
  end

  def decode_sections(value, _version) do
    case Map.fetch(value, "specifications") do
      :error -> {:ok, %{specifications: []}}
      {:ok, []} -> {:ok, %{specifications: []}}
      {:ok, _value} -> error("SPC002", "specifications require AST 0.1.6", "$.specifications")
    end
  end

  @spec elaborate!(map(), map()) :: map()
  def elaborate!(%{frontend_version: version} = ast, core) when version in @versions do
    definitions = Map.new(core.definitions, &{&1.name, &1})

    exported_verification =
      core.definitions
      |> Enum.filter(&Map.get(&1, :verification_only?, false))
      |> Enum.filter(&(&1.name in core.exports))

    if exported_verification != [] do
      fail(
        "SPC003",
        "verification-only definitions cannot be exported as runtime values",
        hd(exported_verification).path
      )
    end

    ensure_erasure_closure!(core)

    claims =
      ast.specifications
      |> Enum.flat_map(fn specification ->
        Enum.map(specification.claims, fn claim ->
          elaborate_claim!(ast, core, definitions, specification, claim)
        end)
      end)

    if length(claims) != length(Enum.uniq_by(claims, & &1.id)) do
      fail("SPC002", "claim identities must be unique", "$.specifications")
    end

    %{claims: claims, digest: CanonicalJCS.digest(Enum.map(claims, &semantic_claim/1))}
  end

  def elaborate!(_ast, _core), do: %{claims: [], digest: nil}

  @spec interface_payload(map()) :: [map()]
  def interface_payload(%{claims: claims}) do
    claims
    |> Enum.map(fn claim ->
      %{
        "id" => claim.id,
        "semantic_digest" => claim.semantic_digest,
        "kind" => claim.kind,
        "subject" => claim.subject,
        "checker_type" => claim.checker_type,
        "examples" => claim.examples
      }
    end)
    |> Enum.sort_by(& &1["id"])
  end

  def interface_payload(_), do: []

  defp decode_specification(value, index) when is_map(value) do
    path = "$.specifications[#{index}]"
    claims = Map.get(value, "claims")

    with {:ok, name} <- valid_name(Map.get(value, "name"), path <> ".name"),
         true <- is_list(claims) and claims != [],
         {:ok, decoded} <- decode_list(claims, &decode_claim(&1, &2, path)),
         true <- unique?(decoded, & &1.name) do
      {:ok, %{name: name, claims: decoded, path: path}}
    else
      false -> error("SPC002", "a specification requires unique claims", path <> ".claims")
      {:error, _} = result -> result
    end
  end

  defp decode_specification(_value, index),
    do: error("SPC002", "specification must be an object", "$.specifications[#{index}]")

  defp decode_claim(value, index, specification_path) when is_map(value) do
    path = "#{specification_path}.claims[#{index}]"
    examples = Map.get(value, "examples", [])

    with {:ok, name} <- valid_name(Map.get(value, "name"), path <> ".name"),
         "rule" <- Map.get(value, "kind"),
         {:ok, subject} <- decode_subject(Map.get(value, "subject"), path <> ".subject"),
         {:ok, checker} <- valid_name(Map.get(value, "checker"), path <> ".checker"),
         true <- is_list(examples),
         {:ok, examples} <- decode_list(examples, &decode_example(&1, &2, path)),
         true <- unique?(examples, & &1.name) do
      {:ok,
       %{
         name: name,
         kind: "rule",
         subject: subject,
         checker: checker,
         examples: examples,
         path: path
       }}
    else
      kind when is_binary(kind) and kind != "rule" ->
        error("SPC002", "module claim kind must be rule in AST 0.1.6", path <> ".kind")

      false ->
        error("SPC004", "examples must have unique names", path <> ".examples")

      {:error, _} = result ->
        result

      _ ->
        error("SPC002", "rule claim requires kind, subject, and checker", path)
    end
  end

  defp decode_claim(_value, index, specification_path),
    do: error("SPC002", "claim must be an object", "#{specification_path}.claims[#{index}]")

  defp decode_subject(%{"kind" => kind, "name" => name}, _path)
       when kind in @subject_kinds and is_binary(name) and byte_size(name) > 0,
       do: {:ok, %{"kind" => kind, "name" => name}}

  defp decode_subject(%{"kind" => kind}, path) when is_binary(kind),
    do: error("SPC001", "unknown or malformed subject kind #{inspect(kind)}", path)

  defp decode_subject(_value, path),
    do: error("SPC001", "subject requires a closed kind and non-empty name", path)

  defp decode_example(value, index, claim_path) when is_map(value) do
    path = "#{claim_path}.examples[#{index}]"
    arguments = Map.get(value, "arguments")
    expected = Map.get(value, "expected")

    with {:ok, name} <- valid_name(Map.get(value, "name"), path <> ".name"),
         true <- is_list(arguments),
         true <- Enum.all?(arguments, &literal?/1),
         true <- is_boolean(expected) do
      {:ok, %{name: name, arguments: arguments, expected: expected, path: path}}
    else
      false ->
        error(
          "SPC004",
          "example requires literal arguments and a Boolean expected result",
          path
        )

      {:error, _} = result ->
        result
    end
  end

  defp decode_example(_value, index, claim_path),
    do: error("SPC004", "example must be an object", "#{claim_path}.examples[#{index}]")

  defp literal?(value) when is_integer(value) or is_boolean(value), do: true
  defp literal?(values) when is_list(values), do: Enum.all?(values, &literal?/1)

  defp literal?(_value), do: false

  defp elaborate_claim!(ast, core, definitions, specification, claim) do
    subject = resolve_subject!(claim.subject, core, claim.path)
    checker = Map.get(definitions, claim.checker)

    if is_nil(checker) or not Map.get(checker, :verification_only?, false) do
      fail(
        "SPC003",
        "rule checker #{claim.checker} must name a verification-only definition",
        claim.path
      )
    end

    if is_nil(find_ast_definition(ast, claim.checker).signature) do
      fail("SPC003", "rule checker #{claim.checker} requires an explicit signature", claim.path)
    end

    {parameter_types, result_type} = split_type(checker.scheme.type, [])

    if result_type != :boolean or length(parameter_types) != length(checker.parameters) do
      fail("SPC003", "rule checker must have its declared parameters and return Bool", claim.path)
    end

    if not Row.equal?(checker.effect_row, Row.empty()) do
      fail("SPC003", "rule checker must infer the empty effect row", claim.path)
    end

    if effect_control?(checker.expression) do
      fail("SPC003", "rule checker may not contain effect-control expressions", claim.path)
    end

    examples =
      Enum.map(claim.examples, fn example ->
        check_example!(core, checker, parameter_types, example)
      end)

    checker_type = checker.scheme.type |> Type.normalize() |> semantic_term()

    base = %{
      id: claim_id(ast, specification.name, claim.name),
      specification: specification.name,
      name: claim.name,
      kind: claim.kind,
      subject: subject,
      checker: claim.checker,
      checker_type: checker_type,
      examples: examples,
      conformance: %{"kind" => "compiler", "result" => "typed_and_pure"}
    }

    digest_input =
      base
      |> Map.put(:checker_body, semantic_term(checker.expression))
      |> Map.delete(:id)

    Map.put(base, :semantic_digest, CanonicalJCS.digest(semantic_term(digest_input)))
  end

  defp check_example!(core, checker, parameter_types, example) do
    if length(parameter_types) != length(example.arguments) or
         not (Enum.zip(parameter_types, example.arguments)
              |> Enum.all?(fn {type, value} -> literal_matches?(Type.normalize(type), value) end)) do
      fail("SPC004", "example arguments do not match the rule checker", example.path)
    end

    arguments = Enum.map(example.arguments, &literal_value/1)

    case Evaluator.run_bounded(core, checker.name, arguments, @budget) do
      {:ok, value, steps} when is_boolean(value) and value == example.expected ->
        %{
          "name" => example.name,
          "arguments" => example.arguments,
          "expected" => example.expected,
          "outcome" => "supported",
          "steps" => steps
        }

      {:ok, value, _steps} when is_boolean(value) ->
        fail(
          "EVD002",
          "example #{example.name} produced #{inspect(value)} instead of #{inspect(example.expected)}",
          example.path
        )

      {:budget_exhausted, steps} ->
        fail(
          "EVD003",
          "example #{example.name} exhausted its 20000-step budget at #{steps}",
          example.path
        )

      {:error, reason, _steps} ->
        fail("EVD002", "example #{example.name} raised #{inspect(reason)}", example.path)

      {:ok, value, _steps} ->
        fail(
          "EVD002",
          "example #{example.name} returned non-Boolean #{inspect(value)}",
          example.path
        )
    end
  end

  defp resolve_subject!(%{"kind" => "value", "name" => name} = subject, core, path) do
    if name in core.exports,
      do: subject,
      else: fail("SPC001", "unknown exported value #{name}", path)
  end

  defp resolve_subject!(%{"kind" => "datatype", "name" => name} = subject, core, path) do
    if Enum.any?(
         core.data.types,
         &(&1.name == name and &1.visibility in [:transparent, :abstract])
       ),
       do: subject,
       else: fail("SPC001", "unknown exported datatype #{name}", path)
  end

  defp resolve_subject!(%{"kind" => "trait", "name" => name} = subject, core, path),
    do: require_named(subject, core.categorical.traits, name, "trait", path)

  defp resolve_subject!(%{"kind" => "instance", "name" => name} = subject, core, path),
    do: require_named(subject, core.categorical.instances, name, "instance", path)

  defp resolve_subject!(%{"kind" => "effect", "name" => name} = subject, core, path),
    do: require_named(subject, Map.values(core.effects.families), name, "effect", path)

  defp resolve_subject!(%{"kind" => "handler", "name" => name} = subject, core, path),
    do: require_named(subject, Map.values(core.effects.handlers), name, "handler", path)

  defp resolve_subject!(%{"kind" => "module", "name" => name} = subject, core, path) do
    if name == core.module, do: subject, else: fail("SPC001", "unknown module #{name}", path)
  end

  defp resolve_subject!(%{"kind" => kind} = subject, _core, _path)
       when kind in ~w(output interface action profile),
       do: subject

  defp require_named(subject, records, name, kind, path) do
    if Enum.any?(records, fn record ->
         Map.get(record, :name) == name or Map.get(record, :id) == name or
           Map.get(record, "name") == name or Map.get(record, "id") == name
       end),
       do: subject,
       else: fail("SPC001", "unknown #{kind} #{name}", path)
  end

  defp ensure_erasure_closure!(core) do
    erased =
      core.definitions
      |> Enum.filter(&Map.get(&1, :verification_only?, false))
      |> MapSet.new(& &1.name)

    Enum.each(core.definitions, fn definition ->
      if not Map.get(definition, :verification_only?, false) do
        escaped = MapSet.intersection(erased, global_references(definition.expression))

        if MapSet.size(escaped) > 0 do
          fail(
            "ERS001",
            "runtime definition #{definition.name} references erased verification value #{escaped |> MapSet.to_list() |> Enum.sort() |> Enum.join(", ")}",
            definition.path
          )
        end
      end
    end)
  end

  defp global_references(expression), do: references(expression, MapSet.new())

  defp effect_control?(%{tag: tag}) when tag in [:request, :handle, :resume], do: true

  defp effect_control?(%{} = expression) do
    expression
    |> Map.drop([:tag, :path, :type, :effects, :latent_effects, :scheme])
    |> Map.values()
    |> Enum.any?(&effect_control?/1)
  end

  defp effect_control?(values) when is_list(values), do: Enum.any?(values, &effect_control?/1)
  defp effect_control?(_value), do: false

  defp references(%{tag: :variable, name: name}, bound),
    do: if(MapSet.member?(bound, name), do: MapSet.new(), else: MapSet.new([name]))

  defp references(%{tag: :function, parameter: name, body: body}, bound),
    do: references(body, MapSet.put(bound, name))

  defp references(%{tag: :let, name: name, value: value, body: body}, bound),
    do: MapSet.union(references(value, bound), references(body, MapSet.put(bound, name)))

  defp references(%{} = expression, bound) do
    expression
    |> Map.drop([:tag, :path, :type, :effects, :latent_effects, :scheme])
    |> Map.values()
    |> Enum.reduce(MapSet.new(), &MapSet.union(&2, references(&1, bound)))
  end

  defp references(values, bound) when is_list(values),
    do: Enum.reduce(values, MapSet.new(), &MapSet.union(&2, references(&1, bound)))

  defp references(_value, _bound), do: MapSet.new()

  defp split_type({:function, parameter, result}, parameters),
    do: split_type(result, parameters ++ [parameter])

  defp split_type(result, parameters), do: {parameters, result}

  defp literal_matches?(:integer, value), do: is_integer(value)
  defp literal_matches?(:boolean, value), do: is_boolean(value)

  defp literal_matches?({:tuple, types}, values) when is_list(values) do
    length(types) == length(values) and
      Enum.zip(types, values) |> Enum.all?(fn {type, value} -> literal_matches?(type, value) end)
  end

  defp literal_matches?(_type, _value), do: false

  defp literal_value(values) when is_list(values),
    do: values |> Enum.map(&literal_value/1) |> List.to_tuple()

  defp literal_value(value), do: value

  defp claim_id(ast, specification, claim) do
    identity = %{
      "origin" => ast.origin,
      "module" => ast.module,
      "specification_name" => specification,
      "claim_name" => claim
    }

    digest =
      :crypto.hash(
        :sha256,
        "catena:claim-id:#{ast.language_revision}\n" <> CanonicalJCS.encode(identity)
      )
      |> Base.encode16(case: :lower)

    "claim:sha256:" <> digest
  end

  defp semantic_claim(claim),
    do: claim |> Map.drop([:id]) |> semantic_term()

  defp semantic_term(%MapSet{} = value),
    do: value |> MapSet.to_list() |> Enum.sort() |> semantic_term()

  defp semantic_term(%_{} = value), do: value |> Map.from_struct() |> semantic_term()

  defp semantic_term(%{} = value) do
    value
    |> Map.drop([:path, "path", :display, "display", :source, "source"])
    |> Map.new(fn {key, item} -> {to_string(key), semantic_term(item)} end)
  end

  defp semantic_term(value) when is_tuple(value),
    do: ["tuple" | value |> Tuple.to_list() |> Enum.map(&semantic_term/1)]

  defp semantic_term(values) when is_list(values), do: Enum.map(values, &semantic_term/1)

  defp semantic_term(value) when is_atom(value) and value not in [true, false, nil],
    do: Atom.to_string(value)

  defp semantic_term(value), do: value

  defp find_ast_definition(ast, name), do: Enum.find(ast.definitions, &(&1.name == name))

  defp valid_name(name, path) when is_binary(name) do
    if Regex.match?(@name, name),
      do: {:ok, name},
      else: error("SPC002", "invalid stable name #{inspect(name)}", path)
  end

  defp valid_name(_name, path), do: error("SPC002", "missing stable name", path)

  defp unique?(values, function) do
    keys = Enum.map(values, function)
    length(keys) == length(Enum.uniq(keys))
  end

  defp decode_list(values, function) do
    values
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {value, index}, {:ok, decoded} ->
      case function.(value, index) do
        {:ok, item} -> {:cont, {:ok, [item | decoded]}}
        {:error, _} = result -> {:halt, result}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      result -> result
    end
  end

  defp error(id, message, path), do: {:error, Diagnostic.new(id, message, path: path)}

  defp fail(id, message, path),
    do: raise(Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path))
end
