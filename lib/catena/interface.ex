defmodule Catena.Interface do
  @moduledoc "Deterministic, layout-free, selection-aware Catena module interfaces."

  alias Catena.Categorical.TypeTerm
  alias Catena.Effect.Row

  alias Catena.{
    CanonicalJSON,
    Condition,
    Diagnostic,
    Kind,
    LanguageVersion,
    Specification
  }

  alias Catena.Type.Scheme

  @versions LanguageVersion.interface_versions()
  @categorical_versions LanguageVersion.from(:traits_and_categories)
  @effect_versions LanguageVersion.from(:effects_and_handlers)
  @pre_categorical_versions LanguageVersion.before(:traits_and_categories)
  @pre_effect_versions LanguageVersion.before(:effects_and_handlers)
  @pre_specification_versions LanguageVersion.before(:specifications_and_governance)
  @specification_versions LanguageVersion.from(:specifications_and_governance)
  @edition_version LanguageVersion.introduced(:editions_and_feature_lifecycle)
  @claim_subject_kinds ~w(value datatype trait instance effect handler module output interface action profile)

  @spec build(map(), keyword()) :: map()
  def build(core, options \\ []) do
    values =
      core.definitions
      |> Enum.filter(&(&1.name in core.exports))
      |> Enum.map(fn definition ->
        value = %{"name" => definition.name, "scheme" => encode_scheme(definition.scheme)}

        value =
          case get_in(definition, [:uses, :row]) do
            %Row{} = row -> Map.put(value, "uses", encode_effect_row(row))
            _ -> value
          end

        case Map.get(definition, :condition) do
          nil -> value
          evidence -> Map.put(value, "condition", Condition.encode_evidence(evidence))
        end
      end)
      |> Enum.sort_by(& &1["name"])

    types =
      core.data.types
      |> Enum.filter(&(&1.visibility in [:transparent, :abstract]))
      |> Enum.map(&encode_datatype/1)
      |> Enum.sort_by(& &1["id"])

    artifact_version =
      Keyword.get(
        options,
        :artifact_version,
        LanguageVersion.default_artifact_version(
          Map.get(core, :frontend_format, core.frontend_version),
          Map.get(core, :language_revision, core.frontend_version)
        )
      )

    payload =
      %{
        "format" => "catena-interface",
        "version" => artifact_version,
        "origin" => core.origin,
        "module" => core.module,
        "types" => types,
        "values" => values
      }
      |> categorical_payload(core)
      |> effect_payload(core)
      |> specification_payload(core)
      |> selection_payload(core, artifact_version)

    Map.put(payload, "digest", digest(payload))
  end

  @spec encode(map()) :: binary()
  def encode(interface), do: CanonicalJSON.encode(interface) <> "\n"

  @spec decode(binary()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def decode(binary) when is_binary(binary) do
    with {:ok, value} <- JSON.decode(binary), true <- is_map(value) do
      if Map.get(value, "version") == LanguageVersion.introduced(:formal_semantic_kernel) do
        Catena.Kernel.Interface.decode_value(value)
      else
        decode_legacy_value(value)
      end
    else
      _ -> error("malformed or unsupported Catena interface")
    end
  rescue
    _error -> error("malformed or unsupported Catena interface")
  end

  defp decode_legacy_value(value) do
    with version when version in @versions <- Map.get(value, "version"),
         "catena-interface" <- Map.get(value, "format"),
         {:ok, selection, required_previews} <- decode_selection(value, version),
         digest when is_binary(digest) <- Map.get(value, "digest"),
         payload = Map.delete(value, "digest"),
         true <- secure_equal?(digest, digest(payload)),
         {:ok, types} <- decode_types(Map.get(value, "types"), value),
         {:ok, values} <- decode_values(Map.get(value, "values"), types),
         {:ok, categorical} <- decode_categorical(value, selection.language_revision),
         {:ok, effects} <- decode_effects(value, selection.language_revision),
         {:ok, specifications} <- decode_specifications(value, selection.language_revision) do
      {:ok,
       Map.merge(
         %{
           version: version,
           edition: selection.edition,
           language_revision: selection.language_revision,
           previews: selection.previews,
           required_previews: required_previews,
           origin: Map.fetch!(value, "origin"),
           module: Map.fetch!(value, "module"),
           digest: digest,
           types: types,
           values: values
         },
         categorical |> Map.merge(effects) |> Map.merge(specifications)
       )}
    else
      false -> error("interface digest does not match its contents")
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      _ -> error("malformed or unsupported Catena interface")
    end
  end

  defp encode_datatype(type) do
    base = %{
      "id" => type.id,
      "origin" => type.origin,
      "module" => type.module,
      "name" => type.name,
      "arity" => type.arity,
      "kind" => List.duplicate("Type", type.arity) ++ ["Type"],
      "visibility" => Atom.to_string(type.visibility),
      "inhabitation" => Atom.to_string(type.inhabitation),
      "variance" => Enum.map(type.variance, &Atom.to_string/1),
      "positive" => type.positive?,
      "regular" => type.regular?
    }

    if type.visibility == :transparent do
      Map.put(base, "constructors", Enum.map(type.constructors, &encode_constructor/1))
    else
      base
    end
  end

  defp categorical_payload(payload, %{frontend_version: version, categorical: categorical})
       when version in @categorical_versions do
    Map.merge(payload, %{
      "standard_digest" => categorical.standard_digest,
      "traits" => Enum.map(categorical.traits, &encode_trait/1),
      "instances" => Enum.map(categorical.instances, &encode_instance/1),
      "templates" => categorical.templates
    })
  end

  defp categorical_payload(payload, _core), do: payload

  defp effect_payload(payload, %{frontend_version: version, effects: effects})
       when version in @effect_versions do
    Map.merge(payload, %{
      "effects" =>
        effects.exported_families
        |> Enum.map(&encode_effect_family/1)
        |> Enum.sort_by(& &1["id"]),
      "handlers" =>
        effects.exported_handlers
        |> Enum.map(&encode_handler/1)
        |> Enum.sort_by(& &1["id"])
    })
  end

  defp effect_payload(payload, _core), do: payload

  defp specification_payload(payload, %{
         frontend_version: version,
         specifications: specifications
       })
       when version in @specification_versions do
    Map.merge(payload, %{
      "claims" => Specification.interface_payload(specifications),
      "specification_digest" => specifications.digest
    })
  end

  defp specification_payload(payload, _core), do: payload

  defp selection_payload(payload, core, @edition_version) do
    Map.merge(payload, %{
      "edition" => core.edition,
      "language_revision" => core.language_revision,
      "previews" => core.previews,
      "required_previews" => core.required_previews
    })
  end

  defp selection_payload(payload, _core, _version), do: payload

  defp encode_effect_family(family) do
    %{
      "id" => family.id,
      "origin" => family.origin,
      "module" => family.module,
      "name" => family.name,
      "parameters" => family.parameters,
      "operations" =>
        family.operations
        |> Map.values()
        |> Enum.sort_by(& &1.name)
        |> Enum.map(fn operation ->
          %{
            "name" => operation.name,
            "parameters" => Enum.map(operation.parameters, &encode_type/1),
            "parameter_names" => operation.parameter_names,
            "result" => encode_type(operation.result)
          }
        end)
    }
  end

  defp encode_handler(handler) do
    %{
      "id" => handler.id,
      "origin" => handler.origin,
      "module" => handler.module,
      "name" => handler.name,
      "family" => handler.family,
      "family_name" => handler.family_name,
      "arguments" => Enum.map(handler.arguments, &encode_type/1),
      "variables" => handler.variables,
      "input" => encode_type(handler.input),
      "output" => encode_type(handler.output),
      "parameters" =>
        Enum.map(handler.parameters, fn parameter ->
          %{"name" => parameter.name, "type" => encode_type(parameter.parsed_type)}
        end),
      "uses" => encode_effect_row(Map.get(handler, :uses_row, Row.empty()))
    }
  end

  defp encode_effect_row(%Row{} = row) do
    %{
      "entries" =>
        Enum.map(row.entries, fn entry ->
          %{
            "family" => entry.family,
            "family_name" => entry.family_name,
            "arguments" => Enum.map(entry.arguments, &encode_type/1),
            "capability" => entry.capability,
            "name" => Map.get(entry, :name),
            "abstract" => Map.get(entry, :abstract?, false)
          }
        end),
      "tail" => row.tail
    }
  end

  defp encode_trait(trait) do
    %{
      "id" => trait.id,
      "name" => trait.name,
      "formal_name" => trait.formal_name,
      "parameters" =>
        Enum.map(trait.parameters, &%{"name" => &1.name, "kind" => Kind.encode(&1.kind)}),
      "parents" =>
        Enum.map(trait.parents, fn parent ->
          %{
            "trait" => parent.trait,
            "arguments" => Enum.map(parent.arguments, &TypeTerm.encode/1)
          }
        end),
      "methods" => Enum.map(trait.methods, &stringify_record/1),
      "laws" => Enum.map(trait.laws, &stringify_record/1),
      "fundeps" =>
        Enum.map(trait.fundeps, fn {inputs, outputs} ->
          %{"inputs" => inputs, "outputs" => outputs}
        end)
    }
  end

  defp encode_instance(instance) do
    %{
      "id" => instance.id,
      "trait" => instance.trait,
      "arguments" => Enum.map(instance.arguments, &TypeTerm.encode/1),
      "owner" => instance.owner,
      "context" =>
        Enum.map(instance.context, fn predicate ->
          %{
            "trait" => predicate.trait,
            "arguments" => Enum.map(predicate.arguments, &TypeTerm.encode/1)
          }
        end),
      "methods" => instance.methods,
      "associated_types" =>
        Map.new(instance.associated_types, fn {name, type} -> {name, TypeTerm.encode(type)} end),
      "law_status" => Atom.to_string(instance.law_status),
      "derivation" => instance.derivation
    }
  end

  defp stringify_record(record),
    do: Map.new(record, fn {key, value} -> {Atom.to_string(key), value} end)

  defp encode_constructor(constructor) do
    %{
      "id" => constructor.id,
      "name" => constructor.name,
      "index" => constructor.index,
      "field_style" => Atom.to_string(constructor.field_style),
      "fields" =>
        Enum.map(constructor.fields, fn field ->
          %{"name" => field.name, "index" => field.index, "type" => encode_type(field.type)}
        end),
      "variables" => constructor.variables,
      "universal_count" => constructor.universal_count,
      "existential_ids" => constructor.existential_ids |> MapSet.to_list() |> Enum.sort(),
      "result" => encode_type(constructor.result),
      "gadt" => constructor.gadt?
    }
  end

  defp encode_scheme(%Scheme{variables: variables, type: type}),
    do: %{"variables" => variables, "type" => encode_type(type)}

  defp encode_type(:integer), do: %{"tag" => "integer"}
  defp encode_type(:boolean), do: %{"tag" => "boolean"}
  defp encode_type({:var, id}), do: %{"tag" => "variable", "id" => id}
  defp encode_type({:skolem, id}), do: %{"tag" => "skolem", "id" => inspect(id)}

  defp encode_type({:function, parameter, result}),
    do: %{
      "tag" => "function",
      "parameter" => encode_type(parameter),
      "result" => encode_type(result)
    }

  defp encode_type({:tuple, elements}),
    do: %{"tag" => "tuple", "elements" => Enum.map(elements, &encode_type/1)}

  defp encode_type({:nominal, id, arguments}),
    do: %{"tag" => "nominal", "id" => id, "arguments" => Enum.map(arguments, &encode_type/1)}

  defp decode_types(types, interface) when is_list(types) do
    decoded =
      Enum.map(types, fn type ->
        visibility = decode_visibility(Map.get(type, "visibility"))

        base = %{
          id: Map.fetch!(type, "id"),
          origin: Map.fetch!(type, "origin"),
          module: Map.fetch!(type, "module"),
          name: Map.fetch!(type, "name"),
          arity: Map.fetch!(type, "arity"),
          visibility: visibility,
          inhabitation: decode_inhabitation(Map.get(type, "inhabitation")),
          variance: Enum.map(Map.get(type, "variance", []), &decode_variance/1),
          positive?: Map.get(type, "positive", false),
          regular?: Map.get(type, "regular", false),
          derivations: [],
          imported?: true,
          path: "interface://#{interface["module"]}/#{type["name"]}"
        }

        constructors =
          Enum.map(Map.get(type, "constructors", []), fn constructor ->
            %{
              id: Map.fetch!(constructor, "id"),
              type_id: Map.fetch!(type, "id"),
              type_name: Map.fetch!(type, "name"),
              name: Map.fetch!(constructor, "name"),
              qualified: "#{type["module"]}.#{type["name"]}.#{constructor["name"]}",
              index: Map.fetch!(constructor, "index"),
              fields:
                Enum.map(Map.fetch!(constructor, "fields"), fn field ->
                  %{
                    name: Map.get(field, "name"),
                    index: Map.fetch!(field, "index"),
                    type: decode_type(Map.fetch!(field, "type"))
                  }
                end),
              field_style: String.to_existing_atom(Map.fetch!(constructor, "field_style")),
              variables: Map.fetch!(constructor, "variables"),
              universal_count: Map.fetch!(constructor, "universal_count"),
              existential_ids: MapSet.new(Map.fetch!(constructor, "existential_ids")),
              result: decode_type(Map.fetch!(constructor, "result")),
              gadt?: Map.fetch!(constructor, "gadt"),
              visibility: visibility,
              path: "interface://#{interface["module"]}/#{constructor["name"]}"
            }
          end)

        Map.put(base, :constructors, constructors)
      end)

    {:ok, decoded}
  end

  defp decode_types(_, _interface), do: error("interface types must be a list")

  defp decode_values(values, _types) when is_list(values) do
    values
    |> Enum.with_index()
    |> Enum.reduce_while({:ok, []}, fn {value, index}, {:ok, decoded} ->
      base = %{
        name: Map.fetch!(value, "name"),
        scheme: decode_scheme(Map.fetch!(value, "scheme")),
        uses: decode_effect_row(Map.get(value, "uses", %{"entries" => [], "tail" => nil}))
      }

      case Map.get(value, "condition") do
        nil ->
          {:cont, {:ok, [Map.put(base, :condition, nil) | decoded]}}

        encoded ->
          case Condition.decode_evidence(encoded, "$.values[#{index}].condition") do
            {:ok, evidence} ->
              if Condition.valid_for_scheme?(evidence, base.scheme) do
                {:cont, {:ok, [Map.put(base, :condition, evidence) | decoded]}}
              else
                {:halt,
                 {:error,
                  Diagnostic.new(
                    "CND005",
                    "condition evidence does not match its exported type scheme",
                    path: "$.values[#{index}].condition"
                  )}}
              end

            {:error, _} = error ->
              {:halt, error}
          end
      end
    end)
    |> case do
      {:ok, decoded} -> {:ok, Enum.reverse(decoded)}
      error -> error
    end
  end

  defp decode_values(_, _types), do: error("interface values must be a list")

  defp decode_categorical(_value, version) when version in @pre_categorical_versions,
    do: {:ok, %{traits: [], instances: [], templates: [], standard_digest: nil}}

  defp decode_categorical(value, version) when version in @categorical_versions do
    traits = Map.get(value, "traits")
    instances = Map.get(value, "instances")
    templates = Map.get(value, "templates")
    standard_digest = Map.get(value, "standard_digest")

    cond do
      not (is_list(traits) and Enum.all?(traits, &is_map/1)) ->
        error("interface traits must be a list of objects")

      not (is_list(instances) and Enum.all?(instances, &is_map/1)) ->
        error("interface instances must be a list of objects")

      not valid_templates?(templates) ->
        error("interface templates must contain a verified helper closure")

      not is_binary(standard_digest) ->
        error("interface requires a standard hierarchy digest")

      true ->
        {:ok,
         %{
           traits: traits,
           instances: instances,
           templates: templates,
           standard_digest: standard_digest
         }}
    end
  end

  defp decode_effects(_value, version) when version in @pre_effect_versions,
    do: {:ok, %{effects: [], handlers: []}}

  defp decode_effects(value, version) when version in @effect_versions do
    effects = Map.get(value, "effects")
    handlers = Map.get(value, "handlers")

    if is_list(effects) and Enum.all?(effects, &is_map/1) and is_list(handlers) and
         Enum.all?(handlers, &is_map/1) do
      decoded_effects = Enum.map(effects, &decode_effect_family/1)
      decoded_handlers = Enum.map(handlers, &decode_handler/1)

      if unique_identity_records?(decoded_effects) and unique_identity_records?(decoded_handlers) do
        {:ok, %{effects: decoded_effects, handlers: decoded_handlers}}
      else
        error("interface effect and handler identities must be unique")
      end
    else
      error("interface effects and handlers must be lists of objects")
    end
  end

  defp decode_specifications(_value, version) when version in @pre_specification_versions,
    do: {:ok, %{claims: [], specification_digest: nil}}

  defp decode_specifications(value, version) when version in @specification_versions do
    claims = Map.get(value, "claims")
    digest = Map.get(value, "specification_digest")

    if is_list(claims) and Enum.all?(claims, &valid_claim_summary?/1) and digest?(digest) and
         length(claims) == length(Enum.uniq_by(claims, & &1["id"])) do
      {:ok, %{claims: claims, specification_digest: digest}}
    else
      error("interface claims must be unique well-formed summaries")
    end
  end

  defp valid_claim_summary?(claim) when is_map(claim) do
    subject = Map.get(claim, "subject")
    examples = Map.get(claim, "examples")

    is_binary(Map.get(claim, "id")) and
      Regex.match?(~r/^claim:sha256:[0-9a-f]{64}$/, claim["id"]) and
      digest?(Map.get(claim, "semantic_digest")) and Map.get(claim, "kind") == "rule" and
      is_map(subject) and subject["kind"] in @claim_subject_kinds and is_binary(subject["name"]) and
      byte_size(subject["name"]) > 0 and is_list(examples) and
      Enum.all?(examples, &valid_claim_example?/1)
  end

  defp valid_claim_summary?(_claim), do: false

  defp decode_selection(value, @edition_version) do
    required = Map.get(value, "required_previews")

    with {:ok, selection} <- LanguageVersion.resolve_selection(value),
         true <- selection.language_revision in LanguageVersion.compilable_revisions(),
         true <- sorted_string_list?(required),
         true <- MapSet.subset?(MapSet.new(required), MapSet.new(selection.previews)) do
      {:ok, selection, required}
    else
      {:error, %Diagnostic{} = diagnostic} -> {:error, diagnostic}
      _ -> error("interface requires sorted preview selection and public requirements")
    end
  end

  defp decode_selection(_value, version) do
    selection = LanguageVersion.legacy_selection(version)
    {:ok, selection, []}
  end

  defp sorted_string_list?(values),
    do:
      is_list(values) and Enum.all?(values, &is_binary/1) and
        values == Enum.sort(Enum.uniq(values))

  defp valid_claim_example?(%{
         "name" => name,
         "arguments" => arguments,
         "expected" => expected,
         "outcome" => "supported",
         "steps" => steps
       }),
       do:
         is_binary(name) and is_list(arguments) and is_boolean(expected) and is_integer(steps) and
           steps >= 0

  defp valid_claim_example?(_example), do: false

  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)

  defp decode_effect_family(family) do
    parameters = Map.fetch!(family, "parameters")

    operations =
      family
      |> Map.fetch!("operations")
      |> Enum.map(fn operation ->
        decoded = %{
          name: Map.fetch!(operation, "name"),
          parameters: Enum.map(Map.fetch!(operation, "parameters"), &decode_type/1),
          parameter_names: Map.fetch!(operation, "parameter_names"),
          result: decode_type(Map.fetch!(operation, "result")),
          path: "interface://#{family["module"]}/#{operation["name"]}"
        }

        {decoded.name, decoded}
      end)
      |> Map.new()

    %{
      id: Map.fetch!(family, "id"),
      origin: Map.fetch!(family, "origin"),
      module: Map.fetch!(family, "module"),
      name: Map.fetch!(family, "name"),
      parameters: parameters,
      parameter_ids: Enum.to_list(0..length(parameters)//1) |> Enum.take(length(parameters)),
      arity: length(parameters),
      operations: operations,
      visibility: :public,
      imported?: true,
      path: "interface://#{family["module"]}/#{family["name"]}"
    }
  end

  defp decode_handler(handler) do
    %{
      id: Map.fetch!(handler, "id"),
      origin: Map.fetch!(handler, "origin"),
      module: Map.fetch!(handler, "module"),
      name: Map.fetch!(handler, "name"),
      family: Map.fetch!(handler, "family"),
      family_name: Map.fetch!(handler, "family_name"),
      arguments: Enum.map(Map.fetch!(handler, "arguments"), &decode_type/1),
      variables: Map.fetch!(handler, "variables"),
      input: decode_type(Map.fetch!(handler, "input")),
      output: decode_type(Map.fetch!(handler, "output")),
      parameters:
        Enum.map(Map.fetch!(handler, "parameters"), fn parameter ->
          %{
            name: Map.fetch!(parameter, "name"),
            parsed_type: decode_type(Map.fetch!(parameter, "type"))
          }
        end),
      uses_row: decode_effect_row(Map.fetch!(handler, "uses")),
      visibility: :public,
      imported?: true,
      path: "interface://#{handler["module"]}/#{handler["name"]}"
    }
  end

  defp decode_effect_row(%{"entries" => entries, "tail" => tail}) do
    Row.new(
      Enum.map(entries, fn entry ->
        %{
          family: Map.fetch!(entry, "family"),
          family_name: Map.fetch!(entry, "family_name"),
          arguments: Enum.map(Map.fetch!(entry, "arguments"), &decode_type/1),
          capability: Map.fetch!(entry, "capability"),
          name: Map.get(entry, "name"),
          abstract?: Map.get(entry, "abstract", false)
        }
      end),
      tail
    )
  end

  defp unique_identity_records?(records) do
    names = Enum.map(records, & &1.name)
    ids = Enum.map(records, & &1.id)
    names == Enum.uniq(names) and ids == Enum.uniq(ids)
  end

  defp valid_templates?(templates) when is_list(templates) do
    if Enum.all?(templates, fn template ->
         is_map(template) and is_binary(Map.get(template, "id")) and
           is_list(Map.get(template, "parameters")) and is_list(Map.get(template, "helpers")) and
           Map.has_key?(template, "body")
       end) do
      ids = MapSet.new(templates, & &1["id"])

      MapSet.size(ids) == length(templates) and
        Enum.all?(templates, &MapSet.subset?(MapSet.new(&1["helpers"]), ids))
    else
      false
    end
  end

  defp valid_templates?(_templates), do: false

  defp decode_scheme(%{"variables" => variables, "type" => type}),
    do: %Scheme{variables: variables, type: decode_type(type)}

  defp decode_type(%{"tag" => "integer"}), do: :integer
  defp decode_type(%{"tag" => "boolean"}), do: :boolean
  defp decode_type(%{"tag" => "variable", "id" => id}), do: {:var, id}
  defp decode_type(%{"tag" => "skolem", "id" => id}), do: {:skolem, id}

  defp decode_type(%{"tag" => "function", "parameter" => parameter, "result" => result}),
    do: {:function, decode_type(parameter), decode_type(result)}

  defp decode_type(%{"tag" => "tuple", "elements" => elements}),
    do: {:tuple, Enum.map(elements, &decode_type/1)}

  defp decode_type(%{"tag" => "nominal", "id" => id, "arguments" => arguments}),
    do: {:nominal, id, Enum.map(arguments, &decode_type/1)}

  defp decode_visibility("transparent"), do: :transparent
  defp decode_visibility("abstract"), do: :abstract
  defp decode_visibility(_), do: raise(ArgumentError, "invalid visibility")

  defp decode_inhabitation("empty"), do: :empty
  defp decode_inhabitation("inhabited"), do: :inhabited
  defp decode_inhabitation("unknown"), do: :unknown
  defp decode_inhabitation(_), do: :unknown

  defp decode_variance("phantom"), do: :phantom
  defp decode_variance("covariant"), do: :covariant
  defp decode_variance("contravariant"), do: :contravariant
  defp decode_variance("invariant"), do: :invariant
  defp decode_variance(_), do: raise(ArgumentError, "invalid variance")

  defp digest(payload),
    do: :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)

  defp secure_equal?(left, right) when byte_size(left) == byte_size(right) do
    left
    |> :binary.bin_to_list()
    |> Enum.zip(:binary.bin_to_list(right))
    |> Enum.reduce(0, fn {a, b}, acc -> Bitwise.bor(acc, Bitwise.bxor(a, b)) end) == 0
  end

  defp secure_equal?(_left, _right), do: false

  defp error(message), do: {:error, Diagnostic.new("A005", message)}
end
