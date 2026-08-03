defmodule Catena.Categorical do
  @moduledoc "Elaboration boundary for Catena 0.4+ traits, instances, laws, and templates."

  alias Catena.Categorical.{Standard, TypeTerm}
  alias Catena.{Diagnostic, Kind}
  alias Catena.Type.Trait

  @law_statuses %{"promised" => :promised, "tested" => :tested, "derived" => :derived}

  @spec prepare!(map(), map(), [map()]) :: map()
  def prepare!(%{frontend_version: version}, _data, _interfaces)
      when version not in ~w(0.4 0.5 0.6),
      do: empty()

  def prepare!(ast, data, interfaces) do
    standard = Standard.interface!()
    verify_standard_digests!(interfaces, standard["digest"])

    registry =
      Trait.new()
      |> add_traits(standard["traits"], standard["origin"], "standard://CatenaStandard")
      |> add_interfaces(interfaces)
      |> add_traits(ast.traits, ast.origin, "$.traits")
      |> add_instances(standard["instances"], standard["origin"], "standard://CatenaStandard")
      |> add_interface_instances(interfaces)
      |> add_instances(ast.instances, ast.origin, "$.instances")

    {derivations, registry} = derivations!(data, registry, ast)

    templates =
      (templates!(ast.templates, ast.origin) ++ derivation_templates(derivations, ast))
      |> validate_template_closure!()
      |> Enum.sort_by(& &1["id"])

    %{
      registry: registry,
      traits: exported_traits(registry, ast.origin),
      instances: exported_instances(registry, ast.origin),
      templates: templates,
      derivations: derivations,
      standard_digest: standard["digest"],
      standard_origin: standard["origin"],
      law_evidence: Enum.map(Map.keys(@law_statuses), &String.to_existing_atom/1)
    }
  end

  @spec empty() :: map()
  def empty do
    %{
      registry: Trait.new(),
      traits: [],
      instances: [],
      templates: [],
      derivations: [],
      standard_digest: nil,
      standard_origin: nil,
      law_evidence: []
    }
  end

  defp add_interfaces(registry, interfaces) do
    Enum.reduce(interfaces, registry, fn interface, current ->
      add_traits(
        current,
        Map.get(interface, :traits, []),
        interface.origin,
        "interface://#{interface.module}"
      )
    end)
  end

  defp verify_standard_digests!(interfaces, expected) do
    Enum.each(interfaces, fn interface ->
      digest = Map.get(interface, :standard_digest)

      if not is_nil(digest) and digest != expected do
        fail(
          "TRT008",
          "interface #{interface.module} was compiled against a different standard hierarchy",
          "interface://#{interface.module}"
        )
      end
    end)
  end

  defp add_interface_instances(registry, interfaces) do
    Enum.reduce(interfaces, registry, fn interface, current ->
      add_instances(
        current,
        Map.get(interface, :instances, []),
        interface.origin,
        "interface://#{interface.module}"
      )
    end)
  end

  defp add_traits(registry, traits, origin, base_path) when is_list(traits) do
    traits
    |> Enum.with_index()
    |> Enum.reduce(registry, fn {declaration, index}, current ->
      path = Map.get(declaration, :path, Map.get(declaration, "path", "#{base_path}[#{index}]"))
      Trait.add_trait(current, decode_trait!(declaration, origin, path))
    end)
  end

  defp add_instances(registry, instances, origin, base_path) when is_list(instances) do
    instances
    |> Enum.with_index()
    |> Enum.reduce(registry, fn {declaration, index}, current ->
      path = Map.get(declaration, :path, Map.get(declaration, "path", "#{base_path}[#{index}]"))
      Trait.add_instance(current, decode_instance!(declaration, origin, path))
    end)
  end

  defp decode_trait!(trait, origin, path) do
    parameters =
      field(trait, "parameters", [])
      |> Enum.map(fn parameter ->
        %{name: field!(parameter, "name"), kind: Kind.parse!(field!(parameter, "kind"), path)}
      end)

    parents =
      field(trait, "parents", [])
      |> Enum.map(fn parent ->
        %{
          trait: field!(parent, "trait"),
          arguments: Enum.map(field(parent, "arguments", []), &TypeTerm.decode!(&1, path: path))
        }
      end)

    %{
      id: field(trait, "id", "#{origin}##{field!(trait, "name")}"),
      name: field!(trait, "name"),
      formal_name: field(trait, "formal_name"),
      origin: origin,
      parameters: parameters,
      parents: parents,
      methods: Enum.map(field(trait, "methods", []), &normalize_record!(&1, :method, path)),
      laws: Enum.map(field(trait, "laws", []), &normalize_record!(&1, :law, path)),
      fundeps: decode_fundeps(field(trait, "fundeps", [])),
      path: path
    }
  end

  defp decode_instance!(instance, origin, path) do
    law_status = field(instance, "law_status", "promised")

    unless Map.has_key?(@law_statuses, law_status) do
      fail("TRT005", "law evidence must be promised, tested, or derived", path)
    end

    %{
      id: field(instance, "id"),
      trait: field!(instance, "trait"),
      arguments: Enum.map(field!(instance, "arguments"), &TypeTerm.decode!(&1, path: path)),
      owner: field(instance, "owner", origin),
      context:
        Enum.map(field(instance, "context", []), fn predicate ->
          %{
            trait: field!(predicate, "trait"),
            arguments:
              Enum.map(field(predicate, "arguments", []), &TypeTerm.decode!(&1, path: path))
          }
        end),
      methods: field(instance, "methods", %{}),
      associated_types:
        Map.new(field(instance, "associated_types", %{}), fn {name, type} ->
          {name, TypeTerm.decode!(type, path: path)}
        end),
      law_status: Map.fetch!(@law_statuses, law_status),
      derivation: field(instance, "derivation"),
      path: path
    }
  end

  defp templates!(templates, origin) when is_list(templates) do
    templates
    |> Enum.with_index()
    |> Enum.map(fn {template, index} ->
      path = Map.get(template, :path, Map.get(template, "path", "$.templates[#{index}]"))
      id = field!(template, "id")
      parameters = field(template, "parameters", [])
      helpers = field(template, "helpers", [])

      unless is_binary(id) and is_list(parameters) and is_list(helpers) and
               Enum.all?(parameters, &is_binary/1) and Enum.all?(helpers, &is_binary/1) do
        fail("TRT006", "template requires an id and string parameter/helper lists", path)
      end

      %{
        "id" => id,
        "origin" => origin,
        "parameters" => parameters,
        "body" => field!(template, "body"),
        "helpers" => Enum.sort(Enum.uniq(helpers))
      }
    end)
    |> Enum.sort_by(& &1["id"])
  end

  defp validate_template_closure!(templates) do
    ids = MapSet.new(templates, & &1["id"])

    if MapSet.size(ids) != length(templates) do
      fail("TRT006", "template identities must be unique", "$.templates")
    end

    Enum.each(templates, fn template ->
      missing = MapSet.difference(MapSet.new(template["helpers"]), ids)

      if MapSet.size(missing) > 0 do
        fail(
          "TRT006",
          "template #{template["id"]} has missing helper closure: #{Enum.join(missing, ", ")}",
          "$.templates"
        )
      end
    end)

    templates
  end

  defp derivations!(data, registry, ast) do
    allowed = ~w(Equatable Orderable Mapper TwoSlotMapper Reducible CollectingMapper)

    plans =
      data.types
      |> Enum.flat_map(fn type ->
        type.derivations
        |> Enum.reject(&(&1 == "fold"))
        |> Enum.map(fn
          %{"capability" => capability, "targets" => targets} when is_list(targets) ->
            unless capability in allowed do
              fail(
                "DRV001",
                "unsupported categorical derivation #{inspect(capability)}",
                type.path
              )
            end

            unless type.visibility == :transparent do
              fail("DRV001", "categorical derivation requires a transparent datatype", type.path)
            end

            unknown = targets -- Enum.map(type.parameters, & &1.name)

            if unknown != [] do
              fail(
                "DRV001",
                "unknown derivation target parameters: #{Enum.join(unknown, ", ")}",
                type.path
              )
            end

            if is_nil(Trait.trait(registry, capability)) do
              fail("DRV001", "unknown derived capability #{capability}", type.path)
            end

            validate_target_count!(capability, targets, type.path)
            validate_structural_positions!(capability, targets, type)

            %{
              capability: capability,
              type_id: type.id,
              type_name: type.name,
              targets: targets,
              function: "#{type.name}.#{derived_function(capability)}",
              law_status: :derived,
              operation_overrides: [],
              type: type,
              path: type.path
            }

          other ->
            fail("DRV001", "unsupported categorical derivation #{inspect(other)}", type.path)
        end)
      end)

    registry =
      Enum.reduce(plans, registry, fn plan, current ->
        trait = Trait.trait(current, plan.capability)
        function = plan.function

        methods =
          Map.new(trait.methods, fn method ->
            implementation =
              if plan.capability == "CollectingMapper" do
                "template:#{collect_template_id(plan.type, plan.targets)}"
              else
                "#{ast.module}.#{function}"
              end

            {method.name, implementation}
          end)

        Trait.add_instance(current, %{
          trait: plan.capability,
          arguments: [derived_head(plan.type, plan.targets, plan.capability)],
          owner: plan.type.origin,
          context: [],
          methods: methods,
          associated_types: %{},
          law_status: :derived,
          derivation: %{
            "capability" => plan.capability,
            "type" => plan.type_id,
            "targets" => plan.targets
          },
          path: plan.path
        })
      end)

    {plans, registry}
  end

  defp validate_target_count!(capability, targets, path) do
    expected =
      case capability do
        capability when capability in ~w(Mapper Reducible CollectingMapper) -> 1
        "TwoSlotMapper" -> 2
        capability when capability in ~w(Equatable Orderable) -> :any
      end

    if expected != :any and length(targets) != expected do
      fail("DRV001", "#{capability} derivation requires #{expected} target parameter(s)", path)
    end
  end

  defp validate_structural_positions!(capability, targets, type)
       when capability in ~w(Mapper TwoSlotMapper Reducible CollectingMapper) do
    indexes =
      Enum.map(targets, fn target -> Enum.find_index(type.parameters, &(&1.name == target)) end)

    Enum.each(type.constructors, fn constructor ->
      Enum.each(constructor.fields, fn field ->
        if Enum.any?(indexes, &contains_variable?(field.type, &1)) and
             not direct_target?(field.type, indexes) do
          fail(
            "DRV001",
            "Catena 0.4 structural derivation requires target parameters to occupy whole fields",
            type.path
          )
        end
      end)
    end)
  end

  defp validate_structural_positions!(_capability, _targets, _type), do: :ok

  defp direct_target?({:var, index}, indexes), do: index in indexes
  defp direct_target?(_type, _indexes), do: false

  defp contains_variable?({:var, index}, index), do: true

  defp contains_variable?({:function, parameter, result}, index),
    do: contains_variable?(parameter, index) or contains_variable?(result, index)

  defp contains_variable?({:tuple, elements}, index),
    do: Enum.any?(elements, &contains_variable?(&1, index))

  defp contains_variable?({:nominal, _id, arguments}, index),
    do: Enum.any?(arguments, &contains_variable?(&1, index))

  defp contains_variable?(_type, _index), do: false

  defp derived_head(type, _targets, capability) when capability in ~w(Equatable Orderable) do
    constructor = {:constructor, type.id, constructor_kind(type.arity), type.origin}

    Enum.reduce(type.parameters, constructor, fn parameter, current ->
      {:application, current, {:variable, parameter.name, :type}}
    end)
  end

  defp derived_head(type, targets, _capability) do
    id = "#{type.id}/derive/#{Enum.join(targets, "+")}"
    {:constructor, id, constructor_kind(length(targets)), type.origin}
  end

  defp constructor_kind(0), do: :type
  defp constructor_kind(arity), do: {:arrow, :type, constructor_kind(arity - 1)}

  defp exported_traits(registry, origin) do
    registry.traits
    |> Map.values()
    |> Enum.filter(&(&1.origin == origin))
    |> Enum.sort_by(& &1.id)
  end

  defp exported_instances(registry, origin) do
    registry.instances
    |> Enum.filter(&(&1.owner == origin))
    |> Enum.sort_by(& &1.id)
  end

  defp derived_function("Equatable"), do: "equals"
  defp derived_function("Orderable"), do: "compare"
  defp derived_function("Mapper"), do: "map"
  defp derived_function("TwoSlotMapper"), do: "map_both"
  defp derived_function("Reducible"), do: "summarize"
  defp derived_function("CollectingMapper"), do: "collect_map"

  defp derivation_templates(plans, ast) do
    plans
    |> Enum.filter(&(&1.capability == "CollectingMapper"))
    |> Enum.flat_map(fn plan ->
      collect_templates(plan.type, plan.targets, ast)
    end)
  end

  defp collect_templates(type, targets, ast) do
    target_indexes =
      Enum.map(targets, &Enum.find_index(type.parameters, fn p -> p.name == &1 end))

    constructors =
      Enum.map(type.constructors, fn constructor ->
        helper_id = "#{type.id}#construct/#{constructor.index}"

        %{
          "index" => constructor.index,
          "arity" => length(constructor.fields),
          "targets" =>
            Enum.map(constructor.fields, fn field ->
              Enum.find_index(target_indexes, &(&1 == direct_variable(field.type)))
            end),
          "helper" => helper_id
        }
      end)

    helper_templates =
      Enum.map(type.constructors, fn constructor ->
        parameters = Enum.map(constructor.fields, &"field#{&1.index}")

        %{
          "id" => "#{type.id}#construct/#{constructor.index}",
          "origin" => ast.origin,
          "parameters" => parameters,
          "helpers" => [],
          "body" => %{
            "tag" => "call",
            "module" => ast.module,
            "function" => "#{type.name}.__construct.#{constructor.index}",
            "arguments" => Enum.map(parameters, &%{"tag" => "argument", "name" => &1})
          }
        }
      end)

    collect = %{
      "id" => collect_template_id(type, targets),
      "origin" => ast.origin,
      "parameters" => ["callback", "subject"],
      "helpers" => Enum.map(helper_templates, & &1["id"]),
      "body" => %{
        "tag" => "derived_collect",
        "eliminator" => "#{ast.module}.#{type.name}.__eliminate",
        "constructors" => constructors,
        "context_type" => %{
          "tag" => "variable",
          "name" => "$type0",
          "kind" => "Type -> Type"
        },
        "callback" => "callback",
        "subject" => "subject"
      }
    }

    [collect | helper_templates]
  end

  defp collect_template_id(type, targets),
    do: "#{type.id}#collect_map/#{Enum.join(targets, "+")}"

  defp direct_variable({:var, index}), do: index
  defp direct_variable(_type), do: nil

  defp decode_fundeps(fundeps) do
    Enum.map(fundeps, fn fundep ->
      {field(fundep, "inputs", []), field(fundep, "outputs", [])}
    end)
  end

  defp normalize_record!(record, kind, path) when is_map(record) do
    allowed =
      case kind do
        :method ->
          %{
            "name" => :name,
            "arity" => :arity,
            "order" => :order,
            "direction" => :direction,
            "signature" => :signature,
            "documentation" => :documentation
          }

        :law ->
          %{
            "id" => :id,
            "domain" => :domain,
            "equation" => :equation,
            "documentation" => :documentation
          }
      end

    Map.new(record, fn
      {key, value} when is_atom(key) ->
        {key, value}

      {key, value} when is_binary(key) ->
        case Map.fetch(allowed, key) do
          {:ok, atom} -> {atom, value}
          :error -> fail("TRT001", "unknown #{kind} metadata #{key}", path)
        end
    end)
  end

  defp normalize_record!(_record, kind, path),
    do: fail("TRT001", "#{kind} declarations must be objects", path)

  defp field(map, key, default \\ nil) do
    case Map.fetch(map, key) do
      {:ok, value} ->
        value

      :error ->
        Enum.find_value(map, default, fn
          {atom, value} when is_atom(atom) -> if Atom.to_string(atom) == key, do: value
          _entry -> nil
        end)
    end
  end

  defp field!(map, key) do
    case field(map, key) do
      nil -> fail("TRT001", "missing #{key}", Map.get(map, :path))
      value -> value
    end
  end

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
