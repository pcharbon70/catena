defmodule C004Identity do
  def map(callback, subject), do: callback.(subject)
  def map2(callback, first, second), do: callback.(first).(second)
  def from_value(value), do: value
end

defmodule Catena.C004CategoricalTest do
  use ExUnit.Case, async: false

  alias Catena.Categorical.{Standard, TypeTerm}
  alias Catena.Categorical.Law
  alias Catena.Package.Linker
  alias Catena.Type.Trait

  test "standard interface freezes all seventeen approachable capabilities and method ABI" do
    standard = Standard.interface!()

    assert length(standard["traits"]) == 17

    assert Enum.map(standard["traits"], &{&1["name"], &1["formal_name"]}) == [
             {"Equatable", "Setoid"},
             {"Orderable", "Ord"},
             {"Combiner", "Semigroup"},
             {"EmptyCombiner", "Monoid"},
             {"Reducible", "Foldable"},
             {"Mapper", "Functor"},
             {"TwoSlotMapper", "Bifunctor"},
             {"MultiMapper", "Apply"},
             {"ValueEmbedder", "Applicative"},
             {"CollectingMapper", "Traversable"},
             {"Chainable", "Chain"},
             {"Workflow", "Monad"},
             {"Composable", "Semigroupoid"},
             {"IdentityComposer", "Category"},
             {"TransformRouter", "Arrow"},
             {"ContextualMapper", "Extend"},
             {"FocusReader", "Comonad"}
           ]

    assert standard["law_evidence"] == ["promised", "tested", "derived"]
    assert standard["execution"]["callback_order"] == "left-to-right"

    compose = Enum.find(standard["traits"], &(&1["name"] == "Composable"))
    assert hd(compose["methods"])["order"] == ["first", "next"]
    assert hd(compose["methods"])["direction"] == "left-to-right"
  end

  test "standard List mapping and reduction stay stack safe on large inputs" do
    list_type =
      constructor(
        "catena://standard/0.4/List",
        "Type -> Type",
        "catena://standard/0.4"
      )

    categorical =
      Catena.Categorical.prepare!(
        %{
          frontend_version: "0.4",
          origin: "pkg://list-stack-safety",
          traits: [],
          instances: [],
          templates: []
        },
        %{types: []},
        []
      )

    assert {:ok, mapper} =
             Trait.resolve(categorical.registry, "Mapper", [TypeTerm.decode!(list_type)])

    assert mapper.methods == %{"map" => "Elixir.Catena.Standard.List.map"}

    assert {:ok, reducible} =
             Trait.resolve(categorical.registry, "Reducible", [TypeTerm.decode!(list_type)])

    assert reducible.methods == %{
             "summarize" => "Elixir.Catena.Standard.List.summarize"
           }

    subject = Enum.to_list(1..250_000)
    mapped = Catena.Standard.List.map(&(&1 + 1), subject)

    assert length(mapped) == 250_000
    assert hd(mapped) == 2
    assert List.last(mapped) == 250_001

    total =
      Catena.Standard.List.summarize(
        fn accumulator -> fn item -> accumulator + item end end,
        0,
        subject
      )

    assert total == div(250_000 * 250_001, 2)
  end

  test "AST 0.4 derives implicit instances and executable type-qualified operations" do
    json = JSON.encode!(derived_module())
    assert {:ok, module, binary, metadata} = Catena.compile_json(json)
    assert {:module, ^module} = :code.load_binary(module, ~c"c004-derived.beam", binary)

    box = Enum.find(metadata.core.data.types, &(&1.name == "Box"))
    constructor = String.to_atom("#{box.id}::Boxed")
    value = {constructor, 3}

    assert :erlang.apply(module, :"Box.equals", [value, value])
    assert :erlang.apply(module, :"Box.compare", [value, {constructor, 4}]) == -1

    assert :erlang.apply(module, :"Box.map", [fn item -> item + 2 end, value]) ==
             {constructor, 5}

    reducer = fn accumulator -> fn item -> accumulator + item end end
    assert :erlang.apply(module, :"Box.summarize", [reducer, 10, value]) == 13

    assert Enum.count(metadata.core.categorical.derivations) == 6
    assert Enum.count(metadata.core.categorical.instances, &(&1.law_status == :derived)) == 6
    assert Enum.all?(metadata.core.categorical.derivations, &(&1.operation_overrides == []))

    assert {:ok, reference_value} =
             Catena.Reference.Evaluator.run(metadata.core, "Box.map", [
               {:closure, &(&1 + 2)},
               {:catena_value, hd(box.constructors).id, [3]}
             ])

    assert reference_value == {:catena_value, hd(box.constructors).id, [5]}

    assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)

    collect_template = Enum.find(interface.templates, &(&1["id"] == "box_collect_via_trait"))

    context = constructor("Identity", "Type -> Type", "pkg://derived")

    collect_manifest = %{
      companion_module: "DerivedCollectCompanion",
      modules: [],
      interfaces: [],
      output: "DerivedCollectCompanion.beam",
      roots: [
        %{
          "template" => collect_template["id"],
          "export" => "collect_box",
          "types" => [context],
          "instances" =>
            Enum.map(~w(Mapper MultiMapper ValueEmbedder), fn trait ->
              %{"trait" => trait, "arguments" => [context]}
            end) ++
              [
                %{
                  "trait" => "CollectingMapper",
                  "arguments" => [
                    constructor(
                      "pkg://derived::Derived::Box/derive/a",
                      "Type -> Type",
                      "pkg://derived"
                    )
                  ]
                }
              ]
        }
      ]
    }

    assert {:ok, collect_module, collect_binary, collect_metadata} =
             Linker.link(collect_manifest, [interface])

    assert collect_metadata.evidence_erased
    collect_forms = inspect(collect_metadata.forms)
    assert collect_forms =~ "C004Identity"
    refute collect_forms =~ "instance_id"
    refute collect_forms =~ "dictionary"
    refute collect_forms =~ "trait_call"

    assert {:module, ^collect_module} =
             :code.load_binary(collect_module, ~c"c004-derived-collect.beam", collect_binary)

    assert :erlang.apply(collect_module, :collect_box, [fn item -> item * 2 end, value]) ==
             {constructor, 6}

    double_constructor = String.to_atom("#{box.id}::DoubleBoxed")
    observer = self()

    assert :erlang.apply(collect_module, :collect_box, [
             fn item ->
               send(observer, {:collected, item})
               item * 2
             end,
             {double_constructor, 2, 3}
           ]) == {double_constructor, 4, 6}

    assert_receive {:collected, 2}
    assert_receive {:collected, 3}
    refute_receive {:collected, _item}

    pair = Enum.find(metadata.core.data.types, &(&1.name == "Pair"))
    pair_constructor = String.to_atom("#{pair.id}::Paired")
    parent = self()

    mapped_pair =
      :erlang.apply(module, :"Pair.map_both", [
        fn item ->
          send(parent, {:visited, :first})
          item + 1
        end,
        fn item ->
          send(parent, {:visited, :second})
          item * 2
        end,
        {pair_constructor, 2, 3}
      ])

    assert mapped_pair == {pair_constructor, 3, 6}
    assert_receive {:visited, :first}
    assert_receive {:visited, :second}
    refute_receive {:visited, _position}

    assert {:ok, ^module, uniform_binary, uniform_metadata} =
             Catena.compile_json(json, layout: :uniform)

    assert {:module, ^module} =
             :code.load_binary(module, ~c"c004-derived-uniform.beam", uniform_binary)

    assert {:ok, uniform_interface} =
             Catena.Interface.decode(uniform_metadata.interface_binary)

    assert {:ok, ^collect_module, uniform_collect_binary, _uniform_collect_metadata} =
             Linker.link(collect_manifest, [uniform_interface])

    assert {:module, ^collect_module} =
             :code.load_binary(
               collect_module,
               ~c"c004-derived-collect-uniform.beam",
               uniform_collect_binary
             )

    uniform_value = {:catena_adt, String.to_atom(box.id), 0, {3}}

    assert :erlang.apply(collect_module, :collect_box, [fn item -> item * 2 end, uniform_value]) ==
             {:catena_adt, String.to_atom(box.id), 0, {6}}
  end

  test "all standard capabilities resolve coherent parent evidence and Workflow has two useful witnesses" do
    categorical =
      Catena.Categorical.prepare!(
        %{
          frontend_version: "0.4",
          origin: "pkg://corpus",
          traits: [],
          instances: [],
          templates: []
        },
        %{types: []},
        []
      )

    registry =
      Enum.reduce(Trait.public_traits(categorical.registry), categorical.registry, fn trait,
                                                                                      current ->
        head = constructor_for_kind(trait.parameters |> hd() |> Map.fetch!(:kind), "Corpus")
        Trait.add_instance(current, instance_for(trait, head))
      end)

    for trait <- Trait.public_traits(registry) do
      head = constructor_for_kind(trait.parameters |> hd() |> Map.fetch!(:kind), "Corpus")
      assert {:ok, evidence} = Trait.resolve(registry, trait.name, [head])
      assert evidence.law_status == :promised
      assert length(evidence.parents) == length(trait.parents)
    end

    workflow_traits = ~w(Mapper MultiMapper ValueEmbedder Chainable Workflow)

    registry =
      Enum.reduce(~w(Option Result), registry, fn witness, outer ->
        head = constructor_for_kind({:arrow, :type, :type}, witness)

        Enum.reduce(workflow_traits, outer, fn name, inner ->
          trait = Trait.trait(inner, name)
          Trait.add_instance(inner, instance_for(trait, head))
        end)
      end)

    for witness <- ~w(Option Result) do
      head = constructor_for_kind({:arrow, :type, :type}, witness)
      assert {:ok, evidence} = Trait.resolve(registry, "Workflow", [head])
      assert Enum.map(evidence.parents, & &1.trait_name) == ["ValueEmbedder", "Chainable"]
    end

    unitless_examples = %{
      "Combiner" => ~w(NonEmptyList ValidationErrors),
      "MultiMapper" => ~w(OptionZip Validation),
      "Chainable" => ~w(OptionFlow ResultFlow),
      "Composable" => ~w(FunctionPipeline ParserPipeline),
      "ContextualMapper" => ~w(NonEmptyZipper AnnotatedTree)
    }

    {_registry, resolved} =
      Enum.reduce(unitless_examples, {registry, []}, fn {name, examples}, {current, resolved} ->
        trait = Trait.trait(current, name)

        Enum.reduce(examples, {current, resolved}, fn example, {inner, observations} ->
          head = constructor_for_kind(hd(trait.parameters).kind, example)
          inner = add_instance_with_parents(inner, trait, head)
          assert {:ok, evidence} = Trait.resolve(inner, name, [head])
          {inner, [{name, example, evidence.instance_id} | observations]}
        end)
      end)

    assert length(resolved) == 10
  end

  test "law testing requires explicit Equatable evidence and bounded function samples" do
    categorical =
      Catena.Categorical.prepare!(
        %{
          frontend_version: "0.4",
          origin: "pkg://laws",
          traits: [],
          instances: [],
          templates: []
        },
        %{types: []},
        []
      )

    trait = Trait.trait(categorical.registry, "Equatable")
    head = constructor_for_kind(:type, "LawInt")
    registry = Trait.add_instance(categorical.registry, instance_for(trait, head))

    evidence =
      Law.check_equatable!(registry, [head], [0, 1, 2], fn _evidence, "equals", [left, right] ->
        left === right
      end)

    assert evidence.status == :tested
    assert evidence.sample_count == 3
    assert Law.extensionally_equal?(&(&1 + 1), fn value -> 1 + value end, [0, 1, 2])
    refute Law.extensionally_equal?(&(&1 + 1), &(&1 * 2), [0, 1, 2])
  end

  test "package specialization resolves evidence to a direct call and is deterministic" do
    assert {:ok, _module, _binary, metadata} =
             Catena.compile_json(JSON.encode!(template_module()))

    assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)

    manifest = %{
      companion_module: "TemplateCompanion",
      modules: [],
      interfaces: [],
      output: "TemplateCompanion.beam",
      roots: [
        %{
          "template" => "equals_specialized",
          "export" => "equals_int",
          "types" => [constructor("Int", "Type", "pkg://template")],
          "instances" => [
            %{
              "trait" => "Equatable",
              "arguments" => [constructor("Int", "Type", "pkg://template")]
            }
          ]
        }
      ]
    }

    assert {:ok, module, first, first_metadata} = Linker.link(manifest, [interface])
    assert {:ok, ^module, second, second_metadata} = Linker.link(manifest, [interface])
    assert first == second
    assert first_metadata.specialization_keys == second_metadata.specialization_keys
    assert first_metadata.evidence_erased

    forms = inspect(first_metadata.forms)
    assert forms =~ ":erlang"
    refute forms =~ "dictionary"
    refute forms =~ "instance_id"

    assert {:module, ^module} = :code.load_binary(module, ~c"c004-link.beam", first)
    assert :erlang.apply(module, :equals_int, [7, 7])
    refute :erlang.apply(module, :equals_int, [7, 8])
  end

  test "toolchain manifest writes the declared companion BEAM relative to itself" do
    assert {:ok, _module, _binary, metadata} =
             Catena.compile_json(JSON.encode!(template_module()))

    directory = Path.join(System.tmp_dir!(), "catena-c004-#{System.unique_integer([:positive])}")
    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)
    File.write!(Path.join(directory, "source.cati.json"), metadata.interface_binary)

    manifest = %{
      "format" => "catena-package-manifest",
      "version" => "0.4",
      "companion_module" => "ManifestCompanion",
      "modules" => [],
      "interfaces" => ["source.cati.json"],
      "roots" => [
        %{
          "template" => "equals_specialized",
          "export" => "equals_int",
          "types" => [constructor("Int", "Type", "pkg://template")],
          "instances" => [
            %{
              "trait" => "Equatable",
              "arguments" => [constructor("Int", "Type", "pkg://template")]
            }
          ]
        }
      ],
      "output" => "build/ManifestCompanion.beam"
    }

    path = Path.join(directory, "package.json")
    File.write!(path, JSON.encode!(manifest))

    assert {:ok, result} = Linker.compile_manifest(path)
    assert result.output == Path.join(directory, "build/ManifestCompanion.beam")
    assert File.read!(result.output) |> byte_size() > 0
    assert result.evidence_erased
  end

  test "0.4 rejects reserved law trust and incomplete template closure" do
    invalid_law =
      template_module()
      |> put_in(["instances", Access.at(0), "law_status"], "trusted")

    assert {:error, law_error} = invalid_law |> JSON.encode!() |> Catena.check_json()
    assert law_error.id == "TRT005"

    invalid_template =
      template_module()
      |> put_in(["templates", Access.at(0), "helpers"], ["missing_helper"])

    assert {:error, closure_error} = invalid_template |> JSON.encode!() |> Catena.check_json()
    assert closure_error.id == "TRT006"
  end

  test "type term codec preserves higher-kinded applications" do
    encoded = %{
      "tag" => "application",
      "callee" => constructor("Pair", "Type -> Type -> Type", "pkg://kind"),
      "argument" => %{"tag" => "variable", "name" => "a", "kind" => "Type"}
    }

    assert encoded |> TypeTerm.decode!() |> TypeTerm.encode() == encoded
  end

  defp derived_module do
    %{
      "version" => "0.4",
      "origin" => "pkg://derived",
      "module" => "Derived",
      "exports" => [],
      "type_exports" => [
        %{"name" => "Box", "visibility" => "transparent"},
        %{"name" => "Pair", "visibility" => "transparent"}
      ],
      "imports" => [],
      "traits" => [],
      "templates" => [box_collect_wrapper()],
      "definitions" => [],
      "type_groups" => [
        %{
          "declarations" => [
            %{
              "name" => "Box",
              "parameters" => [%{"name" => "a", "kind" => "Type"}],
              "constructors" => [
                %{"name" => "Boxed", "fields" => [%{"tag" => "variable", "name" => "a"}]},
                %{
                  "name" => "DoubleBoxed",
                  "fields" => [
                    %{"tag" => "variable", "name" => "a"},
                    %{"tag" => "variable", "name" => "a"}
                  ]
                }
              ],
              "derivations" => [
                %{"capability" => "Equatable", "targets" => ["a"]},
                %{"capability" => "Orderable", "targets" => ["a"]},
                %{"capability" => "Mapper", "targets" => ["a"]},
                %{"capability" => "Reducible", "targets" => ["a"]},
                %{"capability" => "CollectingMapper", "targets" => ["a"]}
              ]
            },
            %{
              "name" => "Pair",
              "parameters" => [
                %{"name" => "a", "kind" => "Type"},
                %{"name" => "b", "kind" => "Type"}
              ],
              "constructors" => [
                %{
                  "name" => "Paired",
                  "fields" => [
                    %{"tag" => "variable", "name" => "a"},
                    %{"tag" => "variable", "name" => "b"}
                  ]
                }
              ],
              "derivations" => [
                %{"capability" => "TwoSlotMapper", "targets" => ["a", "b"]}
              ]
            }
          ]
        }
      ],
      "instances" => identity_instances()
    }
  end

  defp template_module do
    type = constructor("Int", "Type", "pkg://template")

    %{
      "version" => "0.4",
      "origin" => "pkg://template",
      "module" => "TemplateSource",
      "exports" => [],
      "type_exports" => [],
      "type_groups" => [],
      "imports" => [],
      "definitions" => [],
      "traits" => [],
      "instances" => [
        %{
          "trait" => "Equatable",
          "arguments" => [type],
          "owner" => "pkg://template",
          "methods" => %{"equals" => "erlang.=:="},
          "law_status" => "tested"
        }
      ],
      "templates" => [
        %{
          "id" => "equals_specialized",
          "parameters" => ["left", "right"],
          "helpers" => [],
          "body" => %{
            "tag" => "trait_call",
            "trait" => "Equatable",
            "arguments" => [
              %{"tag" => "variable", "name" => "$type0", "kind" => "Type"}
            ],
            "method" => "equals",
            "values" => [
              %{"tag" => "argument", "name" => "left"},
              %{"tag" => "argument", "name" => "right"}
            ]
          }
        }
      ]
    }
  end

  defp constructor(id, kind, owner),
    do: %{"tag" => "constructor", "id" => id, "kind" => kind, "owner" => owner}

  defp identity_instances do
    head = constructor("Identity", "Type -> Type", "pkg://derived")

    [
      %{
        "trait" => "Mapper",
        "arguments" => [head],
        "owner" => "pkg://derived",
        "methods" => %{"map" => "Elixir.C004Identity.map"},
        "law_status" => "tested"
      },
      %{
        "trait" => "MultiMapper",
        "arguments" => [head],
        "owner" => "pkg://derived",
        "methods" => %{"map2" => "Elixir.C004Identity.map2"},
        "law_status" => "tested"
      },
      %{
        "trait" => "ValueEmbedder",
        "arguments" => [head],
        "owner" => "pkg://derived",
        "methods" => %{"from_value" => "Elixir.C004Identity.from_value"},
        "law_status" => "tested"
      }
    ]
  end

  defp box_collect_wrapper do
    collect_id = "pkg://derived::Derived::Box#collect_map/a"

    %{
      "id" => "box_collect_via_trait",
      "parameters" => ["callback", "subject"],
      "helpers" => [collect_id],
      "body" => %{
        "tag" => "trait_call",
        "trait" => "CollectingMapper",
        "arguments" => [
          constructor(
            "pkg://derived::Derived::Box/derive/a",
            "Type -> Type",
            "pkg://derived"
          )
        ],
        "method" => "collect_map",
        "values" => [
          %{"tag" => "argument", "name" => "callback"},
          %{"tag" => "argument", "name" => "subject"}
        ]
      }
    }
  end

  defp constructor_for_kind(kind, name),
    do: {:constructor, "corpus://#{name}", kind, "catena://standard/0.4"}

  defp instance_for(trait, head) do
    %{
      trait: trait.id,
      arguments: [head],
      owner: trait.origin,
      context: [],
      methods: Map.new(trait.methods, &{&1.name, "erlang.#{&1.name}"}),
      associated_types: %{},
      law_status: :promised
    }
  end

  defp add_instance_with_parents(registry, trait, head) do
    registry =
      Enum.reduce(trait.parents, registry, fn parent, current ->
        add_instance_with_parents(current, Trait.trait(current, parent.trait), head)
      end)

    if Enum.any?(registry.instances, &(&1.trait == trait.id and &1.arguments == [head])) do
      registry
    else
      Trait.add_instance(registry, instance_for(trait, head))
    end
  end
end
