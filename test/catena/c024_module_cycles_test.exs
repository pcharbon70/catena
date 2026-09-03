defmodule Catena.C024ModuleCyclesTest do
  use ExUnit.Case, async: false

  alias Catena.{Interface, LanguageLifecycle, LanguageVersion, Scc}

  @a_types [
    %{
      "name" => "Pair",
      "parameters" => [],
      "constructors" => [
        %{"name" => "Base", "fields" => []},
        %{
          "name" => "Wrap",
          "fields" => [%{"tag" => "named", "name" => "B.Mark", "arguments" => []}]
        }
      ],
      "derivations" => []
    }
  ]
  @b_types [
    %{
      "name" => "Mark",
      "parameters" => [],
      "constructors" => [
        %{
          "name" => "Tag",
          "fields" => [%{"tag" => "named", "name" => "A.Pair", "arguments" => []}]
        }
      ],
      "derivations" => []
    }
  ]

  @tag obligations: ~w(CY-OBL-001 CY-OBL-008)
  test "0.1.20 is an exact registered revision with predecessors pinned" do
    assert LanguageVersion.latest() == "0.1.46"

    assert LanguageVersion.source_text_frontend_versions() ==
             ~w(0.1.9 0.1.10 0.1.11 0.1.12 0.1.13 0.1.14 0.1.15 0.1.16 0.1.17 0.1.18 0.1.19 0.1.20 0.1.21 0.1.22 0.1.23 0.1.24 0.1.25 0.1.26 0.1.27 0.1.28 0.1.29 0.1.30 0.1.31 0.1.32 0.1.33 0.1.34 0.1.35 0.1.36 0.1.37 0.1.38 0.1.39 0.1.40 0.1.41 0.1.42 0.1.43 0.1.44 0.1.45 0.1.46)

    refute "0.1.20" in LanguageVersion.compilable_revisions()
    refute "0.1.20" in LanguageVersion.artifact_versions()
    refute "0.1.20" in LanguageVersion.signed_format_versions()

    assert {:ok, :stable} == LanguageLifecycle.state("module-dependency-cycles", "0.1.20")

    change =
      Enum.find(
        LanguageLifecycle.changes(),
        &(&1["id"] == "change-0-1-20-module-dependency-cycles")
      )

    assert change["affects"] == ~w(source-acceptance static-meaning diagnostics interfaces)

    assert String.contains?(
             change["specification"],
             "module-dependency-cycles/scc-admission-and-resolution.md#"
           )

    assert {:error,
            %{
              id: "EDN001",
              details: %{frontend: "namespaces-and-shadowing", required: "0.1.22"}
            }} =
             Catena.build_namespace_environment([], language_selection: selection("0.1.18"))

    assert {:ok, %{selection: %{language_revision: "0.1.13"}}} = Catena.scan_literal("1.0")

    assert {:ok, %{selection: %{language_revision: "0.1.16"}}} =
             Catena.resolve_file_unit("", "A.cat", [])

    assert {:ok, _} = Catena.build_namespace_environment([])

    refute function_exported?(Catena, :compile_signature_file, 1)
    refute function_exported?(Catena, :joint_inference, 1)
  end

  @tag obligations: ~w(CY-OBL-002 CY-OBL-003)
  test "SCCs group cycles and resolve through the two regimes" do
    {:ok, env} =
      Catena.build_namespace_environment([
        %{
          event: :provide_module,
          module: "A",
          digest: "",
          exports: [],
          dependencies: ["B"],
          signatures: []
        },
        %{
          event: :provide_module,
          module: "B",
          digest: "",
          exports: [],
          dependencies: ["A"],
          signatures: []
        }
      ])

    assert env.sccs == %{"A" => ["A", "B"]}

    {:ok, ring} =
      Catena.build_namespace_environment([
        %{
          event: :provide_module,
          module: "A",
          digest: "",
          exports: [],
          dependencies: ["C"],
          signatures: []
        },
        %{
          event: :provide_module,
          module: "B",
          digest: "",
          exports: [],
          dependencies: ["A"],
          signatures: []
        },
        %{
          event: :provide_module,
          module: "C",
          digest: "",
          exports: [],
          dependencies: ["B"],
          signatures: []
        }
      ])

    assert ring.sccs |> Map.get("A") |> Enum.sort() == ["A", "B", "C"]

    {:ok, self_env} =
      Catena.build_namespace_environment(
        [
          %{
            event: :provide_module,
            module: "A",
            digest: "",
            exports: [%{category: :values, spelling: "f"}],
            dependencies: ["A"],
            signatures: [%{category: :values, spelling: "f"}]
          },
          %{event: :import_module, module: "A", digest: "", names: [values: "f"]}
        ],
        current_module: "A"
      )

    assert self_env.sccs == %{"A" => ["A"]}
    assert {:ok, _} = Catena.resolve_name(self_env, %{category: :values, spelling: "f"})

    {:ok, cross} =
      Catena.build_namespace_environment(
        [
          %{
            event: :provide_module,
            module: "A",
            digest: "",
            exports: [],
            dependencies: ["B"],
            signatures: []
          },
          %{
            event: :provide_module,
            module: "B",
            digest: "",
            exports: [%{category: :values, spelling: "step"}],
            dependencies: ["A"],
            signatures: [%{category: :values, spelling: "step"}]
          },
          %{event: :import_module, module: "B", digest: "", names: [values: "step"]}
        ],
        current_module: "A"
      )

    assert {:ok, resolution} = Catena.resolve_name(cross, %{category: :values, spelling: "step"})
    assert resolution.origin == "B"
  end

  @tag obligations: ~w(CY-OBL-005)
  test "regime mixing and signature gaps fail as CYC001 at the closing transaction" do
    assert {:error,
            %{
              id: "CYC001",
              details: %{reason: "regime_mixing", from: "A", to: "B", digest: "d1"}
            }} =
             Catena.build_namespace_environment(
               [
                 %{
                   event: :provide_module,
                   module: "A",
                   digest: "",
                   exports: [],
                   dependencies: ["B"],
                   signatures: []
                 },
                 %{
                   event: :provide_module,
                   module: "B",
                   digest: "",
                   exports: [],
                   dependencies: ["A"],
                   signatures: []
                 },
                 %{event: :import_module, module: "B", digest: "d1", names: []}
               ],
               current_module: "A"
             )

    assert {:error,
            %{
              id: "CYC001",
              details: %{
                reason: "signature_gap",
                module: "B",
                category: :values,
                spelling: "step"
              }
            }} =
             Catena.build_namespace_environment([
               %{
                 event: :provide_module,
                 module: "A",
                 digest: "",
                 exports: [],
                 dependencies: ["B"],
                 signatures: []
               },
               %{
                 event: :provide_module,
                 module: "B",
                 digest: "",
                 exports: [%{category: :values, spelling: "step"}],
                 dependencies: ["A"],
                 signatures: []
               }
             ])

    assert {:error, %{id: "CYC001", details: %{reason: "regime_mixing"}}} =
             Catena.build_namespace_environment(
               [
                 %{
                   event: :provide_module,
                   module: "A",
                   digest: "d9",
                   exports: [],
                   dependencies: ["A"],
                   signatures: []
                 },
                 %{event: :import_module, module: "A", digest: "d9", names: []}
               ],
               current_module: "A"
             )

    assert {:error, %{id: "NSP001", details: %{reason: "invalid_event"}}} =
             Catena.build_namespace_environment([
               %{
                 event: :provide_module,
                 module: "A",
                 digest: "",
                 exports: [],
                 dependencies: [:not_a_module],
                 signatures: []
               }
             ])

    gap_module = %{
      "version" => "0.1.2",
      "origin" => "test://gap",
      "module" => "Gap",
      "type_groups" => [],
      "type_exports" => [],
      "imports" => [],
      "exports" => ["missing"],
      "definitions" => [
        %{"name" => "missing", "parameters" => [], "body" => %{"tag" => "integer", "value" => 1}}
      ]
    }

    assert {:error, %{id: "T008"}} = Catena.compile_scc([JSON.encode!(gap_module)])
  end

  @tag obligations: ~w(CY-OBL-004 CY-OBL-006 CY-OBL-009)
  test "acyclic behavior is C022-identical and joint digests are deterministic" do
    {:ok, alone} =
      Catena.build_namespace_environment(
        [
          %{
            event: :provide_module,
            module: "Solo",
            digest: "d1",
            exports: [%{category: :values, spelling: "z"}]
          },
          %{event: :import_module, module: "Solo", digest: "d1", names: [values: "z"]}
        ],
        current_module: "Client"
      )

    assert alone.sccs == %{"Client" => ["Client"], "Solo" => ["Solo"]}
    assert {:ok, resolution} = Catena.resolve_name(alone, %{category: :values, spelling: "z"})
    assert resolution.origin == "Solo"

    {:ok, result} =
      Catena.compile_scc([
        JSON.encode!(member_a("Ring1")),
        JSON.encode!(member_b("Ring1"))
      ])

    {:ok, flipped} =
      Catena.compile_scc([
        JSON.encode!(member_b("Ring1")),
        JSON.encode!(member_a("Ring1"))
      ])

    assert result.scc_digest == flipped.scc_digest
    assert result.scc_digest == digest_of(result)

    {:ok, rebuilt} =
      Catena.compile_scc([
        JSON.encode!(member_a("Ring1")),
        JSON.encode!(member_b("Ring1"))
      ])

    assert rebuilt.scc_digest == result.scc_digest

    changed_types = [
      %{
        "name" => "Mark",
        "parameters" => [],
        "constructors" => [
          %{
            "name" => "Retag",
            "fields" => [%{"tag" => "named", "name" => "A.Pair", "arguments" => []}]
          }
        ],
        "derivations" => []
      }
    ]

    {:ok, changed} =
      Catena.compile_scc([
        JSON.encode!(
          member(
            "A",
            @a_types,
            [%{"name" => "Pair", "visibility" => "transparent"}],
            [answer()],
            ["answer"]
          )
        ),
        JSON.encode!(
          member(
            "B",
            changed_types,
            [%{"name" => "Mark", "visibility" => "transparent"}],
            [reply()],
            ["reply"]
          )
        )
      ])

    refute changed.scc_digest == result.scc_digest

    {:ok, degenerate} =
      Catena.compile_scc([
        JSON.encode!(
          member(
            "SoloUnit",
            [
              %{
                "name" => "Note",
                "parameters" => [],
                "constructors" => [%{"name" => "Mk", "fields" => []}],
                "derivations" => []
              }
            ],
            [%{"name" => "Note", "visibility" => "transparent"}],
            [reply()],
            ["reply"]
          )
        )
      ])

    {:ok, _module, _binary, metadata} =
      Catena.compile_json(
        JSON.encode!(
          member(
            "SoloUnit",
            [
              %{
                "name" => "Note",
                "parameters" => [],
                "constructors" => [%{"name" => "Mk", "fields" => []}],
                "derivations" => []
              }
            ],
            [%{"name" => "Note", "visibility" => "transparent"}],
            [reply()],
            ["reply"]
          )
        )
      )

    {:ok, ordinary} = Interface.decode(metadata.interface_binary)
    solo = Enum.find(degenerate.members, &(&1.module == "SoloUnit"))
    assert solo.digest == ordinary.digest
    assert degenerate.scc_digest == digest_of(degenerate)
  end

  @tag obligations: ~w(CY-OBL-007)
  test "dependency inversion compiles without a component" do
    inverted =
      member(
        "Inverted",
        [%{"name" => "Config", "parameters" => [], "constructors" => [], "derivations" => []}],
        [],
        [
          %{
            "name" => "serve",
            "parameters" => ["responder", "config"],
            "signature" => %{
              "forall" => [],
              "type" => %{
                "tag" => "function",
                "parameter" => %{
                  "tag" => "function",
                  "parameter" => %{"tag" => "integer"},
                  "result" => %{"tag" => "integer"},
                  "effect" => []
                },
                "result" => %{
                  "tag" => "function",
                  "parameter" => %{"tag" => "integer"},
                  "result" => %{"tag" => "integer"},
                  "effect" => []
                },
                "effect" => []
              }
            },
            "body" => %{"tag" => "variable", "name" => "config"}
          }
        ],
        ["serve"]
      )

    assert {:ok, _core} = Catena.check_json(JSON.encode!(inverted))
  end

  @tag obligations: ~w(CY-OBL-010 CY-OBL-008)
  test "genuine two- and three-module components compile and execute in both layouts" do
    for layout <- [:uniform, :compact] do
      assert {:ok, result} =
               Catena.compile_scc(
                 [JSON.encode!(member_a("Exec")), JSON.encode!(member_b("Exec"))],
                 layout: layout
               )

      assert length(result.members) == 2

      am = Enum.find(result.members, &(&1.module == "A"))
      bm = Enum.find(result.members, &(&1.module == "B"))
      assert {:module, :A} = :code.load_binary(:A, ~c"c024-a.beam", am.binary)
      assert {:module, :B} = :code.load_binary(:B, ~c"c024-b.beam", bm.binary)

      assert apply(:A, :answer, []) == 42
      assert apply(:B, :reply, []) == 7

      value = apply(:A, :main, [])
      assert cross_module_value?(value, layout)

      unload(:A)
      unload(:B)
    end

    c_types = [
      %{
        "name" => "Cell",
        "parameters" => [],
        "constructors" => [
          %{
            "name" => "Mk",
            "fields" => [%{"tag" => "named", "name" => "RingA.Pair", "arguments" => []}]
          }
        ],
        "derivations" => []
      }
    ]

    b_sees_c = [
      %{
        "name" => "Mark",
        "parameters" => [],
        "constructors" => [
          %{
            "name" => "Tag",
            "fields" => [%{"tag" => "named", "name" => "RingC.Cell", "arguments" => []}]
          }
        ],
        "derivations" => []
      }
    ]

    a_sees_b = [
      %{
        "name" => "Pair",
        "parameters" => [],
        "constructors" => [
          %{"name" => "Base", "fields" => []},
          %{
            "name" => "Wrap",
            "fields" => [%{"tag" => "named", "name" => "RingB.Mark", "arguments" => []}]
          }
        ],
        "derivations" => []
      }
    ]

    ring_a =
      member(
        "RingA",
        a_sees_b,
        [%{"name" => "Pair", "visibility" => "transparent"}],
        [answer()],
        ["answer"]
      )
      |> Map.put("origin", "test://ring3")

    ring_b =
      member("RingB", b_sees_c, [%{"name" => "Mark", "visibility" => "transparent"}], [reply()], [
        "reply"
      ])
      |> Map.put("origin", "test://ring3")

    ring_c =
      member(
        "RingC",
        c_types,
        [%{"name" => "Cell", "visibility" => "transparent"}],
        [
          %{
            "name" => "hold",
            "parameters" => [],
            "signature" => %{
              "forall" => [],
              "type" => %{"tag" => "named", "name" => "RingA.Pair", "arguments" => []}
            },
            "body" => %{
              "tag" => "construct",
              "constructor" => "RingA.Pair.Base",
              "arguments" => []
            }
          }
        ],
        ["hold"]
      )
      |> Map.put("origin", "test://ring3")

    assert {:ok, three} =
             Catena.compile_scc([
               JSON.encode!(ring_a),
               JSON.encode!(ring_b),
               JSON.encode!(ring_c)
             ])

    assert length(three.members) == 3
    assert three.scc_digest == digest_of(three)
  end

  @tag obligations: ~w(CY-OBL-004 CY-OBL-009)
  test "outsiders import component members through digest-bound interfaces" do
    {:ok, result} =
      Catena.compile_scc([JSON.encode!(member_a("Share")), JSON.encode!(member_b("Share"))])

    am = Enum.find(result.members, &(&1.module == "A"))

    outsider =
      member(
        "Outsider",
        [],
        [],
        [
          %{
            "name" => "check",
            "parameters" => [],
            "signature" => %{
              "forall" => [],
              "type" => %{"tag" => "named", "name" => "A.Pair", "arguments" => []}
            },
            "body" => %{"tag" => "construct", "constructor" => "A.Pair.Base", "arguments" => []}
          }
        ],
        ["check"]
      )

    assert {:ok, :Outsider, _binary, _metadata} =
             Catena.compile_json(JSON.encode!(outsider), interfaces: [am.interface])

    assert {:error, %{id: "A004"}} =
             Catena.check_json(JSON.encode!(outsider), interfaces: [])
  end

  defp member_a(tag) do
    member(
      "A",
      @a_types,
      [%{"name" => "Pair", "visibility" => "transparent"}],
      [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{
            "forall" => [],
            "type" => %{"tag" => "named", "name" => "B.Mark", "arguments" => []}
          },
          "body" => %{
            "tag" => "construct",
            "constructor" => "B.Mark.Tag",
            "arguments" => [
              %{"tag" => "construct", "constructor" => "Pair.Base", "arguments" => []}
            ]
          }
        },
        answer()
      ],
      ["main", "answer"]
    )
    |> Map.put("origin", "test://scc-#{tag}")
  end

  defp member_b(tag) do
    member("B", @b_types, [%{"name" => "Mark", "visibility" => "transparent"}], [reply()], [
      "reply"
    ])
    |> Map.put("origin", "test://scc-#{tag}")
  end

  defp member(name, [], type_exports, definitions, exports),
    do: member(name, :no_types, type_exports, definitions, exports)

  defp member(name, types, type_exports, definitions, exports) do
    groups = if types == :no_types, do: [], else: [%{"declarations" => types}]

    %{
      "version" => "0.1.2",
      "origin" => "test://scc",
      "module" => name,
      "type_groups" => groups,
      "type_exports" => type_exports,
      "imports" => [],
      "exports" => exports,
      "definitions" => definitions
    }
  end

  defp answer do
    %{
      "name" => "answer",
      "parameters" => [],
      "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
      "body" => %{"tag" => "integer", "value" => 42}
    }
  end

  defp reply do
    %{
      "name" => "reply",
      "parameters" => [],
      "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
      "body" => %{"tag" => "integer", "value" => 7}
    }
  end

  defp cross_module_value?(value, :uniform) do
    is_tuple(value) and elem(value, 0) == :catena_adt
  end

  defp cross_module_value?(value, :compact) do
    is_tuple(value) and is_atom(elem(value, 0))
  end

  defp digest_of(result) do
    result.members
    |> Enum.map(&"#{&1.module}:#{&1.digest}")
    |> Enum.sort()
    |> Enum.join("\n")
    |> then(&:crypto.hash(:sha256, &1))
    |> Base.encode16(case: :lower)
  end

  defp selection(revision),
    do: %{edition: "0.1", language_revision: revision, previews: []}

  defp unload(module) do
    :code.purge(module)
    :code.delete(module)
  end
end
