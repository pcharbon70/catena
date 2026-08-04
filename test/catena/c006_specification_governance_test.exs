defmodule Catena.C006SpecificationGovernanceTest do
  use ExUnit.Case, async: false

  alias Catena.Governance.{Crypto, Lifecycle, Policy, Reference, TrustRoot}
  alias Catena.Package.Linker
  alias Catena.{Assurance, CanonicalJCS, Governance}

  @rfc8032_public "d75a980182b10ab7d54bfed3c964073a0ee172f3daa62325af021a68f707511a"
  @rfc8032_empty_signature "e5564300c360ac729086e2cc806e828a84877f1eb8e5d974d873e06522490155" <>
                             "5fb8821590a33bacc61e39701cf9b46bd25bf5f0595bbe24655141438e7a100b"

  test "Catena's JCS profile is deterministic and rejects ambiguous signed JSON" do
    value = %{"z" => [3, %{"b" => true, "a" => nil}], "a" => "€"}
    assert CanonicalJCS.encode(value) == ~s({"a":"€","z":[3,{"a":null,"b":true}]})
    assert {:ok, ^value} = CanonicalJCS.decode(CanonicalJCS.encode(value), canonical: true)

    assert {:error, %{id: "EVD001", message: duplicate}} =
             CanonicalJCS.decode(~s({"a":1,"a":2}))

    assert duplicate =~ "duplicate object name"
    assert {:error, %{id: "EVD001"}} = CanonicalJCS.decode(~s({"number":1.5}))
    assert {:error, %{id: "EVD001"}} = CanonicalJCS.decode(~s({"number":9007199254740992}))
    assert {:error, %{id: "EVD001"}} = CanonicalJCS.decode(~s({"number":-0}))
    assert {:ok, %{"text" => "-0"}} = CanonicalJCS.decode(~s({"text":"-0"}))
    assert {:error, %{id: "EVD001"}} = CanonicalJCS.decode(<<"{\"text\":\"", 255, "\"}">>)
    assert {:error, %{id: "EVD001"}} = CanonicalJCS.decode(~s({ "a":1}), canonical: true)
  end

  test "OTP 29 Ed25519 verification agrees with the RFC 8032 empty-message vector" do
    assert Crypto.verify(<<>>, @rfc8032_public, @rfc8032_empty_signature)
    refute Crypto.verify("changed", @rfc8032_public, @rfc8032_empty_signature)
  end

  test "AST 0.1.6 type-checks exact rules, exports claim summaries, and erases checkers" do
    json = specification_module_json("C006Rule", true)

    assert {:ok, core} = Catena.check_json(json)

    assert [%{kind: "rule", examples: [%{"outcome" => "supported"}]} = claim] =
             core.specifications.claims

    assert String.starts_with?(claim.id, "claim:sha256:")
    assert byte_size(claim.semantic_digest) == 64

    assert {:ok, :C006Rule, binary, metadata} = Catena.compile_json(json)

    refute Enum.any?(metadata.forms, fn
             {:function, _, :positive, _, _} -> true
             _ -> false
           end)

    assert {:ok, interface} = Catena.Interface.decode(metadata.interface_binary)
    assert [summary] = interface.claims
    assert summary["id"] == claim.id
    refute Enum.any?(interface.values, &(&1.name == "positive"))

    forged_interface =
      metadata.interface
      |> put_in(["claims", Access.at(0), "subject", "kind"], "future-subject")
      |> refresh_interface_digest()

    assert {:error, %{id: "A005"}} =
             forged_interface |> Catena.Interface.encode() |> Catena.Interface.decode()

    assert {:module, :C006Rule} = :code.load_binary(:C006Rule, ~c"C006Rule.beam", binary)
    assert apply(:C006Rule, :main, []) == 7
    refute function_exported?(:C006Rule, :positive, 1)
    :code.purge(:C006Rule)
    :code.delete(:C006Rule)
  end

  test "fully discharged specifications do not change emitted BEAM bytes" do
    plain = base_module("C006Erasure", [])
    specified = specification_module("C006Erasure", true)

    assert {:ok, :C006Erasure, plain_beam, _} = Catena.compile_json(JSON.encode!(plain))
    assert {:ok, :C006Erasure, specified_beam, _} = Catena.compile_json(JSON.encode!(specified))
    assert plain_beam == specified_beam
  end

  test "runtime references to verification-only definitions fail before lowering" do
    module = specification_module("C006Escape", true)

    runtime = %{
      "name" => "runtime_check",
      "parameters" => [],
      "signature" => forall([], boolean_type()),
      "body" => call(variable("positive"), [integer(1)])
    }

    module = %{
      module
      | "exports" => ["main", "runtime_check"],
        "definitions" => module["definitions"] ++ [runtime]
    }

    assert {:error, %{id: "ERS001"}} = Catena.check_json(JSON.encode!(module))
  end

  test "claim subject and example failures keep stable diagnostic families" do
    unknown =
      specification_module("C006Unknown", true)
      |> put_in(["specifications", Access.at(0), "claims", Access.at(0), "subject"], %{
        "kind" => "future-subject",
        "name" => "thing"
      })

    assert {:error, %{id: "SPC001"}} = Catena.check_json(JSON.encode!(unknown))

    counterexample = specification_module_json("C006Counterexample", false)
    assert {:error, %{id: "EVD002"}} = Catena.check_json(counterexample)
  end

  test "all 0.1.6 claim subject kinds resolve against the typed module and package graph" do
    subjects = [
      %{"kind" => "value", "name" => "main"},
      %{"kind" => "datatype", "name" => "Thing"},
      %{"kind" => "trait", "name" => "Friendly"},
      %{"kind" => "instance", "name" => "friendly-thing"},
      %{"kind" => "effect", "name" => "Audit"},
      %{"kind" => "handler", "name" => "AuditHandler"},
      %{"kind" => "module", "name" => "C006Subjects"},
      %{"kind" => "output", "name" => "program.beam"},
      %{"kind" => "interface", "name" => "module.cati.json"},
      %{"kind" => "action", "name" => "publish"},
      %{"kind" => "profile", "name" => "static"}
    ]

    module = specification_module("C006Subjects", true)

    claims =
      subjects
      |> Enum.with_index()
      |> Enum.map(fn {subject, index} ->
        module["specifications"]
        |> hd()
        |> Map.fetch!("claims")
        |> hd()
        |> Map.put("name", "subject_#{index}")
        |> Map.put("subject", subject)
      end)

    module = put_in(module, ["specifications", Access.at(0), "claims"], claims)
    assert {:ok, ast} = Catena.AST.Decoder.decode(JSON.encode!(module))
    core = Catena.Type.Infer.module(ast, []) |> Map.put(:source, ast.source)

    core =
      core
      |> put_in([:data, :types], [%{name: "Thing", visibility: :transparent}])
      |> put_in([:categorical, :traits], [%{name: "Friendly"}])
      |> put_in([:categorical, :instances], [%{id: "friendly-thing"}])
      |> put_in([:effects, :families], %{"audit" => %{name: "Audit"}})
      |> put_in([:effects, :handlers], %{"audit-handler" => %{name: "AuditHandler"}})

    assert %{claims: resolved} = Catena.Specification.elaborate!(ast, core)
    assert Enum.map(resolved, & &1.subject) == subjects
  end

  test "semantic claim digests ignore JSON formatting but change with meaning" do
    module = specification_module("C006Digest", true)
    compact = JSON.encode!(module)
    formatted = "\n  " <> compact <> "\n"

    assert {:ok, compact_core} = Catena.check_json(compact)
    assert {:ok, formatted_core} = Catena.check_json(formatted)
    assert compact_core.specifications.digest == formatted_core.specifications.digest

    changed =
      put_in(module, ["definitions", Access.at(1), "body", "right", "value"], -1)

    assert {:ok, changed_core} = Catena.check_json(JSON.encode!(changed))
    refute compact_core.specifications.digest == changed_core.specifications.digest
  end

  test "mistyped, effectful, failing, and over-budget rule checkers remain distinct" do
    module = specification_module("C006CheckerBoundary", true)
    assert {:ok, ast} = Catena.AST.Decoder.decode(JSON.encode!(module))
    core = Catena.Type.Infer.module(ast, []) |> Map.put(:source, ast.source)
    checker_index = Enum.find_index(core.definitions, &(&1.name == "positive"))

    mistyped =
      update_in(core, [:definitions, Access.at(checker_index)], fn definition ->
        %{definition | scheme: %{definition.scheme | type: {:function, :integer, :integer}}}
      end)

    error =
      assert_raise Catena.TypeError, fn -> Catena.Specification.elaborate!(ast, mistyped) end

    assert error.diagnostic.id == "SPC003"

    effect = %{
      family: "test://Audit",
      family_name: "Audit",
      arguments: [],
      capability: "audit"
    }

    effectful =
      update_in(core, [:definitions, Access.at(checker_index)], fn definition ->
        %{definition | effect_row: Catena.Effect.Row.new([effect])}
      end)

    error =
      assert_raise Catena.TypeError, fn -> Catena.Specification.elaborate!(ast, effectful) end

    assert error.diagnostic.id == "SPC003"

    failing =
      update_in(core, [:definitions, Access.at(checker_index)], fn definition ->
        %{definition | expression: %{tag: :variable, name: "missing_checker"}}
      end)

    error = assert_raise Catena.TypeError, fn -> Catena.Specification.elaborate!(ast, failing) end
    assert error.diagnostic.id == "EVD002"

    looping =
      update_in(core, [:definitions, Access.at(checker_index)], fn definition ->
        %{definition | expression: %{tag: :variable, name: "positive"}}
      end)

    error = assert_raise Catena.TypeError, fn -> Catena.Specification.elaborate!(ast, looping) end
    assert error.diagnostic.id == "EVD003"
  end

  test "verification definitions cannot become runtime exports" do
    module = specification_module("C006Export", true)
    module = %{module | "exports" => ["main", "positive"]}
    assert {:error, %{id: "SPC003"}} = Catena.check_json(JSON.encode!(module))
  end

  test "the rule evaluator reports deterministic budget exhaustion separately" do
    core = %{
      frontend_version: "0.1.6",
      definitions: [
        %{
          name: "loop",
          expression: %{tag: :variable, name: "loop"},
          parameters: []
        }
      ]
    }

    assert {:budget_exhausted, 37} =
             Catena.Reference.Evaluator.run_bounded(core, "loop", [], 37)
  end

  test "production policy evaluation agrees with the independent oracle" do
    requirements = %{
      "op" => "all",
      "requirements" => [
        %{"op" => "action", "allowed" => ["build"]},
        %{
          "op" => "threshold",
          "minimum" => 1,
          "requirements" => [
            %{"op" => "evidence", "kind" => "conformance", "minimum" => 1},
            %{"op" => "deny", "reason" => "not selected"}
          ]
        },
        %{"op" => "state", "allowed" => ["Draft"]},
        %{"op" => "profile", "name" => "static"},
        %{"op" => "sequence", "from" => 1, "to" => 4}
      ]
    }

    context = %{
      action: "build",
      state: "Draft",
      profile: "static",
      sequence: 1,
      root: nil,
      approvals: [],
      approval_payload: %{},
      evidence: [
        %{
          "id" => "compiler:one",
          "kind" => "conformance",
          "claim_id" => "claim:one",
          "result" => "typed_and_pure"
        }
      ]
    }

    assert {:ok, true, explanation, _steps} = Policy.evaluate(requirements, context)

    policies = [%{"id" => "base", "requirement" => requirements}]
    assert {:ok, true, [%{"requirement" => oracle}]} = Reference.decide(policies, context)
    assert explanation == oracle
  end

  test "governance combines every matching policy additively and fails closed" do
    claim_digest = String.duplicate("a", 64)
    subject = %{"kind" => "value", "name" => "main"}

    compiler_evidence = [
      %{
        "id" => "compiler:" <> claim_digest,
        "kind" => "conformance",
        "claim_id" => "claim:one",
        "claim_digest" => claim_digest,
        "subject" => subject,
        "producer" => "catena-compiler",
        "tool" => "catena-test",
        "artifact_digests" => [],
        "result" => "typed_and_pure"
      }
    ]

    bundle =
      bundle(%{
        "policies" => [
          policy("package", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "action",
            "allowed" => ["build"]
          }),
          policy("profile", %{"kind" => "profile", "name" => "static"}, %{
            "op" => "evidence",
            "kind" => "conformance",
            "minimum" => 1
          })
        ]
      })

    assert {:ok, decoded} = bundle |> CanonicalJCS.encode() |> Governance.decode_bundle()

    context = %{
      action: "build",
      package: "demo",
      profile: "static",
      modules: [],
      subjects: [],
      compiler_evidence: compiler_evidence,
      claims: [
        %{
          "id" => "claim:one",
          "semantic_digest" => claim_digest,
          "subject" => subject,
          "examples" => []
        }
      ],
      claim_digests: [claim_digest],
      artifact_digests: []
    }

    assert {:ok, %{decision: "allow", explanations: [_, _]} = production} =
             Governance.evaluate(decoded, nil, context)

    assert {:ok, oracle} = Reference.evaluate(decoded, nil, context)
    assert oracle.decision == production.decision
    assert oracle.state == production.state
    assert oracle.explanations == production.explanations

    denied =
      put_in(bundle, ["policies", Access.at(1), "requirement"], %{
        "op" => "deny",
        "reason" => "blocked"
      })

    assert {:ok, decoded_denied} = denied |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:error, %{id: "GOV001"}} = Governance.evaluate(decoded_denied, nil, context)
  end

  test "the 20000-step policy budget is shared across every matching policy" do
    leaves = List.duplicate(%{"op" => "action", "allowed" => ["build"]}, 10_000)

    value =
      bundle(%{
        "policies" => [
          policy("first", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "all",
            "requirements" => leaves
          }),
          policy("second", %{"kind" => "profile", "name" => "static"}, %{
            "op" => "all",
            "requirements" => leaves
          })
        ]
      })

    assert {:ok, decoded} = value |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:error, %{id: "GOV002"}} = Governance.evaluate(decoded, nil, governance_context())
  end

  test "package, module, subject, action, output, interface, and profile scopes add" do
    scope_context = %{
      governance_context()
      | modules: ["Demo"],
        subjects: [
          %{"kind" => "value", "name" => "main"},
          %{"kind" => "output", "name" => "demo.beam"},
          %{"kind" => "interface", "name" => "demo.cati.json"}
        ]
    }

    scopes = [
      %{"kind" => "package", "name" => "demo"},
      %{"kind" => "module", "name" => "Demo"},
      %{"kind" => "subject", "name" => "main"},
      %{"kind" => "action", "name" => "build"},
      %{"kind" => "output", "name" => "demo.beam"},
      %{"kind" => "interface", "name" => "demo.cati.json"},
      %{"kind" => "profile", "name" => "static"}
    ]

    value =
      bundle(%{
        "policies" =>
          scopes
          |> Enum.with_index()
          |> Enum.map(fn {scope, index} ->
            policy("scope-#{index}", scope, %{"op" => "action", "allowed" => ["build"]})
          end)
      })

    assert {:ok, decoded} = value |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:ok, %{explanations: explanations}} = Governance.evaluate(decoded, nil, scope_context)
    assert length(explanations) == 7
  end

  test "trust roots count distinct Ed25519 principals and require old plus new rotation authority" do
    old = keypair("old")
    recovery = keypair("recovery")
    new = keypair("new")
    next_recovery = keypair("next-recovery")

    initial = root_state(1, [old, recovery], [old.id], [recovery.id])
    initial_root = decoded_root("demo", initial)
    next = root_state(2, [new, next_recovery], [new.id], [next_recovery.id])

    event_payload = %{
      "mode" => "normal",
      "sequence" => 2,
      "prior_digest" => initial_root.digest,
      "root" => next
    }

    event =
      event_payload
      |> Map.put("digest", CanonicalJCS.digest(event_payload))
      |> Map.put("signatures", [signature(old, "root", event_payload)])
      |> Map.put("new_signatures", [signature(new, "root", event_payload)])

    document = %{
      "format" => "catena-trust-root",
      "version" => "0.1.6",
      "namespace" => "demo",
      "initial" => initial,
      "history" => [event]
    }

    assert {:ok, %{sequence: 2, digest: digest} = rotated} =
             document |> CanonicalJCS.encode() |> TrustRoot.decode()

    assert digest == CanonicalJCS.digest(next)

    proposed =
      transition("Draft", "Proposed", 1, String.duplicate("0", 64))
      |> sign_transition(old)

    accepted =
      transition("Proposed", "Accepted", 2, proposed["digest"])
      |> sign_transition(new)

    assert {:ok, %{state: "Accepted"}} = Lifecycle.replay([proposed, accepted], rotated)

    missing_new = put_in(document, ["history", Access.at(0), "new_signatures"], [])

    assert {:error, %{id: "GOV005"}} =
             missing_new |> CanonicalJCS.encode() |> TrustRoot.decode()
  end

  test "signature thresholds reject duplicate actors and cross-domain substitution" do
    first = keypair("first")
    second = keypair("second")
    recovery = keypair("recovery")
    state = root_state(1, [first, second, recovery], [first.id, second.id], [recovery.id])
    root = decoded_root("demo", state)
    payload = %{"release" => "one"}
    first_signature = signature(first, "manifest", payload)
    second_signature = signature(second, "manifest", payload)

    assert {:error, _reason} =
             Crypto.verify_threshold(
               root,
               "normal",
               "manifest",
               payload,
               [first_signature, first_signature],
               1
             )

    assert {:ok, %{valid: ["first"], duplicate: 1, invalid: 0, revoked: 0}} =
             Crypto.signer_audit(
               root,
               "normal",
               "manifest",
               payload,
               [first_signature, first_signature],
               1
             )

    assert {:ok, ["first", "second"]} =
             Crypto.verify_threshold(
               root,
               "normal",
               "manifest",
               payload,
               [first_signature, second_signature],
               1
             )

    approval_signature = signature(first, "approval", payload)

    assert {:error, _reason} =
             Crypto.verify_threshold(
               root,
               "normal",
               "manifest",
               payload,
               [approval_signature, second_signature],
               1
             )

    revoked_state = put_in(state, ["revocations", "principals"], [first.id])
    revoked_root = decoded_root("demo", revoked_state)

    assert {:ok, %{valid: ["second"], revoked: 1}} =
             Crypto.signer_audit(
               revoked_root,
               "normal",
               "manifest",
               payload,
               [first_signature, second_signature],
               1
             )
  end

  test "predeclared recovery can replace normal authority without new-root self-authorization" do
    old = keypair("old")
    recovery = keypair("recovery")
    replacement = keypair("replacement")
    later_recovery = keypair("later-recovery")
    initial = root_state(1, [old, recovery], [old.id], [recovery.id])
    initial_root = decoded_root("demo", initial)
    next = root_state(2, [replacement, later_recovery], [replacement.id], [later_recovery.id])

    payload = %{
      "mode" => "recovery",
      "sequence" => 2,
      "prior_digest" => initial_root.digest,
      "root" => next
    }

    event =
      payload
      |> Map.put("digest", CanonicalJCS.digest(payload))
      |> Map.put("signatures", [signature(recovery, "root", payload)])
      |> Map.put("new_signatures", [])

    document = %{
      "format" => "catena-trust-root",
      "version" => "0.1.6",
      "namespace" => "demo",
      "initial" => initial,
      "history" => [event]
    }

    assert {:ok, %{sequence: 2}} = document |> CanonicalJCS.encode() |> TrustRoot.decode()

    self_authorized = put_in(event, ["signatures"], [signature(later_recovery, "root", payload)])
    invalid = %{document | "history" => [self_authorized]}
    assert {:error, %{id: "GOV005"}} = invalid |> CanonicalJCS.encode() |> TrustRoot.decode()
  end

  test "delegated signatures remain bounded by action, subject, profile, and sequence" do
    owner = keypair("owner")
    recovery = keypair("recovery")
    delegate = keypair("delegate")

    state = root_state(1, [owner, recovery, delegate], [owner.id], [recovery.id])
    state = put_in(state, ["roles", "reviewer"], %{"principals" => [owner.id], "threshold" => 1})

    state =
      Map.put(state, "delegations", [
        %{
          "id" => "build-reviewer",
          "principal" => delegate.id,
          "role" => "reviewer",
          "from" => 1,
          "to" => 2,
          "actions" => ["build"],
          "subjects" => ["demo"],
          "profiles" => ["static"]
        }
      ])

    root = decoded_root("demo", state)
    payload = %{"decision" => "build"}
    signed = [signature(delegate, "approval", payload)]

    assert {:ok, ["delegate"]} =
             Crypto.valid_signers(root, "reviewer", "approval", payload, signed, 1, %{
               action: "build",
               subject: "demo",
               profile: "static"
             })

    assert {:ok, []} =
             Crypto.valid_signers(root, "reviewer", "approval", payload, signed, 1, %{
               action: "publish",
               subject: "demo",
               profile: "static"
             })

    policies = [
      policy("delegated", %{"kind" => "package", "name" => "demo"}, %{
        "op" => "role",
        "role" => "reviewer",
        "minimum" => 1
      })
    ]

    decision = approval_decision(policies, governance_context(), 1)
    approval_payload = %{"id" => "approval:delegated", "decision" => decision}

    approval = %{
      "id" => "approval:delegated",
      "payload" => approval_payload,
      "signatures" => [signature(delegate, "approval", approval_payload)]
    }

    value = bundle(%{"policies" => policies, "approvals" => [approval]})
    assert {:ok, decoded} = value |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:ok, %{decision: "allow"}} = Governance.evaluate(decoded, root, governance_context())
  end

  test "assumptions count only when policy names them and an authorized role signs the exact decision" do
    assumer = keypair("assumer")
    recovery = keypair("recovery")
    state = root_state(1, [assumer, recovery], [assumer.id], [recovery.id])
    state = put_in(state, ["roles", "assumer"], %{"principals" => [assumer.id], "threshold" => 1})
    root = decoded_root("demo", state)
    claim_digest = String.duplicate("c", 64)
    subject = %{"kind" => "value", "name" => "main"}

    requirement = %{
      "op" => "all",
      "requirements" => [
        %{
          "op" => "evidence",
          "kind" => "assumption",
          "claim_id" => "claim:one",
          "minimum" => 1
        },
        %{"op" => "role", "role" => "assumer", "minimum" => 1}
      ]
    }

    policies = [policy("assumption", %{"kind" => "package", "name" => "demo"}, requirement)]

    assumption = %{
      "id" => "assumption:one",
      "kind" => "assumption",
      "claim_id" => "claim:one",
      "claim_digest" => claim_digest,
      "subject" => subject,
      "result" => "assumed",
      "reason" => "external dependency",
      "sequence" => %{"from" => 1, "to" => 1}
    }

    decision = %{
      "action" => "build",
      "package" => "demo",
      "profile" => "static",
      "subject" => %{"kind" => "package", "name" => "demo"},
      "from" => "Draft",
      "to" => "Draft",
      "sequence" => 1,
      "prior_transition_digest" => String.duplicate("0", 64),
      "policy_digest" => CanonicalJCS.digest(policies),
      "claim_digests" => [claim_digest],
      "artifact_digests" => [],
      "evidence" => [
        %{"id" => "assumption:one", "digest" => CanonicalJCS.digest(assumption)}
      ]
    }

    approval_payload = %{"id" => "approval:one", "decision" => decision}

    approval = %{
      "id" => "approval:one",
      "payload" => approval_payload,
      "signatures" => [signature(assumer, "approval", approval_payload)]
    }

    value =
      bundle(%{
        "policies" => policies,
        "evidence" => [assumption],
        "approvals" => [approval]
      })

    context =
      governance_context()
      |> Map.put(:claims, [
        %{
          "id" => "claim:one",
          "semantic_digest" => claim_digest,
          "subject" => subject,
          "examples" => []
        }
      ])
      |> Map.put(:claim_digests, [claim_digest])

    assert {:ok, decoded} = value |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:ok, %{decision: "allow"}} = Governance.evaluate(decoded, root, context)
    assert {:ok, %{decision: "allow"}} = Reference.evaluate(decoded, root, context)

    generic =
      put_in(value, ["policies", Access.at(0), "requirement", "requirements", Access.at(0)], %{
        "op" => "evidence",
        "minimum" => 1
      })

    assert {:ok, generic_decoded} = generic |> CanonicalJCS.encode() |> Governance.decode_bundle()

    assert {:error, %{id: "EVD001"}} =
             Governance.evaluate(generic_decoded, root, context)

    disjunctive =
      put_in(value, ["policies", Access.at(0), "requirement", "op"], "any")

    assert {:ok, disjunctive_decoded} =
             disjunctive |> CanonicalJCS.encode() |> Governance.decode_bundle()

    assert {:error, %{id: "EVD001"}} =
             Governance.evaluate(disjunctive_decoded, root, context)
  end

  test "external attestations are signed, sequence-bounded, and claim-bound" do
    attestor = keypair("attestor")
    recovery = keypair("recovery")
    state = root_state(1, [attestor, recovery], [attestor.id], [recovery.id])

    state =
      put_in(state, ["roles", "attestor"], %{"principals" => [attestor.id], "threshold" => 1})

    root = decoded_root("demo", state)
    claim_digest = String.duplicate("a", 64)
    artifact_digest = String.duplicate("b", 64)
    subject = %{"kind" => "value", "name" => "main"}
    window = %{"from" => 1, "to" => 1}

    payload = %{
      "id" => "attestation:one",
      "kind" => "attestation",
      "claim_id" => "claim:one",
      "claim_digest" => claim_digest,
      "subject" => subject,
      "artifact_digests" => [artifact_digest],
      "result" => "accepted",
      "tool" => "external:test",
      "producer_role" => "attestor",
      "sequence" => window
    }

    attestation = %{
      "id" => "attestation:one",
      "kind" => "attestation",
      "producer_role" => "attestor",
      "sequence" => window,
      "payload" => payload,
      "signatures" => [signature(attestor, "evidence", payload)]
    }

    value =
      bundle(%{
        "policies" => [
          policy("attested", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "evidence",
            "kind" => "attestation",
            "claim_id" => "claim:one",
            "minimum" => 1
          })
        ],
        "evidence" => [attestation]
      })

    assert {:ok, decoded} = value |> CanonicalJCS.encode() |> Governance.decode_bundle()

    context =
      governance_context()
      |> Map.put(:claims, [
        %{
          "id" => "claim:one",
          "semantic_digest" => claim_digest,
          "subject" => subject,
          "examples" => []
        }
      ])
      |> Map.put(:claim_digests, [claim_digest])
      |> Map.put(:artifact_digests, [artifact_digest])

    assert {:ok, %{decision: "allow"} = production} = Governance.evaluate(decoded, root, context)
    assert {:ok, oracle} = Reference.evaluate(decoded, root, context)
    assert oracle.explanations == production.explanations

    substituted = put_in(value, ["evidence", Access.at(0), "payload", "claim_id"], "claim:two")
    assert {:ok, bad} = substituted |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:error, %{id: "EVD001"}} = Governance.evaluate(bad, root, context)

    stale_window = %{"from" => 2, "to" => 2}

    stale_payload =
      payload
      |> Map.put("sequence", stale_window)

    stale =
      attestation
      |> Map.put("sequence", stale_window)
      |> Map.put("payload", stale_payload)
      |> Map.put("signatures", [signature(attestor, "evidence", stale_payload)])

    stale_value = put_in(value, ["evidence", Access.at(0)], stale)

    assert {:ok, stale_decoded} =
             stale_value |> CanonicalJCS.encode() |> Governance.decode_bundle()

    assert {:error, %{id: "EVD001"}} = Governance.evaluate(stale_decoded, root, context)

    revoked_state = put_in(state, ["revocations", "evidence"], ["attestation:one"])
    revoked_root = decoded_root("demo", revoked_state)
    assert {:error, %{id: "EVD001"}} = Governance.evaluate(decoded, revoked_root, context)
  end

  test "lifecycle replay rejects skipped, terminal, and broken hash-chain transitions" do
    signer = keypair("signer")
    recovery = keypair("recovery")
    root = decoded_root("demo", root_state(1, [signer, recovery], [signer.id], [recovery.id]))

    unsigned = transition("Draft", "Proposed", 1, String.duplicate("0", 64))
    assert {:error, %{id: "GOV004"}} = Lifecycle.replay([unsigned], nil)

    event = sign_transition(unsigned, signer)
    assert {:ok, %{state: "Proposed", sequence: 1}} = Lifecycle.replay([event], root)

    skipped =
      transition("Draft", "Active", 1, String.duplicate("0", 64))
      |> sign_transition(signer)

    assert {:error, %{id: "GOV004"}} = Lifecycle.replay([skipped], root)

    rejected =
      transition("Proposed", "Rejected", 2, event["digest"])
      |> sign_transition(signer)

    assert {:ok, %{state: "Rejected"}} = Lifecycle.replay([event, rejected], root)

    terminal =
      transition("Rejected", "Proposed", 3, rejected["digest"])
      |> sign_transition(signer)

    assert {:error, %{id: "GOV004"}} = Lifecycle.replay([event, rejected, terminal], root)

    broken = %{rejected | "prior_digest" => String.duplicate("f", 64)}
    assert {:error, %{id: "GOV004"}} = Lifecycle.replay([event, broken], root)
  end

  test "activate requires a signed lifecycle transition into Active" do
    signer = keypair("signer")
    recovery = keypair("recovery")
    root = decoded_root("demo", root_state(1, [signer, recovery], [signer.id], [recovery.id]))

    value =
      bundle(%{
        "policies" => [
          policy("activate", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "action",
            "allowed" => ["activate"]
          })
        ]
      })

    assert {:ok, draft} = value |> CanonicalJCS.encode() |> Governance.decode_bundle()
    context = %{governance_context() | action: "activate"}
    assert {:error, %{id: "GOV001"}} = Governance.evaluate(draft, root, context)

    proposed =
      transition("Draft", "Proposed", 1, String.duplicate("0", 64))
      |> sign_transition(signer)

    accepted =
      transition("Proposed", "Accepted", 2, proposed["digest"])
      |> sign_transition(signer)

    policy_digest = CanonicalJCS.digest(value["policies"])

    explanations = [
      %{
        "policy" => "activate",
        "decision" => true,
        "requirement" => %{
          "op" => "action",
          "decision" => true,
          "allowed" => ["activate"]
        }
      }
    ]

    active =
      transition("Accepted", "Active", 3, accepted["digest"], %{
        "policy_digest" => policy_digest,
        "explanation" => %{"policies" => explanations}
      })
      |> sign_transition(signer)

    active_value = Map.put(value, "transitions", [proposed, accepted, active])
    assert {:ok, decoded} = active_value |> CanonicalJCS.encode() |> Governance.decode_bundle()
    assert {:ok, %{state: "Active"}} = Governance.evaluate(decoded, root, context)
  end

  test "lifecycle replay covers every valid edge and rejects reordering" do
    signer = keypair("signer")
    recovery = keypair("recovery")
    root = decoded_root("demo", root_state(1, [signer, recovery], [signer.id], [recovery.id]))
    zero = String.duplicate("0", 64)

    proposed = transition("Draft", "Proposed", 1, zero) |> sign_transition(signer)

    accepted =
      transition("Proposed", "Accepted", 2, proposed["digest"]) |> sign_transition(signer)

    active = transition("Accepted", "Active", 3, accepted["digest"]) |> sign_transition(signer)

    deprecated =
      transition("Active", "Deprecated", 4, active["digest"]) |> sign_transition(signer)

    superseded =
      transition("Deprecated", "Superseded", 5, deprecated["digest"])
      |> sign_transition(signer)

    assert {:ok, %{state: "Superseded"}} =
             Lifecycle.replay([proposed, accepted, active, deprecated, superseded], root)

    accepted_withdrawn =
      transition("Accepted", "Withdrawn", 3, accepted["digest"])
      |> sign_transition(signer)

    assert {:ok, %{state: "Withdrawn"}} =
             Lifecycle.replay([proposed, accepted, accepted_withdrawn], root)

    proposed_withdrawn =
      transition("Proposed", "Withdrawn", 2, proposed["digest"])
      |> sign_transition(signer)

    assert {:ok, %{state: "Withdrawn"}} =
             Lifecycle.replay([proposed, proposed_withdrawn], root)

    rejected =
      transition("Proposed", "Rejected", 2, proposed["digest"]) |> sign_transition(signer)

    assert {:ok, %{state: "Rejected"}} = Lifecycle.replay([proposed, rejected], root)
    assert {:error, %{id: "GOV004"}} = Lifecycle.replay([accepted, proposed], root)
  end

  test "0.1.6 package build stages outputs, emits a sidecar, and verifies exact artifacts" do
    directory = temporary_directory!("package")
    module_path = Path.join(directory, "module.json")
    manifest_path = Path.join(directory, "package.json")
    File.write!(module_path, specification_module_json("C006Package", true))

    manifest = package_manifest("demo", "module.json", nil)
    File.write!(manifest_path, JSON.encode!(manifest))

    assert {:ok, result} = Linker.compile_manifest(manifest_path)
    assert File.exists?(result.output)
    assert File.exists?(result.assurance)

    assurance_binary = File.read!(result.assurance)
    assert {:ok, verified} = Assurance.verify(assurance_binary, directory, nil)
    assert verified.package == "demo"
    assert verified.action == "build"

    assert {:ok, assurance_document} = CanonicalJCS.decode(assurance_binary, canonical: true)

    forged =
      put_in(
        assurance_document,
        ["signed", "evidence", Access.at(0), "artifact_digests"],
        []
      )

    assert {:error, %{id: "ART001"}} =
             Assurance.verify(CanonicalJCS.encode(forged), directory, nil)

    beam = Path.join(directory, "C006Package.beam")
    binary = File.read!(beam)
    File.rm!(result.assurance)
    assert {:module, :C006Package} = :code.load_binary(:C006Package, ~c"C006Package.beam", binary)
    assert apply(:C006Package, :main, []) == 7
    :code.purge(:C006Package)
    :code.delete(:C006Package)
  end

  test "fully discharged specifications leave every package BEAM byte-identical" do
    directory = temporary_directory!("package-erasure")
    module_path = Path.join(directory, "module.json")
    manifest_path = Path.join(directory, "package.json")
    manifest = package_manifest("demo", "module.json", nil, "C006PackageErasure")
    File.write!(manifest_path, JSON.encode!(manifest))

    File.write!(module_path, JSON.encode!(base_module("C006PackageErasure", [])))
    assert {:ok, plain} = Linker.compile_manifest(manifest_path)
    plain_module = File.read!(Path.join(directory, "C006PackageErasure.beam"))
    plain_companion = File.read!(plain.output)

    File.write!(module_path, specification_module_json("C006PackageErasure", true))
    assert {:ok, specified} = Linker.compile_manifest(manifest_path)
    specified_module = File.read!(Path.join(directory, "C006PackageErasure.beam"))
    specified_companion = File.read!(specified.output)

    assert plain_module == specified_module
    assert plain_companion == specified_companion
  end

  test "a governed build consumes compiler evidence and emits the external signing payload" do
    directory = temporary_directory!("governed")

    File.write!(
      Path.join(directory, "module.json"),
      specification_module_json("C006Governed", true)
    )

    governance =
      bundle(%{
        "policies" => [
          policy("build", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "all",
            "requirements" => [
              %{"op" => "action", "allowed" => ["build"]},
              %{"op" => "evidence", "kind" => "conformance", "minimum" => 1}
            ]
          })
        ]
      })

    File.write!(Path.join(directory, "governance.json"), CanonicalJCS.encode(governance))
    manifest = package_manifest("demo", "module.json", "governance.json", "C006Governed")
    path = Path.join(directory, "package.json")
    File.write!(path, JSON.encode!(manifest))

    assert {:ok, result} = Linker.compile_manifest(path, action: :build)
    assert result.governance.decision == "allow"

    assert result.signing_payload ==
             File.read!(result.assurance) |> CanonicalJCS.decode() |> signing_payload!()

    assert byte_size(result.signing_payload_digest) == 64

    assert {:ok, %{action: "build"}} =
             Assurance.verify(File.read!(result.assurance), directory, nil)
  end

  test "imported interfaces carry claim obligations and semantic dependency digests" do
    directory = temporary_directory!("inherited-claims")

    assert {:ok, :C006Dependency, _beam, dependency} =
             Catena.compile_json(specification_module_json("C006Dependency", true))

    File.write!(Path.join(directory, "dependency.cati.json"), dependency.interface_binary)

    File.write!(
      Path.join(directory, "consumer.json"),
      JSON.encode!(base_module("C006Consumer", []))
    )

    manifest =
      package_manifest("demo", "consumer.json", nil, "C006Consumer")
      |> Map.put("interfaces", ["dependency.cati.json"])

    path = Path.join(directory, "package.json")
    File.write!(path, JSON.encode!(manifest))

    assert {:ok, result} = Linker.compile_manifest(path)
    assert {:ok, assurance} = CanonicalJCS.decode(File.read!(result.assurance), canonical: true)
    assert [claim] = get_in(assurance, ["signed", "claims"])
    assert claim["subject"] == %{"kind" => "value", "name" => "main"}
    assert length(get_in(assurance, ["signed", "dependency_digests"])) == 1
  end

  test "signed assurance manifests bind the exact payload and artifact" do
    signer = keypair("signer")
    recovery = keypair("recovery")
    root = decoded_root("demo", root_state(1, [signer, recovery], [signer.id], [recovery.id]))
    directory = temporary_directory!("signed-manifest")
    File.write!(Path.join(directory, "program.beam"), "beam-bytes")

    package = %{package: "demo", profile: "static", action: "publish"}
    artifacts = [%{path: "program.beam", kind: "beam", binary: "beam-bytes"}]

    governance_bundle =
      bundle(%{
        "policies" => [
          policy("publish", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "action",
            "allowed" => ["publish"]
          })
        ]
      })

    assert {:ok, decoded} =
             governance_bundle |> CanonicalJCS.encode() |> Governance.decode_bundle()

    artifact_digest = :crypto.hash(:sha256, "beam-bytes") |> Base.encode16(case: :lower)

    context =
      governance_context()
      |> Map.put(:action, "publish")
      |> Map.put(:artifact_digests, [artifact_digest])

    assert {:ok, governance} = Governance.evaluate(decoded, root, context)

    unsigned = Assurance.build(package, artifacts, [], governance)

    signed =
      Assurance.build(package, artifacts, [], governance, [
        signature(signer, "manifest", unsigned.document["signed"])
      ])

    assert {:ok, %{package: "demo", action: "publish"}} =
             Assurance.verify(signed.binary, directory, root)

    wrong_domain =
      Assurance.build(package, artifacts, [], governance, [
        signature(signer, "approval", unsigned.document["signed"])
      ])

    assert {:error, %{id: "ART001"}} = Assurance.verify(wrong_domain.binary, directory, root)
  end

  test "publish exposes an exact candidate payload, writes nothing, then accepts external signing" do
    signer = keypair("signer")
    recovery = keypair("recovery")
    state = root_state(1, [signer, recovery], [signer.id], [recovery.id])
    root = decoded_root("demo", state)
    directory = temporary_directory!("two-pass-signing")
    root_path = Path.join(directory, "trust-root.json")

    File.write!(root_path, CanonicalJCS.encode(root_document("demo", state)))

    File.write!(
      Path.join(directory, "module.json"),
      specification_module_json("C006Publish", true)
    )

    governance =
      bundle(%{
        "policies" => [
          policy("publish", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "action",
            "allowed" => ["publish"]
          })
        ]
      })

    governance_path = Path.join(directory, "governance.json")
    File.write!(governance_path, CanonicalJCS.encode(governance))
    manifest = package_manifest("demo", "module.json", "governance.json", "C006Publish")
    manifest_path = Path.join(directory, "package.json")
    File.write!(manifest_path, JSON.encode!(manifest))

    assert {:error, %{id: "GOV003", details: details}} =
             Linker.compile_manifest(manifest_path,
               action: :publish,
               trust_root: root_path
             )

    assert byte_size(details.signing_payload_digest) == 64
    refute File.exists?(Path.join(directory, "C006Publish.beam"))
    refute File.exists?(Path.join(directory, "assurance.json"))

    signed =
      :crypto.sign(:eddsa, :none, details.signing_payload, [signer.private, :ed25519])

    manifest_signature = %{
      "principal" => signer.id,
      "signature" => Base.encode16(signed, case: :lower)
    }

    governance = Map.put(governance, "manifest_signatures", [manifest_signature])
    File.write!(governance_path, CanonicalJCS.encode(governance))

    assert {:ok, result} =
             Linker.compile_manifest(manifest_path,
               action: :publish,
               trust_root: root_path
             )

    assert File.exists?(result.output)
    assert File.exists?(result.assurance)

    assert {:ok, %{action: "publish"}} =
             Assurance.verify(File.read!(result.assurance), directory, root)
  end

  test "failed governed gates and unsafe paths leave final outputs absent" do
    directory = temporary_directory!("gate")
    File.write!(Path.join(directory, "module.json"), specification_module_json("C006Gate", true))

    governance =
      bundle(%{
        "policies" => [
          policy("deny", %{"kind" => "package", "name" => "demo"}, %{
            "op" => "deny",
            "reason" => "not approved"
          })
        ]
      })

    File.write!(Path.join(directory, "governance.json"), CanonicalJCS.encode(governance))
    manifest = package_manifest("demo", "module.json", "governance.json", "C006Gate")
    path = Path.join(directory, "package.json")
    File.write!(path, JSON.encode!(manifest))

    assert {:error, %{id: "GOV001"}} = Linker.compile_manifest(path, action: :build)
    refute File.exists?(Path.join(directory, "C006Gate.beam"))
    refute File.exists?(Path.join(directory, "assurance.json"))

    unsafe = put_in(manifest, ["modules", Access.at(0), "beam"], "../escape.beam")
    File.write!(path, JSON.encode!(unsafe))
    assert {:error, %{id: "ART001"}} = Linker.compile_manifest(path, action: :build)

    outside = temporary_directory!("outside")
    File.ln_s!(outside, Path.join(directory, "outside-link"))

    symlink_escape =
      put_in(manifest, ["modules", Access.at(0), "beam"], "outside-link/escape.beam")

    File.write!(path, JSON.encode!(symlink_escape))
    assert {:error, %{id: "ART001"}} = Linker.compile_manifest(path, action: :build)

    collision = Map.put(manifest, "assurance", "C006Gate.beam")
    File.write!(path, JSON.encode!(collision))
    assert {:error, %{id: "ART001"}} = Linker.compile_manifest(path, action: :build)

    input_overwrite = Map.put(manifest, "output", "module.json")
    File.write!(path, JSON.encode!(input_overwrite))
    assert {:error, %{id: "ART001"}} = Linker.compile_manifest(path, action: :build)
  end

  test "package-level claim subjects must name declared outputs, interfaces, actions, and profiles" do
    directory = temporary_directory!("package-subject")

    module =
      specification_module("C006PackageSubject", true)
      |> put_in(["specifications", Access.at(0), "claims", Access.at(0), "subject"], %{
        "kind" => "output",
        "name" => "missing.beam"
      })

    File.write!(Path.join(directory, "module.json"), JSON.encode!(module))
    manifest = package_manifest("demo", "module.json", nil, "C006PackageSubject")
    path = Path.join(directory, "package.json")
    File.write!(path, JSON.encode!(manifest))

    assert {:error, %{id: "SPC001"}} = Linker.compile_manifest(path)
    refute File.exists?(Path.join(directory, "C006PackageSubject.beam"))
    refute File.exists?(Path.join(directory, "assurance.json"))
  end

  test "artifact substitution invalidates a previously valid assurance manifest" do
    directory = temporary_directory!("substitution")
    File.write!(Path.join(directory, "module.json"), specification_module_json("C006Bind", true))
    path = Path.join(directory, "package.json")
    File.write!(path, JSON.encode!(package_manifest("demo", "module.json", nil, "C006Bind")))
    assert {:ok, result} = Linker.compile_manifest(path)
    manifest = File.read!(result.assurance)

    File.write!(Path.join(directory, "C006Bind.beam"), "substituted")
    assert {:error, %{id: "ART001"}} = Assurance.verify(manifest, directory, nil)
  end

  test "assurance verification refuses artifact paths that escape through symlinks" do
    directory = temporary_directory!("verify-symlink")
    outside = temporary_directory!("verify-symlink-outside")
    File.write!(Path.join(outside, "program.beam"), "beam-bytes")
    File.ln_s!(outside, Path.join(directory, "outside"))

    assurance =
      Assurance.build(
        %{package: "demo", profile: "static", action: "build"},
        [%{path: "outside/program.beam", kind: "beam", binary: "beam-bytes"}],
        [],
        nil
      )

    assert {:error, %{id: "ART001"}} = Assurance.verify(assurance.binary, directory, nil)
  end

  defp specification_module_json(name, expected),
    do: name |> specification_module(expected) |> JSON.encode!()

  defp specification_module(name, expected) do
    checker = %{
      "name" => "positive",
      "parameters" => ["value"],
      "signature" => forall([], function_type(integer_type(), boolean_type())),
      "verification_only" => true,
      "body" => binary("greater", variable("value"), integer(0))
    }

    claim = %{
      "name" => "main_is_positive",
      "kind" => "rule",
      "subject" => %{"kind" => "value", "name" => "main"},
      "checker" => "positive",
      "examples" => [
        %{"name" => "positive_one", "arguments" => [1], "expected" => expected}
      ]
    }

    base_module(name, [checker])
    |> Map.put("specifications", [%{"name" => "main_contract", "claims" => [claim]}])
  end

  defp base_module(name, extra_definitions) do
    %{
      "version" => "0.1.6",
      "origin" => "pkg://tests/#{name}",
      "module" => name,
      "source" => "module.json",
      "exports" => ["main"],
      "type_exports" => [],
      "type_groups" => [],
      "imports" => [],
      "definitions" =>
        [
          %{
            "name" => "main",
            "parameters" => [],
            "signature" => forall([], integer_type()),
            "body" => integer(7)
          }
        ] ++ extra_definitions,
      "traits" => [],
      "instances" => [],
      "templates" => [],
      "effects" => [],
      "handlers" => []
    }
  end

  defp bundle(overrides) do
    Map.merge(
      %{
        "format" => "catena-governance-bundle",
        "version" => "0.1.6",
        "package" => "demo",
        "profile" => "static",
        "policies" => [],
        "evidence" => [],
        "approvals" => [],
        "transitions" => [],
        "manifest_signatures" => []
      },
      overrides
    )
  end

  defp policy(id, scope, requirement),
    do: %{"id" => id, "scope" => scope, "requirement" => requirement}

  defp governance_context do
    %{
      action: "build",
      package: "demo",
      profile: "static",
      modules: [],
      subjects: [],
      compiler_evidence: [],
      claim_digests: [],
      artifact_digests: []
    }
  end

  defp approval_decision(policies, context, sequence) do
    %{
      "action" => context.action,
      "package" => context.package,
      "profile" => context.profile,
      "subject" => %{"kind" => "package", "name" => context.package},
      "from" => "Draft",
      "to" => "Draft",
      "sequence" => sequence,
      "prior_transition_digest" => String.duplicate("0", 64),
      "policy_digest" => CanonicalJCS.digest(policies),
      "claim_digests" => [],
      "artifact_digests" => [],
      "evidence" => []
    }
  end

  defp keypair(id) do
    {public, private} = :crypto.generate_key(:eddsa, :ed25519)

    %{
      id: id,
      public: Base.encode16(public, case: :lower),
      private: private
    }
  end

  defp signature(key, kind, payload) do
    signed =
      :crypto.sign(:eddsa, :none, CanonicalJCS.payload(kind, payload), [key.private, :ed25519])

    %{"principal" => key.id, "signature" => Base.encode16(signed, case: :lower)}
  end

  defp root_state(sequence, keys, normal, recovery) do
    %{
      "sequence" => sequence,
      "principals" =>
        Enum.map(keys, &%{"id" => &1.id, "public_key" => &1.public}) |> Enum.sort_by(& &1["id"]),
      "roles" => %{
        "normal" => %{"principals" => normal, "threshold" => length(normal)},
        "recovery" => %{"principals" => recovery, "threshold" => length(recovery)}
      },
      "delegations" => [],
      "revocations" => %{"principals" => [], "delegations" => [], "evidence" => []}
    }
  end

  defp decoded_root(namespace, state) do
    document = root_document(namespace, state)

    {:ok, root} = document |> CanonicalJCS.encode() |> TrustRoot.decode()
    root
  end

  defp root_document(namespace, state) do
    %{
      "format" => "catena-trust-root",
      "version" => "0.1.6",
      "namespace" => namespace,
      "initial" => state,
      "history" => []
    }
  end

  defp transition(from, to, sequence, prior_digest, overrides \\ %{}) do
    payload =
      %{
        "sequence" => sequence,
        "prior_digest" => prior_digest,
        "from" => from,
        "to" => to,
        "action" => "activate",
        "subject" => %{"kind" => "package", "name" => "demo"},
        "proposal_digest" => String.duplicate("1", 64),
        "claim_digests" => [],
        "artifact_digests" => [],
        "policy_digest" => String.duplicate("2", 64),
        "evidence" => [],
        "approvals" => [],
        "decision" => "allow",
        "explanation" => %{"policy" => "test"}
      }
      |> Map.merge(overrides)

    payload
    |> Map.put("digest", CanonicalJCS.digest(payload))
    |> Map.put("signatures", [])
  end

  defp sign_transition(event, signer) do
    payload = Map.drop(event, ["digest", "signatures"])
    Map.put(event, "signatures", [signature(signer, "transition", payload)])
  end

  defp package_manifest(package, source, governance, module \\ "C006Package") do
    %{
      "format" => "catena-package-manifest",
      "version" => "0.1.6",
      "package" => package,
      "profile" => "static",
      "companion_module" => "C006Companion",
      "modules" => [
        %{"source" => source, "beam" => module <> ".beam", "interface" => "module.cati.json"}
      ],
      "interfaces" => [],
      "roots" => [],
      "output" => "C006Companion.beam",
      "assurance" => "assurance.json"
    }
    |> maybe_put("governance", governance)
  end

  defp temporary_directory!(suffix) do
    path =
      Path.join(System.tmp_dir!(), "catena-c006-#{suffix}-#{System.unique_integer([:positive])}")

    File.mkdir_p!(path)
    on_exit(fn -> File.rm_rf!(path) end)
    path
  end

  defp maybe_put(map, _key, nil), do: map
  defp maybe_put(map, key, value), do: Map.put(map, key, value)

  defp signing_payload!({:ok, document}),
    do: CanonicalJCS.payload("manifest", document["signed"])

  defp refresh_interface_digest(interface) do
    payload = Map.delete(interface, "digest")

    digest =
      :crypto.hash(:sha256, Catena.CanonicalJSON.encode(payload))
      |> Base.encode16(case: :lower)

    Map.put(payload, "digest", digest)
  end

  defp integer(value), do: %{"tag" => "integer", "value" => value}
  defp variable(name), do: %{"tag" => "variable", "name" => name}

  defp call(callee, arguments),
    do: %{"tag" => "call", "callee" => callee, "arguments" => arguments}

  defp binary(operator, left, right),
    do: %{"tag" => "binary", "operator" => operator, "left" => left, "right" => right}

  defp forall(variables, type), do: %{"forall" => variables, "type" => type}
  defp integer_type, do: %{"tag" => "integer"}
  defp boolean_type, do: %{"tag" => "boolean"}

  defp function_type(parameter, result),
    do: %{"tag" => "function", "parameter" => parameter, "result" => result, "effect" => []}
end
