defmodule Catena.TrustedPolicyTest.Host do
  def native_roundtrip(value, control) do
    Catena.Foreign.Control.checkpoint(control)
    ready = :persistent_term.get({__MODULE__, :native_ready})

    {:ok, result} =
      Catena.Foreign.Native.run(ready.package, ready.policy, fn scope ->
        Catena.Foreign.Native.call(scope, value * 1.0)
      end)

    trunc(result)
  end

  def increment(value, control) do
    Catena.Foreign.Control.checkpoint(control)
    value + 1
  end
end

defmodule Catena.TrustedPolicyTest do
  use ExUnit.Case, async: false
  alias Catena.Trust.{Obligations, Policy}
  alias Catena.Foreign.Native.Package
  @limits %{nodes: 1000, bytes: 10_000, depth: 100}

  defp pure do
    {:ok, core} =
      Catena.check_kernel("""
      (module TrustPolicyPure (edition 0.1) (revision 0.1.8) (origin "test://trust-policy-pure")
        (export value main) (def main (signature Int (uses)) 42))
      """)

    {:ok, artifact} = Catena.Calling.Artifact.build(core)
    %{kind: :pure, core: core, artifact: artifact}
  end

  defp foreign do
    {:ok, core} =
      Catena.Kernel.CapabilityKernel.check(
        """
        (module TrustPolicyForeign (edition 0.1) (revision 0.1.8) (origin "test://trust-policy-foreign")
          (export value main) (effect Host (operation increment (params Int) Int))
          (def main (signature Int (uses Host)) (request Host increment 41)))
        """,
        %{"Host" => "test://trust-policy"}
      )

    {:ok, int} = Catena.Foreign.Codec.new({:data, :integer})

    {:ok, declaration} =
      Catena.Foreign.Descriptor.new(
        {Catena.TrustedPolicyTest.Host, :increment},
        [int],
        int,
        "test://trust-policy",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    [slot] = Map.keys(core.capabilities)

    {:ok, program} =
      Catena.Foreign.Program.build(core, "main", %{slot => %{"increment" => declaration}})

    %{kind: :foreign, program: program}
  end

  defp package_node(implementation, dependencies \\ []),
    do: %{version: "1.0.0", dependencies: dependencies, implementation: implementation}

  defp grants(graph),
    do: Map.new(Obligations.obligations(graph, graph.root), &{&1["boundary"], &1["obligations"]})

  defp bytes(graph), do: Catena.CanonicalJCS.encode(graph.document)

  test "pure compiled application has no foreign obligations and exact sidecar is independently rebound" do
    {:ok, graph} = Catena.Interface.trusted_obligations("app", %{"app" => package_node(pure())})
    assert Obligations.obligations(graph, "app") == []
    assert {:ok, _} = Catena.Package.Manifest.decode_trusted_obligations(bytes(graph))
    assert :ok = Catena.Assurance.verify_trusted_obligations(bytes(graph), graph)

    assert {:ok, 42} =
             Policy.run(
               graph,
               %{},
               &Policy.invoke(&1, "app", "main", [], Map.delete(@limits, :depth))
             )

    assert {:error, _} = Obligations.decode(:binary.copy(" ", 1_048_577))
  end

  test "transitive diamond retains a single boundary with exact owners and executes compiled foreign entry" do
    impl = pure()

    inputs = %{
      "native-edge" => package_node(foreign()),
      "left" => package_node(impl, ["native-edge"]),
      "right" => package_node(impl, ["native-edge"]),
      "app" => package_node(impl, ["left", "right"])
    }

    {:ok, graph} = Obligations.build("app", inputs)
    [record] = Obligations.obligations(graph, "app")
    assert record["kind"] == "trusted-beam"
    assert Enum.map(record["owners"], & &1["package"]) == ["native-edge"]

    assert {:error, :trusted_admission_denied} =
             Policy.run(
               graph,
               %{},
               &Policy.invoke(&1, "app", "main", [], Map.delete(@limits, :depth))
             )

    assert {:ok, 42} =
             Policy.run(
               graph,
               grants(graph),
               &Catena.Foreign.Program.invoke_admitted(&1, "native-edge", "main", @limits)
             )

    assert {:ok, 42} =
             Policy.run(
               graph,
               grants(graph),
               &Policy.invoke(&1, "app", "main", [], Map.delete(@limits, :depth))
             )

    [{id, _}] = Map.to_list(grants(graph))

    assert {:error, :trusted_admission_denied} =
             Policy.run(graph, %{id => []}, &Policy.admit(&1, "app"))
  end

  test "forged omission or changed artifact cannot substitute for original checked graph" do
    {:ok, graph} = Obligations.build("app", %{"app" => package_node(foreign())})
    doc = put_in(graph.document, ["nodes", "app", "obligations"], [])
    doc = Map.put(doc, "digest", Catena.CanonicalJCS.digest(Map.delete(doc, "digest")))
    assert {:ok, _} = Obligations.decode(Catena.CanonicalJCS.encode(doc))

    assert {:error, :unbound_trusted_obligation_document} =
             Obligations.verify_document(Catena.CanonicalJCS.encode(doc), graph)

    assert {:error, _} = Obligations.verify(%{graph | document: doc})
    changed = put_in(graph.inputs, ["app", :implementation, :program, :binary], <<0>>)
    assert {:error, _} = Obligations.build("app", changed)

    assert {:error, _} =
             Obligations.build("app", %{"app" => Map.put(package_node(pure()), :safe, true)})

    assert {:error, _} =
             Obligations.build("app", %{
               "app" => package_node(%{kind: :pure, function: fn -> :ok end})
             })
  end

  test "cycles missing edges unreachable dependencies duplicate edges and capacity excess fail closed" do
    p = pure()

    for inputs <- [
          %{"a" => package_node(p, ["missing"])},
          %{"a" => package_node(p, ["b"]), "b" => package_node(p, ["a"])},
          %{"a" => package_node(p), "b" => package_node(p)},
          %{"a" => package_node(p, ["b", "b"]), "b" => package_node(p)},
          Map.new(1..65, &{Integer.to_string(&1), package_node(p)})
        ] do
      assert {:error, _} = Obligations.build("a", inputs)
    end
  end

  test "child attenuation cannot add authority and parent revocation reaches existing descendants" do
    {:ok, graph} = Obligations.build("app", %{"app" => package_node(foreign())})
    grants = grants(graph)
    [id] = Map.keys(grants)

    escaped =
      Policy.run(graph, grants, fn scope ->
        {:ok, child} = Policy.attenuate(scope, grants)
        {:ok, empty} = Policy.attenuate(scope, %{})
        assert {:error, :trusted_admission_denied} = Policy.admit(empty, "app")
        assert {:error, :trusted_attenuation_denied} = Policy.attenuate(empty, grants)
        assert {:ok, 42} = Policy.invoke(child, "app", "main", [], @limits)
        assert :ok = Policy.revoke(scope, id)

        assert {:error, :trusted_admission_denied} =
                 Policy.invoke(child, "app", "main", [], @limits)

        {:ok, later} = Policy.attenuate(scope, grants)
        assert {:error, :trusted_admission_denied} = Policy.admit(later, "app")
        child
      end)

    assert {:error, :expired_trusted_scope} = Policy.admit(escaped, "app")
  end

  test "scope identity owner and bounded ledger are enforced and child revocation leaves parent usable" do
    {:ok, graph} = Obligations.build("app", %{"app" => package_node(foreign())})
    grants = grants(graph)
    [id] = Map.keys(grants)

    Policy.run(graph, grants, fn {_, owner, pid, _} = scope ->
      assert {:error, :invalid_trusted_scope} =
               Policy.admit({Policy, owner, pid, make_ref()}, "app")

      assert Task.async(fn -> Policy.admit(scope, "app") end) |> Task.await() ==
               {:error, :invalid_trusted_scope_owner}

      {:ok, child} = Policy.attenuate(scope, grants)
      assert :ok = Policy.revoke(child, id)
      assert {:error, :trusted_admission_denied} = Policy.admit(child, "app")
      assert {:ok, _} = Policy.admit(scope, "app")
      assert {:error, :trusted_admission_denied} = Policy.admit(scope, "absent")
      for _ <- 1..62, do: assert({:ok, _} = Policy.attenuate(scope, %{}))
      assert {:error, :trusted_attenuation_denied} = Policy.attenuate(scope, %{})
    end)
  end

  test "same host under a different owning package or replacement version needs explicit new admission" do
    host = foreign()
    {:ok, original} = Obligations.build("app", %{"app" => package_node(host)})
    {:ok, renamed} = Obligations.build("other", %{"other" => package_node(host)})

    {:ok, upgraded} =
      Obligations.build("app", %{"app" => %{package_node(host) | version: "2.0.0"}})

    for graph <- [renamed, upgraded] do
      assert {:error, :trusted_admission_denied} =
               Policy.run(graph, grants(original), &Policy.admit(&1, graph.root))

      assert {:ok, 42} =
               Policy.run(
                 graph,
                 grants(graph),
                 &Policy.invoke(&1, graph.root, "main", [], @limits)
               )

      assert {:error, :unbound_trusted_obligation_document} =
               Obligations.verify_document(bytes(original), graph)
    end
  end

  test "grant lists must be bounded canonical acknowledgements and graph metadata cannot widen pure input" do
    {:ok, graph} = Obligations.build("app", %{"app" => package_node(pure())})
    id = String.duplicate("a", 64)

    for value <- [
          %{id => ["z", "a"]},
          %{id => ["a", "a"]},
          %{id => [String.duplicate("x", 129)]},
          %{id => :all},
          %{"all" => []}
        ] do
      assert {:error, :invalid_trusted_grants} =
               Policy.run(graph, value, fn _ -> flunk("invalid grant entered") end)
    end

    changed =
      put_in(
        graph.inputs,
        ["app", :implementation, :core, :definitions, Access.at(0), :expression, :type],
        :boolean
      )

    assert {:error, _} = Obligations.build("app", changed)
  end

  test "policy revision disclosure does not widen retained executable and signed formats" do
    info = Catena.ConformanceInfo.document()["trusted_obligation_policy"]
    assert info["contract"] == "0.1.71"
    assert info["revocation"] == "future-admissions-only"
    refute info["host_safety_proven"]
    refute "0.1.71" in Catena.LanguageVersion.compilable_revisions()
    refute "0.1.71" in Catena.LanguageVersion.signed_format_versions()
    refute "0.1.71" in Catena.LanguageVersion.interface_versions()
    assert :ok == Policy.valid_grants(%{})

    assert {:error, :invalid_trusted_grants} =
             Policy.valid_grants(%{String.duplicate("a", 64) => ["a" | :bad]})
  end

  test "policy manager expires on owner death" do
    {:ok, graph} = Obligations.build("app", %{"app" => package_node(pure())})
    parent = self()

    owner =
      spawn(fn ->
        Policy.run(graph, %{}, fn {_, _, pid, _} ->
          send(parent, {:manager, pid})

          receive do
            :done -> :ok
          end
        end)
      end)

    assert_receive {:manager, manager}, 3000
    monitor = Process.monitor(manager)
    Process.exit(owner, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^manager, :normal}, 3000
  end

  test "actual signed native work retains visible obligations and package replacement needs new grants" do
    directory =
      Path.join(System.tmp_dir!(), "catena-policy-native-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)

    try do
      assert {_, 0} =
               System.cmd(
                 "cc",
                 [
                   "-std=c11",
                   "-Wall",
                   "-Wextra",
                   "-Werror",
                   "test/fixtures/native-service.c",
                   "-o",
                   Path.join(directory, "service")
                 ],
                 stderr_to_stdout: true
               )

      payloads = %{"service" => File.read!(Path.join(directory, "service"))}
      {public, private} = :crypto.generate_key(:eddsa, :ed25519)
      publisher = Base.encode16(public, case: :lower)

      make = fn work ->
        {:ok, description} =
          Package.describe(:port, payloads,
            scheduler: :os_process,
            timeout_ms: 50,
            max_work_units: work
          )

        signature =
          :crypto.sign(:eddsa, :none, Package.signing_payload(description), [private, :ed25519])
          |> Base.encode16(case: :lower)

        package = Package.assemble(description, payloads, publisher, signature)

        policy = %{
          kinds: ["port"],
          publishers: [publisher],
          max_package_bytes: 1_000_000,
          unsafe_acknowledgements: Package.obligations(:port)
        }

        {:ok, ready} = Package.verify(package, policy)

        {:ok, graph} =
          Obligations.build("service", %{
            "service" => package_node(%{kind: :native, ready: ready})
          })

        graph
      end

      first = make.(1)
      ready = first.inputs["service"].implementation.ready
      :persistent_term.put({Catena.TrustedPolicyTest.Host, :native_ready}, ready)

      {:ok, core} =
        Catena.Kernel.CapabilityKernel.check(
          """
          (module TrustPolicyNativeApplication (edition 0.1) (revision 0.1.8) (origin "test://trust-policy-native-app")
            (export value main) (effect NativeHost (operation roundtrip (params Int) Int))
            (def main (signature Int (uses NativeHost)) (request NativeHost roundtrip 41)))
          """,
          %{"NativeHost" => "test://native-policy"}
        )

      {:ok, int} = Catena.Foreign.Codec.new({:data, :integer})

      {:ok, declaration} =
        Catena.Foreign.Descriptor.new(
          {Catena.TrustedPolicyTest.Host, :native_roundtrip},
          [int],
          int,
          "test://native-policy",
          trust: :trusted_beam,
          scheduler: :owned_process,
          cancellation: :cooperative
        )

      [slot] = Map.keys(core.capabilities)

      {:ok, program} =
        Catena.Foreign.Program.build(core, "main", %{
          slot => %{"roundtrip" => declaration}
        })

      {:ok, app} =
        Obligations.build(
          "app",
          Map.put(
            first.inputs,
            "app",
            package_node(%{kind: :foreign, program: program}, ["service"])
          )
        )

      assert length(Obligations.obligations(app, "app")) == 2
      foreign_only = app |> grants() |> Map.drop(Map.keys(grants(first)))

      assert {:error, :trusted_admission_denied} =
               Policy.run(app, foreign_only, &Policy.invoke(&1, "app", "main", [], @limits))

      assert {:ok, 41} =
               Policy.run(app, grants(app), &Policy.invoke(&1, "app", "main", [], @limits))

      replacement = make.(2)
      [record] = Obligations.obligations(first, "service")
      assert record["kind"] == "port"
      assert "direct-child-only" in record["obligations"]

      assert {:ok, 1.25} =
               Policy.run(
                 first,
                 grants(first),
                 &Catena.Foreign.Native.invoke_admitted(&1, "service", 1.25)
               )

      assert {:error, :trusted_admission_denied} =
               Policy.run(
                 replacement,
                 grants(first),
                 &Catena.Foreign.Native.invoke_admitted(&1, "service", 1.25)
               )

      assert {:ok, 1.25} =
               Policy.run(
                 replacement,
                 grants(replacement),
                 &Catena.Foreign.Native.invoke_admitted(&1, "service", 1.25)
               )

      [id] = Map.keys(grants(first))

      Policy.run(first, grants(first), fn scope ->
        assert :ok = Policy.revoke(scope, id)

        assert {:error, :trusted_admission_denied} =
                 Catena.Foreign.Native.invoke_admitted(scope, "service", 1.25)
      end)
    after
      :persistent_term.erase({Catena.TrustedPolicyTest.Host, :native_ready})
      File.rm_rf!(directory)
    end
  end
end
