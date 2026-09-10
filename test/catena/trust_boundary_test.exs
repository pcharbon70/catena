defmodule Catena.TrustBoundaryTest do
  use ExUnit.Case, async: false
  alias Catena.Trust.{Inventory, Profile}

  defp core do
    {:ok, core} =
      Catena.check_kernel("""
      (module TrustLoweringWitness (edition 0.1) (revision 0.1.8) (origin "test://trust-lowering")
        (export value main) (def main (signature Int (uses)) 42))
      """)

    core
  end

  test "reviewed graph classifies all source paths, call expressions and generated data" do
    assert :ok = Profile.verify(Profile.document())
    assert :ok = Profile.audit(File.cwd!())
    assert map_size(Profile.document()["guarantees"]) == 16
    assert Catena.ConformanceInfo.document()["trusted_computing_base"] == Profile.summary()
    refute Profile.summary()["proof_verified_compiler"]
    refute Profile.summary()["runtime_sandbox"]

    changed =
      put_in(Profile.document()["guarantees"]["source-and-static-acceptance"]["depends_on"], [])

    changed =
      Map.put(
        changed,
        "digest",
        Catena.Categorical.Standard.digest(Map.delete(changed, "digest"))
      )

    assert {:error, :invalid_trust_profile} = Profile.verify(changed)
  end

  test "new compiler and dynamic foreign call sites require a reviewed inventory update" do
    {:ok, baseline} = Inventory.elixir_calls("defmodule Example do def work, do: :ok end")

    for source <- [
          "defmodule Example do def work, do: :compile.noenv_forms([], []) end",
          "defmodule Example do def work(m,f,a), do: apply(m,f,a) end",
          "defmodule Example do alias :compile, as: Compiler; def work, do: Compiler.forms([]) end",
          "defmodule Example do def work(f), do: f.() end",
          "defmodule Example do def work, do: {:remote, 0, {:atom,0,:os}, {:atom,0,:cmd}} end"
        ] do
      assert {:ok, changed} = Inventory.elixir_calls(source)

      assert {:error, [%{reason: :changed_boundary_calls}]} =
               Inventory.compare(%{"lib/example.ex" => baseline}, %{"lib/example.ex" => changed})

      assert {:error, [%{reason: :unclassified_source}]} =
               Inventory.compare(%{}, %{"lib/new.ex" => changed})
    end

    assert {:error, [%{reason: :missing_source}]} =
             Inventory.compare(%{"lib/example.ex" => baseline}, %{})

    assert {:error, :trust_source_limit} = Inventory.elixir_calls(:binary.copy(" ", 2_000_001))
  end

  test "syntactic call inventory ignores comments but does not prove control-flow guards" do
    {:ok, a} = Inventory.elixir_calls("if true, do: :compile.noenv_forms([], [])")

    {:ok, b} =
      Inventory.elixir_calls(
        "# A comment about apply(m,f,a)\nif false, do: :compile.noenv_forms([], [])"
      )

    assert a == b
    # Equal call inventories do not establish equal behavior or correct guards.
    assert :ok = Inventory.compare(%{"lib/example.ex" => a}, %{"lib/example.ex" => b})

    assert {:error, [%{reason: :changed_boundary_calls}]} =
             Inventory.compare(%{"priv/table" => "old-digest"}, %{
               "priv/table" => "changed-digest"
             })
  end

  test "Python helper inspection parses imports and calls without executing the inspected source" do
    file = Path.join(System.tmp_dir!(), "catena-trust-#{System.unique_integer([:positive])}.py")
    File.write!(file, "raise RuntimeError('must not execute')\nimport os\nos.system('false')\n")

    try do
      {output, 0} = System.cmd("python3", ["-I", "scripts/scan_python_trust.py", file])
      calls = JSON.decode!(output)
      assert Enum.any?(calls, &(&1["target"] == "os.system"))
      assert Enum.any?(calls, &(&1["target"] == "import"))
    after
      File.rm!(file)
    end
  end

  test "new native source formats and literal build dependency changes cannot bypass the inventory" do
    root = Path.join(System.tmp_dir!(), "catena-trust-tree-#{System.unique_integer([:positive])}")
    File.mkdir_p!(Path.join(root, "src"))
    File.write!(Path.join(root, "src/bypass.erl"), "-module(bypass).\n")

    try do
      assert Inventory.source_paths(root) == ["src/bypass.erl"]
      assert {:error, :unclassified_trust_source_format} = Inventory.scan(root)
      File.write!(Path.join(root, "mix.exs"), "[deps: []]")
      first = Profile.data(root)
      File.write!(Path.join(root, "mix.exs"), "[deps: [{:new_host_code, \"1.0.0\"}]]")

      assert {:error, [%{path: "mix.exs", reason: :changed_boundary_calls}]} =
               Inventory.compare(first, Profile.data(root))
    after
      File.rm_rf!(root)
    end
  end

  test "both structural verifiers reject forged types and kernel effect evidence" do
    valid = core()
    assert :ok = Catena.Kernel.Verifier.verify(valid)

    changed =
      put_in(valid.definitions, [
        %{
          hd(valid.definitions)
          | expression: %{hd(valid.definitions).expression | type: :boolean}
        }
      ])

    assert {:error, _} = Catena.Kernel.Verifier.verify(changed)
    changed = put_in(valid, [:definitions, Access.at(0), :expression, :effects], [:process])
    assert {:error, _} = Catena.Kernel.Verifier.verify(changed)

    json = %{
      "version" => "0.1.1",
      "module" => "TrustJSON",
      "exports" => ["main"],
      "definitions" => [
        %{
          "name" => "main",
          "parameters" => [],
          "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
          "body" => %{"tag" => "integer", "value" => 42}
        }
      ]
    }

    {:ok, ordinary} = Catena.check_json(JSON.encode!(json))
    assert :ok = Catena.TypedCore.Verifier.verify(ordinary)
    altered = put_in(ordinary, [:definitions, Access.at(0), :expression, :type], :boolean)
    assert {:error, _} = Catena.TypedCore.Verifier.verify(altered)
  end

  test "raw OTP accepts wrong lowering while exact source-bound artifact verification refuses it" do
    valid = core()
    {:ok, artifact} = Catena.Calling.Artifact.build(valid)
    forms = Catena.Kernel.Backend.lower(valid, calling: true)

    altered =
      Enum.map(forms, fn
        {:function, annotation, :main, 0, [{:clause, c, [], [], _}]} ->
          {:function, annotation, :main, 0, [{:clause, c, [], [], [{:integer, c, 99}]}]}

        form ->
          form
      end)

    assert altered != forms
    assert {:ok, module, binary, _} = Catena.OTP.Compiler.compile(altered)
    assert {:module, ^module} = :code.load_binary(module, ~c"trust-wrong-lowering", binary)

    try do
      assert apply(module, :main, []) == 99
      assert {:ok, 42, _} = Catena.Kernel.Stepper.run(valid, "main")
      changed = %{artifact | binary: binary, binary_digest: Inventory.digest(binary)}
      assert {:error, :invalid_call_artifact} = Catena.Calling.Artifact.verify(changed, valid)
    after
      :code.delete(module)
      :code.purge(module)
    end
  end

  test "well-typed alternative core is accepted: original-source preservation remains a separate obligation" do
    valid = core()
    changed = put_in(valid, [:definitions, Access.at(0), :expression, :value], 43)
    assert :ok = Catena.Kernel.Verifier.verify(changed)
    assert {:ok, 43, _} = Catena.Kernel.Stepper.run(changed, "main")
    assert {:ok, 42, _} = Catena.Kernel.Stepper.run(valid, "main")
  end

  test "cryptographic authenticity is separate from semantic truth and checked ingress" do
    {public, private} = :crypto.generate_key(:eddsa, :ed25519)
    message = "an intentionally unsupported correctness claim"
    signature = :crypto.sign(:eddsa, :none, message, [private, :ed25519])

    assert Catena.Governance.Crypto.verify(
             message,
             Base.encode16(public, case: :lower),
             Base.encode16(signature, case: :lower)
           )

    refute Catena.Governance.Crypto.verify(
             message <> " altered",
             Base.encode16(public, case: :lower),
             Base.encode16(signature, case: :lower)
           )

    {:ok, codec} = Catena.Foreign.Codec.new({:data, {:tuple, [:integer, :text]}})
    limits = %{nodes: 100, bytes: 1000, depth: 10}
    assert {:ok, _} = Catena.TypedCore.Verifier.verify_foreign_value(codec, {1, "valid"}, limits)

    assert {:error, _} =
             Catena.TypedCore.Verifier.verify_foreign_value(codec, {1, <<255>>}, limits)

    assert {:error, _} =
             Catena.Runtime.Environment.Authority.decode({:catena_adt, :forged, 0, {"io", <<>>}})
  end
end
