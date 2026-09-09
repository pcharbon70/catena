defmodule Catena.CallingDescriptorTest do
  use ExUnit.Case, async: false
  @moduletag obligations: ~w(CV-OBL-002 CV-OBL-003 CV-OBL-005)
  alias Catena.Calling.Descriptor

  test "ordinary call shape binds written arity separately from the curried type" do
    int = %{"tag" => "integer"}
    fn_type = fn a, b -> %{"tag" => "function", "parameter" => a, "result" => b} end

    source =
      JSON.encode!(%{
        "version" => "0.1.1",
        "module" => "CallingShape",
        "exports" => ["capture"],
        "definitions" => [
          %{
            "name" => "capture",
            "parameters" => ["x"],
            "signature" => %{"forall" => [], "type" => fn_type.(int, fn_type.(int, int))},
            "body" => %{
              "tag" => "function",
              "parameter" => "y",
              "body" => %{"tag" => "variable", "name" => "x"}
            }
          }
        ]
      })

    assert {:ok, core} = Catena.check_json(source)
    assert {:ok, description} = Descriptor.build(core)

    assert [%{beam_arity: 1, semantic_parameters: [:integer, :integer], result_type: :integer}] =
             description.entries

    assert :ok = Descriptor.verify(description, core)
    assert {:error, _} = Descriptor.verify(%{description | module: "Forged"}, core)

    assert {:error, _} =
             Descriptor.verify(%{description | forms_digest: String.duplicate("0", 64)}, core)

    assert {:ok, ^description} = Descriptor.build(core)
  end

  test "kernel inventory covers actual exported values and process spawn entries" do
    assert {:ok, core} =
             File.read!("test/fixtures/c010-kernel.catena") |> Catena.check_kernel()

    assert {:ok, description} = Descriptor.build(core)
    assert :ok = Descriptor.verify(description, core)
    assert Enum.any?(description.entries, &(&1.kind == :process_entry and &1.name == "Selective"))
    assert Enum.any?(description.functions, &(not &1.exported))
    assert description.interface_digest == Catena.Kernel.Interface.build(core)["digest"]
    assert description.unadmitted == [:general_foreign_call, :general_foreign_callback]

    for entry <- description.entries do
      assert Enum.any?(
               description.functions,
               &(&1.name == entry.symbol and &1.arity == entry.beam_arity)
             )
    end
  end

  test "artifact verification rejects substituted code even when descriptor metadata is retained" do
    alias Catena.Calling.Artifact

    source = fn value ->
      JSON.encode!(%{
        "version" => "0.1.1",
        "module" => "CallingArtifact",
        "exports" => ["main"],
        "definitions" => [
          %{
            "name" => "main",
            "parameters" => [],
            "signature" => %{"forall" => [], "type" => %{"tag" => "integer"}},
            "body" => %{"tag" => "integer", "value" => value}
          }
        ]
      })
    end

    assert {:ok, core} = Catena.check_json(source.(41))
    assert {:ok, other} = Catena.check_json(source.(99))
    assert {:ok, artifact} = Artifact.build(core)
    assert :ok = Artifact.verify(artifact, core)
    assert {:ok, substituted} = Artifact.build(other)

    assert {:error, :invalid_call_artifact} =
             Artifact.verify(%{artifact | binary: substituted.binary}, core)

    assert {:error, :invalid_call_artifact} =
             Artifact.verify(
               %{artifact | binary: substituted.binary, binary_digest: substituted.binary_digest},
               core
             )

    assert {:error, :invalid_call_artifact} =
             Artifact.verify(
               %{artifact | descriptor: %{artifact.descriptor | toolchain_digest: "forged"}},
               core
             )

    assert {:ok, ^artifact} = Artifact.build(core)
  end

  test "checked adapter refuses wrong arguments and substituted code before loading an entry" do
    alias Catena.Calling.{Artifact, Adapter}

    assert {:ok, core} =
             Catena.check_kernel("""
             (module CheckedCallingEntry (edition 0.1) (revision 0.1.8)
               (origin "test://checked-calling-entry") (export value echo)
               (def echo (signature (Fn Int (effects) Int) (uses)) (fn (x Int) (var x))))
             """)

    assert {:ok, artifact} = Artifact.build(core)
    limits = %{nodes: 3, bytes: 10}
    assert false == :code.is_loaded(artifact.module)
    assert {:error, _} = Adapter.invoke(artifact, core, "echo", [false], limits)
    assert {:error, _} = Adapter.invoke(artifact, core, "echo", [], limits)

    assert {:error, _} =
             Adapter.invoke(%{artifact | binary_digest: "forged"}, core, "echo", [7], limits)

    assert false == :code.is_loaded(artifact.module)
    assert {:ok, 7} = Adapter.invoke(artifact, core, "echo", [7], limits)
    :code.purge(artifact.module)
    :code.delete(artifact.module)
  end

  test "a separately compiled Erlang caller reaches the checked Catena adapter" do
    alias Catena.Calling.Artifact

    assert {:ok, core} =
             Catena.check_kernel("""
             (module ErlangCalledEntry (edition 0.1) (revision 0.1.8)
               (origin "test://erlang-caller") (export value main)
               (def main (signature Int (uses)) 42))
             """)

    assert {:ok, artifact} = Artifact.build(core)
    arguments = Enum.map(~w(A C N V L)a, &{:var, 1, &1})

    forms = [
      {:attribute, 1, :module, :calling_erlang_fixture},
      {:attribute, 1, :export, [invoke: 5]},
      {:function, 1, :invoke, 5,
       [
         {:clause, 1, arguments, [],
          [
            {:call, 1, {:remote, 1, {:atom, 1, Catena.Calling.Adapter}, {:atom, 1, :invoke}},
             arguments}
          ]}
       ]}
    ]

    assert {:ok, fixture, binary, _} =
             Catena.OTP.Compiler.compile(forms,
               specification: "experimental/erlang-caller",
               frontend: "erlang-abstract-format"
             )

    assert {:module, ^fixture} = Catena.OTP.Compiler.load(fixture, ~c"erlang-caller.beam", binary)

    assert {:ok, 42} =
             apply(fixture, :invoke, [artifact, core, "main", [], %{nodes: 1, bytes: 1}])

    for module <- [fixture, artifact.module] do
      :code.purge(module)
      :code.delete(module)
    end
  end
end
