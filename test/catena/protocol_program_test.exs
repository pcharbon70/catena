defmodule Catena.ProtocolProgramTest do
  use ExUnit.Case, async: false
  alias Catena.Protocol.{Contract, Program}

  @roles %{
    request: "request_role",
    negotiate: "negotiate_role",
    reply: "reply_role",
    ready: "ready_role"
  }
  @reply "(Variant (row (field reply_role (Tuple Int Int)) (field ready_role Int)))"
  @mailbox "(Variant (row (field request_role (Tuple Int Int (Process #{@reply}))) (field negotiate_role (Process #{@reply}))))"

  test "checked payload producers and scoped operations form deterministic executable application" do
    {core, contract} = fixture()

    operations = [
      {:submit, "a", "first_payload", 1_000_000_000},
      {:submit, "b", "second_payload", 1_000_000_000},
      {:await, "b"},
      {:await, "a"}
    ]

    assert {:ok, program} = Program.check(core, contract, operations, capacity: 2)
    assert :ok = Program.verify(program)

    assert {:ok, expected} =
             Catena.Protocol.ProgramReference.run(
               program,
               %{1 => [{:reply, 1, 21}, {:reply, 0, 11}]}
             )

    assert {:ok, module, binary, metadata} = Program.compile(program)
    assert {:ok, ^module, ^binary, _} = Program.compile(program)
    assert metadata.interface == nil

    assert {:module, ^module} =
             :code.load_binary(module, ~c"protocol-application-experiment.beam", binary)

    peer = peer(contract)

    on_exit(fn ->
      Process.exit(peer, :kill)
      :code.purge(module)
      :code.delete(module)
    end)

    assert apply(module, metadata.entry, [peer]) ==
             {:catena_variant, :completed,
              {{:catena_variant, :reply, 21}, {:catena_variant, :reply, 11}}}

    assert Catena.Protocol.ProgramReference.canonical(apply(module, metadata.entry, [peer])) ==
             expected

    assert Process.info(self(), :messages) == {:messages, []}
  end

  test "application admission rejects wrong producer types unknown handles and forged evidence" do
    {core, contract} = fixture()

    for operations <- [
          [{:submit, "a", "wrong_payload", 1}],
          [{:await, "missing"}],
          [{:submit, "a", "first_payload", -1}],
          [{:submit, "a", "first_payload", 1}, {:submit, "a", "second_payload", 1}]
        ] do
      assert {:error, %{id: "PRT001"}} = Program.check(core, contract, operations)
    end

    assert {:ok, program} =
             Program.check(core, contract, [{:submit, "a", "first_payload", 1}, {:await, "a"}])

    assert {:error, _} = Program.verify(%{program | result_type: :integer})
    assert {:error, _} = Program.check(core, contract, program.operations, capacity: 0)
  end

  test "compiled overload cancellation and duplicate observation stay explicit outcomes" do
    {core, contract} = fixture()

    operations = [
      {:submit, "a", "first_payload", 1_000_000_000},
      {:submit, "b", "second_payload", 1_000_000_000},
      {:cancel, "a"},
      {:await, "a"},
      {:await, "a"},
      {:await, "b"}
    ]

    {:ok, program} = Program.check(core, contract, operations, capacity: 1)
    {:ok, module, binary, metadata} = Program.compile(program)
    {:module, ^module} = :code.load_binary(module, ~c"protocol-admission.beam", binary)
    peer = peer(contract)

    on_exit(fn ->
      Process.exit(peer, :kill)
      :code.purge(module)
      :code.delete(module)
    end)

    assert apply(module, metadata.entry, [peer]) ==
             {:catena_variant, :completed,
              {{:catena_variant, :cancel_selected, :unit}, {:catena_variant, :cancelled, :unit},
               {:catena_variant, :invalid_request_handle, :unit},
               {:catena_variant, :overloaded, :unit}}}
  end

  test "protocol application revision is exact and artifacts declare its selected boundary" do
    {core, contract} = fixture()
    operations = [{:submit, "a", "first_payload", 1}, {:await, "a"}]

    for version <- Catena.LanguageVersion.before(:local_protocol_contracts) do
      assert {:error, _} =
               Program.check(core, contract, operations,
                 selection: Catena.LanguageVersion.legacy_selection(version)
               )
    end

    {:ok, program} = Program.check(core, contract, operations)

    assert {:error, _} =
             Program.verify(%{
               program
               | selection: Catena.LanguageVersion.legacy_selection("0.1.8")
             })

    {:ok, module, binary, _} = Program.compile(program)
    {:ok, {^module, [compile_info: info]}} = :beam_lib.chunks(binary, [:compile_info])
    assert info[:catena_language_revision] == ~c"0.1.55"
    assert info[:catena_frontend] == ~c"local-protocol-application-0.1.55"
    refute "0.1.55" in Catena.LanguageVersion.interface_versions()
    refute "0.1.55" in Catena.LanguageVersion.signed_format_versions()
  end

  test "nominal payload producers retain the interface owner rather than just its display name" do
    mailbox = String.replace(@mailbox, "Tuple Int Int (Process", "Tuple Int Payload (Process")

    source = fn origin ->
      """
      (module ProtocolNominal (edition 0.1) (revision 0.1.8) (origin "#{origin}")
        (export process Server) (export type Payload)
        (data Payload (params) (constructor PayloadValue (fields Int)))
        (process Server (mailbox #{mailbox}) (params) (unit))
        (def payload (signature Payload (uses)) (construct PayloadValue 5)))
      """
    end

    {:ok, core} = Catena.check_kernel(source.("test://owner-a"))
    interface = core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()

    {:ok, contract} =
      Contract.from_interface(interface, "Server", "test://nominal-protocol", "1.0.0", @roles)

    assert Contract.valid_payload?(contract, :request, {:catena_constructor, :PayloadValue, {5}})

    refute Contract.valid_payload?(
             contract,
             :request,
             {:catena_constructor, :PayloadValue, {true}}
           )

    assert {:ok, _} = Program.check(core, contract, [{:submit, "a", "payload", 1}])
    {:ok, other} = Catena.check_kernel(source.("test://owner-b"))
    assert {:error, _} = Program.check(other, contract, [{:submit, "a", "payload", 1}])
  end

  defp fixture do
    source = """
    (module ProtocolApplicationProbe (edition 0.1) (revision 0.1.8) (origin "test://protocol-application")
      (export process Server)
      (process Server (mailbox #{@mailbox}) (params) (unit))
      (def first_payload (signature Int (uses)) 10)
      (def second_payload (signature Int (uses)) 20)
      (def wrong_payload (signature Bool (uses)) true))
    """

    {:ok, core} = Catena.check_kernel(source)
    interface = core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()

    {:ok, contract} =
      Contract.from_interface(interface, "Server", "test://application", "1.0.0", @roles)

    {core, contract}
  end

  defp peer(contract), do: spawn(fn -> peer_loop(contract, []) end)

  defp peer_loop(contract, requests) do
    receive do
      {:catena_variant, :negotiate_role, reply} ->
        send(reply, {:catena_variant, :ready_role, contract.wire_identity})
        peer_loop(contract, requests)

      {:catena_variant, :request_role, request} ->
        case [request | requests] do
          [{id2, value2, target2}, {id1, value1, target1}] ->
            send(target2, {:catena_variant, :reply_role, {id2, value2 + 1}})
            send(target1, {:catena_variant, :reply_role, {id1, value1 + 1}})
            peer_loop(contract, [])

          waiting ->
            peer_loop(contract, waiting)
        end
    end
  end
end
