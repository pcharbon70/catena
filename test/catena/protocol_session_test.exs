defmodule Catena.ProtocolSessionTest do
  use ExUnit.Case, async: false
  alias Catena.Protocol.{Contract, Session}
  alias Catena.Task.Runtime

  @roles %{
    request: "request_role",
    negotiate: "negotiate_role",
    reply: "reply_role",
    ready: "ready_role"
  }
  @reply "(Variant (row (field reply_role (Tuple Int Int)) (field ready_role Int)))"
  @mailbox "(Variant (row (field request_role (Tuple Int Int (Process #{@reply}))) (field negotiate_role (Process #{@reply}))))"

  setup do
    source = """
    (module ProtocolSessionProbe (edition 0.1) (revision 0.1.8) (origin "test://protocol-session")
      (export process Server)
      (process Server (mailbox #{@mailbox}) (params) (unit)))
    """

    {:ok, core} = Catena.check_kernel(source)
    binary = core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()

    {:ok, contract} =
      Contract.from_interface(binary, "Server", "test://protocol", "1.0.0", @roles)

    %{contract: contract}
  end

  test "overlapping requests receive reversed replies without confusing correlation", %{
    contract: contract
  } do
    owner = self()
    peer = peer(contract, owner)

    assert {{:ok, :done}, [{_, {:ok, :unit}}]} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 2, 1_000_000_000, fn session ->
                   {:ok, first} = Session.submit(session, 10, 1_000_000_000)
                   {:ok, second} = Session.submit(session, 20, 1_000_000_000)
                   assert_receive {:request, 0, 10, broker}
                   assert_receive {:request, 1, 20, ^broker}
                   send(broker, {:catena_variant, :reply_role, {1, 21}})
                   send(broker, {:catena_variant, :reply_role, {0, 11}})
                   assert Session.await(second) == {:ok, 21}
                   assert Session.await(first) == {:ok, 11}
                   assert Session.await(first) == {:error, :invalid_request_handle}
                   :done
                 end)
               end,
               1_000_000_000
             )
  end

  test "terminal but unobserved results retain admission; observing permits another request", %{
    contract: contract
  } do
    peer = peer(contract, self())

    assert {{:ok, :done}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 1, 1_000_000_000, fn session ->
                   {:ok, request} = Session.submit(session, 10, 1_000_000_000)
                   assert_receive {:request, 0, 10, broker}
                   assert :ok = Session.cancel(request)
                   assert Session.submit(session, 20, 1_000_000_000) == {:error, :overloaded}
                   assert Session.await(request) == {:error, :cancelled}
                   {:ok, next} = Session.submit(session, 20, 1_000_000_000)
                   assert_receive {:request, 1, 20, ^broker}
                   send(broker, {:catena_variant, :reply_role, {0, 99}})
                   send(broker, {:catena_variant, :reply_role, {1, 21}})
                   assert Session.await(next) == {:ok, 21}
                   :done
                 end)
               end,
               1_000_000_000
             )
  end

  test "zero deadline and late reply retain timeout and aliases are removed at scope exit", %{
    contract: contract
  } do
    peer = peer(contract, self())
    send(self(), :ordinary_message)

    assert {{:ok, :done}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 2, 1_000_000_000, fn session ->
                   {:ok, request} = Session.submit(session, 10, 0)
                   assert_receive {:request, 0, 10, broker}
                   assert Session.await(request) == {:error, :timed_out}
                   send(broker, {:catena_variant, :reply_role, {0, 99}})
                   {:ok, _abandoned} = Session.submit(session, 20, 1_000_000_000)
                   assert_receive {:request, 1, 20, ^broker}
                   send(broker, {:catena_variant, :reply_role, {1, 21}})
                   :done
                 end)
               end,
               1_000_000_000
             )

    assert_receive :ordinary_message
    assert Process.info(self(), :messages) == {:messages, []}
  end

  test "peer loss returns explicit pending failure and mismatched schema transmits no request", %{
    contract: contract
  } do
    peer = peer(contract, self())

    assert {{:ok, {:error, :peer_lost}}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 1, 1_000_000_000, fn session ->
                   {:ok, request} = Session.submit(session, 10, 1_000_000_000)
                   assert_receive {:request, 0, 10, _}
                   Process.exit(peer, :kill)
                   Session.await(request)
                 end)
               end,
               1_000_000_000
             )

    wrong = peer(%{contract | wire_identity: contract.wire_identity + 1}, self())

    assert {{:ok, {:error, :schema_mismatch}}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, wrong, 1, 1_000_000_000, fn _ ->
                   flunk("mismatched peer admitted")
                 end)
               end,
               1_000_000_000
             )

    refute_receive {:request, _, _, _}
  end

  test "invalid payload and stale session handles cannot send application traffic", %{
    contract: contract
  } do
    peer = peer(contract, self())

    assert {{:ok, session}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 1, 1_000_000_000, fn session ->
                   assert Session.submit(session, make_ref(), 10) == {:error, :invalid_payload}
                   assert Session.submit(session, 2, -1) == {:error, :invalid_duration}
                   session
                 end)
               end,
               1_000_000_000
             )

    assert catch_error(Session.submit(session, 1, 10)) ==
             {:catena_trap, :expired_protocol_session}

    refute_receive {:request, _, _, _}
  end

  test "native adapter exchanges only checked envelope roles with a compiled Catena actor" do
    source = fn identity ->
      """
      (module ProtocolCompiledPeer (edition 0.1) (revision 0.1.8) (origin "test://protocol-compiled-peer")
        (export process Server)
        (def loop
          (signature (Fn (Fn Unit (effects Process) Unit) (effects Process) Unit) (uses))
          (fn (step (Fn Unit (effects Process) Unit))
            (sequence (call (var step) (unit)) (call (var loop) (var step)))))
        (process Server (mailbox #{@mailbox}) (params)
          (call (var loop)
            (fn (ignored Unit)
              (receive
                (case (variant negotiate_role (bind reply_to))
                  (send (var reply_to) (inject ready_role #{identity})))
                (case (variant request_role (tuple (bind correlation) (bind payload) (bind reply_to)))
                  (send (var reply_to) (inject reply_role (tuple (var correlation) (add (var payload) 1))))))))))
      """
    end

    assert {:ok, core} = Catena.check_kernel(source.(0))
    interface = core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()

    assert {:ok, contract} =
             Contract.from_interface(
               interface,
               "Server",
               "test://compiled-protocol",
               "1.0.0",
               @roles
             )

    assert {:ok, module, binary, metadata} =
             Catena.compile_kernel(source.(contract.wire_identity))

    assert metadata.interface_binary == interface
    assert {:module, ^module} = :code.load_binary(module, ~c"protocol-peer.beam", binary)
    peer = apply(module, :__catena_spawn_Server, [])

    on_exit(fn ->
      Process.exit(peer, :kill)
      :code.purge(module)
      :code.delete(module)
    end)

    assert {{:ok, {{:ok, 11}, {:ok, 21}}}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 2, 1_000_000_000, fn session ->
                   {:ok, first} = Session.submit(session, 10, 1_000_000_000)
                   {:ok, second} = Session.submit(session, 20, 1_000_000_000)
                   {Session.await(first), Session.await(second)}
                 end)
               end,
               1_000_000_000
             )

    {:ok, client_core} =
      Catena.check_kernel("""
      (module ProtocolCompiledClient (edition 0.1) (revision 0.1.8) (origin "test://protocol-client")
        (def payload (signature Int (uses)) 42))
      """)

    {:ok, program} =
      Catena.Protocol.Program.check(client_core, contract, [
        {:submit, "request", "payload", 1_000_000_000},
        {:await, "request"}
      ])

    {:ok, client, client_binary, client_metadata} = Catena.Protocol.Program.compile(program)
    {:module, ^client} = :code.load_binary(client, ~c"protocol-client.beam", client_binary)

    on_exit(fn ->
      :code.purge(client)
      :code.delete(client)
    end)

    assert apply(client, client_metadata.entry, [peer]) ==
             {:catena_variant, :completed, {{:catena_variant, :reply, 43}}}
  end

  test "cancelling an owner blocked on a reply joins its protocol worker", %{contract: contract} do
    owner = self()
    peer = peer(contract, owner)

    assert {{:exit, {:child, _, {:cancelled, 7}}}, _} =
             Runtime.scope(
               fn outer ->
                 child =
                   Runtime.start(outer, fn ->
                     Runtime.scope(
                       fn inner ->
                         Session.with_session(
                           inner,
                           contract,
                           peer,
                           1,
                           1_000_000_000,
                           fn session ->
                             {:ok, request} = Session.submit(session, 10, 1_000_000_000)
                             send(owner, :waiting_on_reply)
                             Session.await(request)
                           end
                         )
                       end,
                       1_000_000_000
                     )
                     |> Runtime.value()
                   end)

                 assert_receive :waiting_on_reply
                 assert_receive {:request, 0, 10, broker}
                 monitor = Process.monitor(broker)
                 Process.put(:protocol_worker_monitor, {monitor, broker})
                 Runtime.cancel(child, 7)
               end,
               1_000_000_000
             )

    {monitor, broker} = Process.delete(:protocol_worker_monitor)
    assert_receive {:DOWN, ^monitor, :process, ^broker, _}
    refute Process.alive?(broker)
  end

  test "invalid response is terminal and cannot be replaced by a later valid reply", %{
    contract: contract
  } do
    peer = peer(contract, self())

    assert {{:ok, {:error, :invalid_response}}, _} =
             Runtime.scope(
               fn scope ->
                 Session.with_session(scope, contract, peer, 1, 1_000_000_000, fn session ->
                   {:ok, request} = Session.submit(session, 10, 1_000_000_000)
                   assert_receive {:request, 0, 10, broker}
                   send(broker, {:catena_variant, :reply_role, {0, true}})
                   send(broker, {:catena_variant, :reply_role, {0, 11}})
                   Session.await(request)
                 end)
               end,
               1_000_000_000
             )
  end

  defp peer(contract, owner) do
    pid = spawn(fn -> peer_loop(contract, owner) end)
    on_exit(fn -> Process.exit(pid, :kill) end)
    pid
  end

  defp peer_loop(contract, owner) do
    receive do
      {:catena_variant, :negotiate_role, requester} ->
        send(requester, {:catena_variant, :ready_role, contract.wire_identity})
        peer_loop(contract, owner)

      {:catena_variant, :request_role, {id, payload, reply}} ->
        send(owner, {:request, id, payload, reply})
        peer_loop(contract, owner)
    end
  end
end
