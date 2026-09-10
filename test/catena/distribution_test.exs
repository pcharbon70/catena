defmodule Catena.DistributionTest do
  use ExUnit.Case, async: false

  alias Catena.Distribution.{Contract, Model, TLS, Wire}
  alias Catena.Protocol.Contract, as: LocalContract

  @roles %{
    request: "request_role",
    negotiate: "negotiate_role",
    reply: "reply_role",
    ready: "ready_role"
  }
  @reply "(Variant (row (field reply_role (Tuple Int Bool)) (field ready_role Int)))"
  @mailbox "(Variant (row (field request_role (Tuple Int Int (Process #{@reply}))) (field negotiate_role (Process #{@reply}))))"
  @package String.duplicate("1", 64)
  @message String.duplicate("2", 64)

  setup_all do
    fixture = Path.expand("../fixtures/distribution", __DIR__)
    a_cert = certificate(Path.join(fixture, "node-a.pem"))
    b_cert = certificate(Path.join(fixture, "node-b.pem"))
    local = local_contract()

    {:ok, a} =
      Contract.new(local,
        node: "node-a",
        service: "counter",
        package_digest: @package,
        peers: %{
          "node-b" => %{
            certificate_sha256: Contract.hash(b_cert),
            services: ["counter"],
            package_digests: [@package]
          }
        }
      )

    {:ok, b} =
      Contract.new(local,
        node: "node-b",
        service: "counter",
        package_digest: @package,
        peers: %{
          "node-a" => %{
            certificate_sha256: Contract.hash(a_cert),
            services: ["counter"],
            package_digests: [@package]
          }
        }
      )

    {:ok,
     fixture: fixture,
     a_cert: a_cert,
     b_cert: b_cert,
     a: a,
     b: b,
     a_tls: tls(fixture, "node-a"),
     b_tls: tls(fixture, "node-b")}
  end

  test "contracts bind protocol package service peer and exact certificate", context do
    assert :ok = Contract.validate(context.a)
    assert context.a.digest != context.b.digest

    assert :ok =
             Contract.authorize(context.a, "node-b", "counter", @package, context.b_cert)

    assert {:error, :unauthorized_peer} =
             Contract.authorize(context.a, "node-b", "counter", @package, context.a_cert)

    assert {:error, :unauthorized_peer} =
             Contract.authorize(context.a, "node-b", "other", @package, context.b_cert)

    assert {:error, :unauthorized_peer} =
             Contract.authorize(
               context.a,
               "node-b",
               "counter",
               String.duplicate("9", 64),
               context.b_cert
             )

    assert {:error, :invalid_distribution_contract} =
             Contract.validate(%{context.a | package_digest: String.duplicate("3", 64)})
  end

  test "canonical frames round-trip checked payloads and reject mutation", context do
    assert {:ok, bytes} = Wire.encode(context.a, "node-b", :request, 0, @message, 42)
    assert {:ok, decoded} = Wire.decode(context.b, "node-a", :request, bytes)
    assert decoded.payload == 42
    assert decoded.sequence == 0
    assert {:ok, ^bytes} = Wire.encode(context.a, "node-b", :request, 0, @message, 42)

    assert {:error, :invalid_remote_payload} =
             Wire.encode(context.a, "node-b", :request, 0, @message, self())

    changed =
      bytes |> JSON.decode!() |> Map.put("unexpected", true) |> Catena.CanonicalJSON.encode()

    assert {:error, :invalid_remote_frame} = Wire.decode(context.b, "node-a", :request, changed)

    assert {:error, :invalid_remote_frame} =
             Wire.decode(context.b, "node-a", :request, bytes <> " ")

    assert {:error, :invalid_remote_frame} =
             Wire.decode(context.b, "node-a", :request, :binary.copy(" ", 1_048_577))
  end

  test "closed nominal payloads cross the wire without accepting foreign carriers", context do
    local = nominal_contract()

    {:ok, a} =
      Contract.new(local,
        node: "node-a",
        service: "nominal",
        package_digest: @package,
        peers: %{
          "node-b" => %{
            certificate_sha256: Contract.hash(context.b_cert),
            services: ["nominal"],
            package_digests: [@package]
          }
        }
      )

    {:ok, b} =
      Contract.new(local,
        node: "node-b",
        service: "nominal",
        package_digest: @package,
        peers: %{
          "node-a" => %{
            certificate_sha256: Contract.hash(context.a_cert),
            services: ["nominal"],
            package_digests: [@package]
          }
        }
      )

    value = {:catena_constructor, :PayloadValue, {5}}
    assert {:ok, bytes} = Wire.encode(a, "node-b", :request, 0, @message, value)
    assert {:ok, %{payload: ^value}} = Wire.decode(b, "node-a", :request, bytes)

    assert {:error, :invalid_remote_payload} =
             Wire.encode(a, "node-b", :request, 1, String.duplicate("3", 64), self())
  end

  test "disconnect distinguishes work never transmitted from uncertain delivery", context do
    {:ok, model} = Model.new(context.a, "node-b", 2)
    assert {:error, :not_enqueued, ^model} = Model.prepare(model, :request, @message, 1)
    assert {:ok, model} = Model.connect(model, Wire.hello(context.b), context.b_cert)

    assert {:ok, first, _bytes, model} = Model.prepare(model, :request, @message, 1)
    second_id = String.duplicate("3", 64)
    assert {:ok, second, _bytes, model} = Model.prepare(model, :request, second_id, 2)

    assert {:error, :overloaded, ^model} =
             Model.prepare(model, :request, String.duplicate("4", 64), 3)

    assert {:ok, model} = Model.transmitted(model, first)
    assert {:ok, outcomes, disconnected} = Model.disconnect(model)
    assert outcomes == [{first, :delivery_unknown}, {second, :not_enqueued}]
    assert disconnected.connection == :disconnected
    assert disconnected.pending == %{}

    assert {:ok, reconnected} =
             Model.connect(disconnected, Wire.hello(context.b), context.b_cert)

    assert {:ok, _token, _bytes, _reconnected} =
             Model.prepare(reconnected, :request, String.duplicate("5", 64), 4)
  end

  test "handshake rejects a peer with a different protocol revision", context do
    incompatible = local_contract("0.1.1")

    {:ok, peer} =
      Contract.new(incompatible,
        node: "node-b",
        service: "counter",
        package_digest: @package,
        peers: %{
          "node-a" => %{
            certificate_sha256: Contract.hash(context.a_cert),
            services: ["counter"],
            package_digests: [@package]
          }
        }
      )

    {:ok, model} = Model.new(context.a, "node-b", 1)

    assert {:error, :remote_handshake_refused, ^model} =
             Model.connect(model, Wire.hello(peer), context.b_cert)
  end

  test "remote admission acknowledgement is distinct from processing and duplicates do not redeliver",
       context do
    {:ok, sender} = Model.new(context.a, "node-b", 1)
    {:ok, sender} = Model.connect(sender, Wire.hello(context.b), context.b_cert)
    {:ok, token, bytes, sender} = Model.prepare(sender, :request, @message, 7)
    {:ok, sender} = Model.transmitted(sender, token)

    {:ok, receiver} = Model.new(context.b, "node-a", 1)
    {:ok, receiver} = Model.connect(receiver, Wire.hello(context.a), context.a_cert)
    assert {:ok, :deliver, 7, receiver} = Model.receive_frame(receiver, :request, bytes)
    assert {:ok, :duplicate, nil, ^receiver} = Model.receive_frame(receiver, :request, bytes)

    assert {:ok, conflicting} =
             Wire.encode(context.a, "node-b", :request, 1, @message, 8)

    assert {:error, :conflicting_duplicate, ^receiver} =
             Model.receive_frame(receiver, :request, conflicting)

    assert {:ok, :admitted, sender} = Model.acknowledged(sender, token)
    assert sender.pending == %{}
  end

  test "mutually authenticated TLS carries one checked frame between logical nodes", context do
    assert {:ok, listener} = TLS.listen(context.b, context.b_tls)

    server =
      Task.async(fn ->
        assert {:ok, connection} = TLS.accept(listener)
        assert connection.authenticated
        assert connection.peer == "node-a"
        assert {:ok, bytes} = TLS.receive(connection)
        assert {:ok, %{payload: 99}} = Wire.decode(context.b, "node-a", :request, bytes)
        assert :ok = TLS.send(connection, "admitted")
        TLS.close(connection)
        :ok
      end)

    assert {:ok, connection} = TLS.connect(~c"localhost", listener.port, context.a, context.a_tls)
    assert connection.authenticated
    assert connection.peer == "node-b"
    assert {:ok, bytes} = Wire.encode(context.a, "node-b", :request, 0, @message, 99)
    assert :ok = TLS.send(connection, bytes)
    assert {:ok, "admitted"} = TLS.receive(connection)
    TLS.close(connection)
    assert :ok = Task.await(server)
    TLS.close_listener(listener)
  end

  test "TLS chain success cannot bypass application peer authorization", context do
    assert {:ok, wrong} =
             Contract.new(context.b.local,
               node: "node-b",
               service: "counter",
               package_digest: @package,
               peers: %{
                 "node-a" => %{
                   certificate_sha256: String.duplicate("0", 64),
                   services: ["counter"],
                   package_digests: [@package]
                 }
               }
             )

    assert {:ok, listener} = TLS.listen(wrong, context.b_tls)
    server = Task.async(fn -> TLS.accept(listener) end)

    assert {:error, :tls_connect_failed} =
             TLS.connect(~c"localhost", listener.port, context.a, context.a_tls)

    assert {:error, :tls_accept_failed} = Task.await(server)
    TLS.close_listener(listener)
  end

  test "profiles disclose typed framing authentication and uncertainty", context do
    assert Contract.profile().automatic_retry == false
    assert Contract.profile().exactly_once == false
    assert Contract.profile().peer_verification == :certificate_sha256
    assert Wire.profile().max_frame_bytes == 1_048_576
    assert Wire.profile().unknown_fields == :reject
    assert context.a.version == "0.1.76"
    profile = Catena.ConformanceInfo.document()["distribution"]
    assert profile["contract"]["transport"] == "tls_1_3"
    assert profile["wire"]["float_encoding"] == "ieee_754_binary64_hex"
    assert profile["delivery_outcomes"] == ["admitted", "not_enqueued", "delivery_unknown"]
    assert profile["application_processing_acknowledged"] == false
    assert Catena.LanguageVersion.introduced(:distribution) == "0.1.76"

    assert {:error, :tls_listen_failed} =
             TLS.listen(context.a, Keyword.put(context.a_tls, :timeout, 0))
  end

  defp local_contract(version \\ "1.0.0") do
    source = """
    (module DistributionProtocol
      (edition 0.1) (revision 0.1.8) (origin "test://distribution-protocol")
      (export process Server)
      (process Server (mailbox #{@mailbox}) (params) (unit)))
    """

    {:ok, core} = Catena.check_kernel(source)
    interface = core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()

    {:ok, contract} =
      LocalContract.from_interface(interface, "Server", "test://distribution", version, @roles)

    contract
  end

  defp nominal_contract do
    mailbox = String.replace(@mailbox, "Tuple Int Int (Process", "Tuple Int Payload (Process")

    source = """
    (module DistributionNominal
      (edition 0.1) (revision 0.1.8) (origin "test://distribution-nominal")
      (export process Server) (export type Payload)
      (data Payload (params) (constructor PayloadValue (fields Int)))
      (process Server (mailbox #{mailbox}) (params) (unit)))
    """

    {:ok, core} = Catena.check_kernel(source)
    interface = core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()

    {:ok, contract} =
      LocalContract.from_interface(interface, "Server", "test://distribution", "1.0.0", @roles)

    contract
  end

  defp certificate(path) do
    [{:Certificate, der, :not_encrypted}] = path |> File.read!() |> :public_key.pem_decode()
    der
  end

  defp tls(fixture, node) do
    [
      certfile: Path.join(fixture, "#{node}.pem"),
      keyfile: Path.join(fixture, "#{node}-key.pem"),
      cacertfile: Path.join(fixture, "ca.pem"),
      timeout: 5_000
    ]
  end
end
