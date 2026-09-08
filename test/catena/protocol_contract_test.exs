defmodule Catena.ProtocolContractTest do
  use ExUnit.Case, async: false
  alias Catena.Protocol.Contract

  @roles %{
    request: "request_role",
    negotiate: "negotiate_role",
    reply: "reply_role",
    ready: "ready_role"
  }
  @reply "(Variant (row (field reply_role (Tuple Int Bool)) (field ready_role Int)))"
  @mailbox "(Variant (row (field request_role (Tuple Int Int (Process #{@reply}))) (field negotiate_role (Process #{@reply}))))"

  test "verified closed mailbox roles produce stable nominally bound schema identity" do
    interface = interface(@mailbox)

    assert {:ok, contract} =
             Contract.from_interface(interface, "Server", "test://protocol", "1.0.0", @roles)

    assert contract.request == :integer
    assert contract.response == :boolean
    assert :ok = Contract.validate(contract)

    assert {:ok, ^contract} =
             Contract.from_interface(interface, "Server", "test://protocol", "1.0.0", @roles)

    assert {:ok, next} =
             Contract.from_interface(interface, "Server", "test://protocol", "1.0.1", @roles)

    refute contract.digest == next.digest

    assert {:ok, other} =
             Contract.from_interface(
               interface(@mailbox, "test://other-owner"),
               "Server",
               "test://protocol",
               "1.0.0",
               @roles
             )

    refute contract.digest == other.digest
  end

  test "forged contracts malformed roles and wrong closed signature are rejected" do
    binary = interface(@mailbox)

    {:ok, contract} =
      Contract.from_interface(binary, "Server", "test://protocol", "1.0.0", @roles)

    assert {:error, _} = Contract.validate(%{contract | response: :integer})

    assert {:error, _} =
             Contract.from_interface(binary, "Server", "test://protocol", "latest", @roles)

    assert {:error, _} =
             Contract.from_interface(binary, "Server", "test://protocol", "1.0.0", %{
               @roles
               | ready: "reply_role"
             })

    assert {:error, _} =
             Contract.from_interface(
               interface("Int"),
               "Server",
               "test://protocol",
               "1.0.0",
               @roles
             )

    assert {:error, _} =
             Contract.from_interface("{}", "Server", "test://protocol", "1.0.0", @roles)
  end

  test "typed payload validation never admits raw foreign terms or another shape" do
    {:ok, contract} =
      Contract.from_interface(interface(@mailbox), "Server", "test://protocol", "1.0.0", @roles)

    assert Contract.valid_payload?(contract, :request, 10)
    assert Contract.valid_payload?(contract, :response, true)
    refute Contract.valid_payload?(contract, :response, 1)
    refute Contract.valid_payload?(contract, :request, true)

    for value <- [self(), make_ref(), fn -> 1 end, <<1>>, %{foreign: :buffer}] do
      refute Contract.valid_payload?(contract, :request, value)
    end
  end

  defp interface(mailbox, origin \\ "test://protocol-owner") do
    source = """
    (module ProtocolSchemaProbe
      (edition 0.1) (revision 0.1.8) (origin "#{origin}")
      (export process Server)
      (process Server (mailbox #{mailbox}) (params) (unit)))
    """

    assert {:ok, core} = Catena.check_kernel(source)
    core |> Catena.Kernel.Interface.build() |> Catena.Kernel.Interface.encode()
  end
end
