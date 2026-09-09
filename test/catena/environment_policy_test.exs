defmodule Catena.EnvironmentPolicyTest do
  use ExUnit.Case, async: true
  alias Catena.Runtime.Environment.{Policy, Schema}

  @base %{
    operations: [:read, :write],
    max_bytes: 1024,
    ttl_ms: 1000,
    root: "/tmp",
    paths: ["a", "dir/b"]
  }
  test "authority can narrow operations, resources, bytes and lifetime but cannot broaden" do
    {:ok, grant} = Policy.new(:filesystem, @base)
    narrowed = %{@base | operations: [:read], paths: ["a"], max_bytes: 10, ttl_ms: 100}
    assert {:ok, _} = Policy.attenuate(grant, narrowed)

    for changed <- [
          %{narrowed | paths: ["other"]},
          %{narrowed | max_bytes: 1025},
          %{narrowed | ttl_ms: 1001},
          %{narrowed | root: "/"}
        ] do
      assert {:error, :authority_escalation} = Policy.attenuate(grant, changed)
    end

    assert :ok = Policy.authorize(grant, :read, {"a", 1024})
    assert {:error, :denied} = Policy.authorize(grant, :read, {"other", 1})
    assert {:error, :denied} = Policy.authorize(grant, :read, {"a", 1025})
    assert {:error, _} = Policy.new(:filesystem, Map.put(@base, :extra, true))
  end

  test "path and numeric endpoint grants have no implicit expansion" do
    for path <- ["../a", "/a", "a/../b", "a//b", "a/./b", "a\\b", <<0>>] do
      assert {:error, _} = Policy.new(:filesystem, %{@base | paths: [path]})
    end

    base = %{
      operations: [:exchange],
      max_bytes: 64,
      ttl_ms: 1000,
      endpoints: %{"echo" => {{127, 0, 0, 1}, 1234}}
    }

    assert {:ok, grant} = Policy.new(:network, base)
    assert :ok = Policy.authorize(grant, :exchange, {"echo", <<1>>, 1})
    assert {:error, :denied} = Policy.authorize(grant, :exchange, {"other", <<1>>, 1})

    assert {:error, _} =
             Policy.new(:network, %{base | endpoints: %{"echo" => {"localhost", 1234}}})

    assert {:error, _} =
             Policy.new(:network, %{base | endpoints: %{"echo" => {{256, 0, 0, 1}, 1234}}})
  end

  test "every declared service operation has complete checked input and output codecs" do
    for service <- Schema.services(), operation <- Map.keys(Schema.operations(service)) do
      assert {:ok, description} = Schema.operation(service, operation)
      assert :ok = Schema.verify(description)
      assert :ok = Catena.Foreign.Codec.verify(description.input)
      assert :ok = Catena.Foreign.Codec.verify(description.result)
      assert {:error, _} = Schema.verify(Map.put(description, :extra, true))
    end

    assert {:error, _} = Schema.operation(:network, :resolve)
  end
end
