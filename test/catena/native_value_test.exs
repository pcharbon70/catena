defmodule Catena.NativeValueTest.Host do
  def reference(value, _control), do: is_reference(value)
  def process(value, _control), do: is_pid(value) and node(value) == node()
end

defmodule Catena.NativeValueTest do
  use ExUnit.Case, async: false

  @moduletag obligations:
               ~w(NV-OBL-001 NV-OBL-002 NV-OBL-003 NV-OBL-004 NV-OBL-005 NV-OBL-006 NV-OBL-007 NV-OBL-008 NV-OBL-009)
  alias Catena.Foreign.{Adapter, Codec, Descriptor, NativeValue}
  alias Catena.NativeValueTest.Host
  @limits %{nodes: 100, bytes: 4096, depth: 20}

  defp codec(type), do: elem(Codec.new({:data, type}), 1)

  defp declaration(kind, role) do
    {:ok, declaration} =
      NativeValue.declaration(
        {Host, kind},
        [{:native, role}],
        codec(:boolean),
        "test://native-role",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    declaration
  end

  test "borrowed local process authority sends checked messages, transfers only send authority and expires" do
    mailbox = codec(:integer)
    {:ok, role} = Catena.Type.native_role({:local_process, mailbox})
    {:ok, :unit} = Catena.Kernel.Checker.check_native_operation(role, :send)
    {:ok, ^role} = Catena.Kernel.Checker.check_native_operation(role, :transfer_send_authority)
    host = declaration(:process, role)
    parent = self()

    escaped =
      NativeValue.run([host], [{:local_process, parent, mailbox}], @limits, fn scope ->
        {:ok, handle} = NativeValue.fetch(scope, 0)
        assert Catena.Values.value?(handle)
        assert {:ok, :unit} = NativeValue.send_message(handle, 41)
        assert_receive 41
        assert {:error, %{kind: :conversion_failure}} = NativeValue.send_message(handle, false)

        assert Task.async(fn -> NativeValue.send_message(handle, 42) end) |> Task.await() ==
                 {:ok, :unit}

        assert_receive 42
        assert {:ok, {:completed, true}} = Adapter.call(scope, host, [handle], 1000)

        assert {:error, :invalid_native_handle} =
                 NativeValue.send_message(%{handle | token: make_ref()}, 0)

        assert {:error, :invalid_native_handle} = Adapter.start(scope, host, [parent])
        assert {:ok, events} = Adapter.events(scope)
        assert Enum.count(events, &match?({:native_send, _}, &1)) == 2
        handle
      end)

    assert Process.alive?(parent)
    assert {:error, :expired_native_value} = NativeValue.send_message(escaped, 0)
    refute_received 0

    assert {:error, :invalid_foreign_scope_setup} =
             NativeValue.run(
               [],
               [:fresh_reference, :fresh_reference],
               @limits,
               fn _ -> flunk("excess grants admitted") end,
               max_operations: 1
             )

    NativeValue.run(
      [],
      [{:local_process, parent, mailbox}],
      @limits,
      fn scope ->
        {:ok, handle} = NativeValue.fetch(scope, 0)
        assert {:ok, :unit} = NativeValue.send_message(handle, 7)
        assert {:error, :foreign_operation_limit} = NativeValue.send_message(handle, 8)
        assert_receive 7
        refute_received 8
      end,
      max_operations: 1
    )
  end

  test "dead process send preserves C010's no-liveness-acknowledgement contract" do
    pid = spawn(fn -> :ok end)
    monitor = Process.monitor(pid)
    assert_receive {:DOWN, ^monitor, :process, ^pid, _}

    NativeValue.run([], [{:local_process, pid, codec(:integer)}], @limits, fn scope ->
      {:ok, handle} = NativeValue.fetch(scope, 0)
      assert {:ok, :unit} = NativeValue.send_message(handle, 1)
    end)
  end

  test "fresh references cross only the matching owner-scoped foreign declaration" do
    {:ok, role} = Catena.Type.native_role(:reference)
    host = declaration(:reference, role)

    NativeValue.run([host], [:fresh_reference], @limits, fn scope ->
      {:ok, handle} = NativeValue.fetch(scope, 0)
      assert {:ok, {:completed, true}} = Adapter.call(scope, host, [handle], 1000)
      assert {:error, :invalid_native_handle} = Adapter.start(scope, host, [make_ref()])
      assert {:error, :native_role_operation_forbidden} = NativeValue.send_message(handle, 1)

      assert {:error, :native_role_operation_forbidden} =
               Catena.Kernel.Checker.check_native_operation(role, :transfer_send_authority)

      assert Task.async(fn -> Adapter.start(scope, host, [handle]) end) |> Task.await() ==
               {:error, :invalid_foreign_scope_owner}

      NativeValue.run([host], [:fresh_reference], @limits, fn other ->
        assert {:error, :invalid_native_handle} = Adapter.start(other, host, [handle])
      end)
    end)

    assert {:error, :invalid_foreign_scope_setup} =
             NativeValue.run([], [make_ref()], @limits, fn _ ->
               flunk("raw reference admitted")
             end)
  end

  test "native identity supplies no language equality, reflection or generic data admission" do
    NativeValue.run([], [:fresh_reference], @limits, fn scope ->
      {:ok, handle} = NativeValue.fetch(scope, 0)
      refute Catena.Values.comparable?(handle)
      refute Catena.Values.orderable?(handle)
      assert_raise ArgumentError, fn -> Catena.Values.compare(handle, handle) end

      assert {:error, :native_handle_equality_forbidden} =
               Catena.Kernel.Checker.check_native_operation(handle.role, :equality)

      assert {:error, :native_handle_reflection_forbidden} =
               Catena.Kernel.Checker.check_native_operation(handle.role, :reflection)

      assert {:error, _} = Codec.decode(codec(:integer), handle, @limits)

      assert {:error, :invalid_native_role} =
               Catena.Kernel.Checker.check_native_operation(
                 %{handle.role | equality: true},
                 :equality
               )
    end)
  end

  test "binary and map meanings still use closed typed codecs with complete payload bounds" do
    bytes = :binary.copy(<<0, 255>>, 524_288)
    {:ok, boundary} = Codec.new({:data, {:record, %{"payload" => :bytes}}})
    limits = %{nodes: 8, bytes: byte_size(bytes) + 32, depth: 3}
    assert {:ok, semantic} = Codec.decode(boundary, %{payload: bytes}, limits)
    assert {:ok, %{payload: ^bytes}} = Codec.encode(boundary, semantic, limits)
    assert {:error, _} = Codec.decode(boundary, %{payload: bytes}, %{limits | bytes: 1024})
    assert {:error, _} = Codec.decode(codec(:text), <<255>>, @limits)
    assert {:error, _} = Codec.decode(boundary, %{{:unsupported, 0} => bytes}, limits)
    assert {:error, _} = Codec.decode(boundary, %{payload: bytes, extra: 1}, limits)
  end

  test "live and closed ports, raw funs and unsupported grant forms remain explicit exclusions" do
    port = Port.open({:spawn_executable, System.find_executable("cat")}, [:binary])
    on_exit(fn -> if Port.info(port) != nil, do: Port.close(port) end)
    assert {:error, :native_port_role_not_admitted} = NativeValue.role(:port)
    assert {:error, :use_checked_callback_role} = NativeValue.role(:function)

    assert {:error, :invalid_foreign_scope_setup} =
             NativeValue.run([], [port], @limits, fn _ -> flunk("live port admitted") end)

    true = Port.close(port)

    assert {:error, :invalid_foreign_scope_setup} =
             NativeValue.run([], [port], @limits, fn _ -> flunk("closed port admitted") end)

    assert {:error, :invalid_foreign_scope_setup} =
             NativeValue.run([], [fn x -> x end], @limits, fn _ -> flunk("raw fun admitted") end)
  end

  test "native roles require 0.1.62 without widening retained 0.1.61 scopes or artifacts" do
    {:ok, role} = NativeValue.role(:reference)
    host = declaration(:reference, role)
    assert host.version == "0.1.62"

    assert {:error, :invalid_native_selection} =
             NativeValue.run([], [], @limits, fn _ -> flunk("wrong selection entered") end,
               language_selection: Catena.LanguageVersion.legacy_selection("0.1.61")
             )

    assert {:error, :invalid_native_selection} =
             NativeValue.declaration(
               {Host, :reference},
               [{:native, role}],
               codec(:boolean),
               "test://native-role",
               language_selection: Catena.LanguageVersion.legacy_selection("0.1.61")
             )

    {expired_scope, expired_reference} =
      NativeValue.run([host], [:fresh_reference], @limits, fn scope ->
        {:ok, handle} = NativeValue.fetch(scope, 0)
        {scope, handle}
      end)

    assert {:error, :expired_foreign_scope} =
             Adapter.start(expired_scope, host, [expired_reference])

    assert {:error, :invalid_foreign_declaration} =
             Descriptor.new(
               {Host, :reference},
               [{:native, role}],
               codec(:boolean),
               "test://native-role",
               trust: :trusted_beam,
               scheduler: :owned_process,
               cancellation: :cooperative
             )

    assert {:error, :invalid_foreign_scope_setup} =
             Adapter.run([host], @limits, fn _ -> flunk("old scope admitted new declaration") end)

    assert {:error, :invalid_foreign_scope_setup} =
             Adapter.run([], @limits, fn _ -> flunk("old scope admitted native grants") end,
               native_grants: [:fresh_reference]
             )

    refute "0.1.62" in Catena.LanguageVersion.compilable_revisions()
    refute "0.1.62" in Catena.LanguageVersion.artifact_versions()
    refute "0.1.62" in Catena.LanguageVersion.interface_versions()
    refute "0.1.62" in Catena.LanguageVersion.signed_format_versions()

    NativeValue.run([], [:fresh_reference], @limits, fn scope ->
      {:ok, handle} = NativeValue.fetch(scope, 0)
      assert {:error, :unknown_native_grant} = NativeValue.fetch(scope, 1)

      assert {:error, :invalid_native_handle} =
               NativeValue.send_message(%{handle | role: %{role | kind: :local_process}}, 1)
    end)
  end
end
