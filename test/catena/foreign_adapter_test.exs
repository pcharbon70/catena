defmodule Catena.ForeignAdapterTest.Host do
  def increment(value, control) do
    Catena.Foreign.Control.checkpoint(control)
    value + 1
  end

  def wrong(_, _), do: false
  def fail(_, _), do: raise("host failure")
  def thrown(_, _), do: throw(:host_throw)
  def killed(_, _), do: Process.exit(self(), :kill)
  def opaque_failure(_, _), do: :erlang.error(make_ref())
  def catena_trap(_, _), do: :erlang.error({:catena_trap, 42})

  def cancel_now(_, {Catena.Foreign.Control, _, token}),
    do: throw({Catena.Foreign.Control, token, :requested})

  def invoke(callback, value, _), do: callback.(value)

  def retain(callback, _) do
    :persistent_term.put({__MODULE__, :callback}, callback)
    1
  end

  def cooperative(_, control) do
    :persistent_term.put({__MODULE__, :worker}, self())
    wait(control)
  end

  defp wait(control) do
    Catena.Foreign.Control.checkpoint(control)

    receive do
      :finish -> 42
    after
      1 -> wait(control)
    end
  end

  def ignore(_, _) do
    :persistent_term.put({__MODULE__, :worker}, self())

    receive do
      :finish -> 42
    end
  end
end

defmodule Catena.ForeignAdapterTest do
  use ExUnit.Case, async: false

  @moduletag obligations:
               ~w(FA-OBL-001 FA-OBL-002 FA-OBL-003 FA-OBL-004 FA-OBL-005 FA-OBL-006 FA-OBL-007 FA-OBL-008 FA-OBL-009 FA-OBL-010 FA-OBL-011 FA-OBL-012)
  alias Catena.Foreign.{Adapter, Callback, Codec, Descriptor}
  alias Catena.ForeignAdapterTest.Host
  @limits %{nodes: 1000, bytes: 10_000, depth: 100}

  setup do
    for key <- [:worker, :callback], do: :persistent_term.erase({Host, key})

    on_exit(fn ->
      for key <- [:worker, :callback], do: :persistent_term.erase({Host, key})
    end)

    :ok
  end

  defp integer, do: elem(Codec.new({:data, :integer}), 1)

  defp declaration(name, arguments \\ nil) do
    {:ok, descriptor} =
      Descriptor.new({Host, name}, arguments || [integer()], integer(), "test://host",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    descriptor
  end

  test "exact granted identity and typed data are required before any host call" do
    good = declaration(:increment)
    bad = declaration(:wrong)
    assert {:error, _} = Descriptor.new({Host, :increment}, [integer()], integer(), "test://host")

    Adapter.run([good], @limits, fn scope ->
      assert {:error, :foreign_authority_denied} = Adapter.start(scope, bad, [1])

      assert {:error, :foreign_authority_denied} =
               Adapter.start(scope, Map.put(good, :pure, true), [1])

      assert {:error, _} = Adapter.start(scope, good, [false])
      assert {:error, :foreign_argument_arity} = Adapter.start(scope, good, [])
      assert {:ok, {:completed, 42}} = Adapter.call(scope, good, [41], 1000)
      assert {:ok, events} = Adapter.events(scope)

      assert [
               {:foreign_requested, token, "test://host", _},
               {:foreign_terminal, token, {:completed, 42}}
             ] = events

      assert Task.async(fn -> Adapter.start(scope, good, [1]) end) |> Task.await() ==
               {:error, :invalid_foreign_scope_owner}

      assert {:error, _} = Adapter.poll(scope, make_ref())
    end)
  end

  test "foreign exceptions and wrong results become visible traps" do
    wrong = declaration(:wrong)
    fail = declaration(:fail)

    others = Enum.map([:thrown, :killed, :opaque_failure, :catena_trap], &declaration/1)

    Adapter.run([wrong, fail | others], @limits, fn scope ->
      [thrown, killed, opaque, trap] = others

      assert {:ok, {:trap, {:foreign_failure, :throw, :host_throw}}} =
               Adapter.call(scope, thrown, [1], 1000)

      assert {:ok, {:trap, {:foreign_worker_exit, :killed}}} =
               Adapter.call(scope, killed, [1], 1000)

      assert {:ok, {:trap, :unrepresentable_foreign_failure}} =
               Adapter.call(scope, opaque, [1], 1000)

      assert {:ok, {:trap, 42}} = Adapter.call(scope, trap, [1], 1000)
      assert {:ok, {:trap, {:foreign_result_boundary, _}}} = Adapter.call(scope, wrong, [1], 1000)

      assert {:ok, {:trap, {:foreign_failure, :error, %RuntimeError{}}}} =
               Adapter.call(scope, fail, [1], 1000)
    end)
  end

  test "cancellation may stop cooperatively or race with a completed external effect" do
    stop = declaration(:cooperative)
    ignore = declaration(:ignore)

    Adapter.run([stop, ignore], @limits, fn scope ->
      {:ok, handle} = Adapter.start(scope, stop, [0])
      assert {:ok, :pending} = Adapter.await(scope, handle, 0)
      assert {:ok, :requested} = Adapter.cancel(scope, handle, :stop)

      assert {:ok, {:cancelled, :stop, :external_effects_possible}} =
               Adapter.await(scope, handle, 1000)

      assert {:ok, {:already_terminal, _}} = Adapter.cancel(scope, handle, :again)
      :persistent_term.erase({Host, :worker})
      {:ok, handle} = Adapter.start(scope, ignore, [0])
      worker = await_persistent(:worker)
      assert {:ok, :requested} = Adapter.cancel(scope, handle, :stop)
      assert {:ok, :already_requested} = Adapter.cancel(scope, handle, :again)
      assert {:ok, :pending} = Adapter.await(scope, handle, 1)
      send(worker, :finish)
      assert {:ok, {:completed, 42}} = Adapter.await(scope, handle, 1000)
    end)
  end

  test "scope exit terminates owned work and escaped owner handles expire" do
    host = declaration(:ignore)

    {{scope, worker}, trace} =
      Catena.Effect.Runtime.capture_trace(fn ->
        Adapter.run([host], @limits, fn scope ->
          {:ok, _} = Adapter.start(scope, host, [0])
          {scope, await_persistent(:worker)}
        end)
      end)

    refute Process.alive?(worker)
    assert {:error, :expired_foreign_scope} = Adapter.events(scope)

    assert {:foreign_scope_closed, %{interrupted_workers: 1, external_effects: :possible}} =
             List.last(trace)
  end

  test "verified captured callback crosses a host call and persists only in its scope" do
    description = callback()
    invoke = declaration(:invoke, [{:callback, integer(), integer()}, integer()])
    retain = declaration(:retain, [{:callback, integer(), integer()}])

    assert {:error, :callback_authority_or_limit} =
             Adapter.run([], @limits, fn scope ->
               Adapter.callback(scope, description)
             end)

    Adapter.run(
      [invoke, retain],
      @limits,
      fn scope ->
        {:ok, handle} = Adapter.callback(scope, description)
        assert {:ok, {:completed, 42}} = Adapter.call(scope, invoke, [handle, 40], 2000)

        assert {:error, :invalid_foreign_callback_handle} =
                 Adapter.start(scope, invoke, [fn x -> x end, 40])

        assert {:ok, {:completed, 1}} = Adapter.call(scope, retain, [handle], 1000)
        escaped = await_persistent(:callback)
        assert Task.async(fn -> escaped.(41) end) |> Task.await() == 43

        assert {:catena_trap, {:foreign_callback, {:error, %{kind: :conversion_failure}}}} =
                 catch_error(escaped.(false))

        assert :ok = Adapter.revoke(scope, handle)

        assert {:catena_trap, {:foreign_callback, {:error, :revoked_foreign_callback}}} =
                 catch_error(escaped.(41))
      end,
      allow_callbacks: true
    )

    escaped = await_persistent(:callback)

    assert {:catena_trap, {:foreign_callback, {:error, :expired_foreign_callback}}} =
             catch_error(escaped.(41))
  end

  test "callback capture is checked against compiled type before publication" do
    description = callback()

    assert {:error, :invalid_foreign_callback} =
             Callback.verify(%{description | captures: [false]}, @limits)

    assert {:error, :invalid_foreign_callback} =
             Callback.verify(%{description | name: "missing"}, @limits)

    {:ok, bool} = Codec.new({:data, :boolean})

    assert {:error, :invalid_foreign_callback} =
             Callback.verify(%{description | input: bool}, @limits)
  end

  test "operation capacity and cancelled wait do not retry host work" do
    host = declaration(:increment)

    Adapter.run(
      [host],
      @limits,
      fn scope ->
        assert {:ok, {:completed, 2}} = Adapter.call(scope, host, [1], 1000)
        assert {:error, :foreign_operation_limit} = Adapter.start(scope, host, [1])
        assert {:ok, events} = Adapter.events(scope)
        assert length(events) == 2
      end,
      max_operations: 1
    )
  end

  test "checked capability requests enter only matching granted host declarations" do
    {:ok, core} =
      Catena.Kernel.CapabilityKernel.check(
        """
        (module ForeignProgram (edition 0.1) (revision 0.1.8) (origin "test://foreign-program")
          (export value main)
          (effect Host (operation increment (params Int) Int))
          (def main (signature Int (uses Host)) (request Host increment 41)))
        """,
        %{"Host" => "test://host"}
      )

    [slot] = Map.keys(core.capabilities)
    host = declaration(:increment)
    bindings = %{slot => %{"increment" => host}}
    {:ok, description} = Catena.Effect.foreign_bindings(core, "main", bindings)
    assert {:ok, ^description} = Catena.Interface.foreign_descriptor(core, "main", bindings)

    assert {:ok, ^description} =
             Catena.Kernel.Interface.foreign_descriptor(core, "main", bindings)

    assert {:error, _} = Catena.Foreign.Program.build(core, "main", %{})
    assert {:error, _} = Catena.Foreign.Program.build(core, "main", %{slot => %{"wrong" => host}})
    {:ok, bool} = Codec.new({:data, :boolean})

    {:ok, wrong} =
      Descriptor.new({Host, :increment}, [bool], integer(), "test://host",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    assert {:error, _} =
             Catena.Foreign.Program.build(core, "main", %{slot => %{"increment" => wrong}})

    cancelled = declaration(:cancel_now)

    {:ok, cancelling_program} =
      Catena.Foreign.Program.build(core, "main", %{slot => %{"increment" => cancelled}})

    assert {:catena_resource_cancelled, :requested} =
             catch_throw(
               Adapter.run([cancelled], @limits, fn scope ->
                 Catena.Foreign.Program.invoke(cancelling_program, scope, @limits, 1000)
               end)
             )

    {:ok, program} = Catena.Foreign.Program.build(core, "main", bindings)
    {:ok, {_, [compile_info: info]}} = :beam_lib.chunks(program.binary, [:compile_info])
    assert info[:catena_specification] == ~c"0.1.61"
    assert info[:catena_frontend] == ~c"foreign-program-0.1.61"

    assert info[:catena_foreign_descriptor] ==
             Catena.Calling.Descriptor.digest(
               Map.put(
                 program.description,
                 :forms_digest,
                 Catena.Calling.Descriptor.digest(program.forms)
               )
             )

    assert {:error, _} = Catena.Foreign.Program.verify(%{program | binary: <<0>>})

    Adapter.run([], @limits, fn scope ->
      assert {:error, :foreign_authority_denied} =
               Catena.Foreign.Program.invoke(program, scope, @limits, 1000)

      assert {:ok, []} = Adapter.events(scope)
    end)

    Adapter.run([host], @limits, fn scope ->
      assert {{:ok, 42},
              [
                {:foreign_request, "test://host", _},
                {:foreign_outcome, "test://host", {:ok, {:completed, 42}}}
              ]} =
               Catena.Effect.Runtime.capture_trace(fn ->
                 Catena.Foreign.Program.invoke(program, scope, @limits, 1000)
               end)
    end)
  end

  test "an active callback refuses overlap and scope close releases its waiting foreign caller" do
    {:ok, core} =
      Catena.check_kernel("""
      (module ForeignBusy (edition 0.1) (revision 0.1.8) (origin "test://foreign-busy")
        (export value loop)
        (def loop (signature (Fn Int (effects) Int) (uses))
          (fn (n Int) (call (var loop) (var n)))))
      """)

    {:ok, artifact} = Catena.Calling.Artifact.build(core)
    {:ok, description} = Callback.new(artifact, core, "loop", [], integer(), integer(), @limits)
    retain = declaration(:retain, [{:callback, integer(), integer()}])

    task =
      Adapter.run(
        [retain],
        @limits,
        fn scope ->
          {:ok, handle} = Adapter.callback(scope, description)
          assert {:ok, {:completed, 1}} = Adapter.call(scope, retain, [handle], 1000)
          callback = await_persistent(:callback)
          task = Task.async(fn -> catch_error(callback.(1)) end)
          await_callback_entry(scope)

          assert {:catena_trap, {:foreign_callback, {:error, :callback_reentry}}} =
                   catch_error(callback.(2))

          assert :ok = Adapter.revoke(scope, handle)

          assert {:catena_trap, {:foreign_callback, {:error, :revoked_foreign_callback}}} =
                   catch_error(callback.(3))

          task
        end,
        allow_callbacks: true
      )

    assert {:catena_trap, {:foreign_callback, {:error, :expired_foreign_callback}}} =
             Task.await(task)
  end

  test "owner death terminates active host work without an explicit close" do
    host = declaration(:ignore)
    parent = self()

    owner =
      spawn(fn ->
        Adapter.run([host], @limits, fn scope ->
          {:ok, _} = Adapter.start(scope, host, [0])
          send(parent, {:scope_worker, await_persistent(:worker)})

          receive do
            :never -> :ok
          end
        end)
      end)

    assert_receive {:scope_worker, worker}, 1000
    monitor = Process.monitor(worker)
    Process.exit(owner, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^worker, :killed}, 1000
  end

  defp await_callback_entry(scope, remaining \\ 1000)
  defp await_callback_entry(_, 0), do: flunk("callback did not enter")

  defp await_callback_entry(scope, remaining) do
    {:ok, events} = Adapter.events(scope)

    if Enum.any?(events, &match?({:callback_entered, _}, &1)) do
      :ok
    else
      Process.sleep(1)
      await_callback_entry(scope, remaining - 1)
    end
  end

  test "exact adapter selection retains old codecs and serialized formats" do
    host = declaration(:increment)
    assert host.version == "0.1.61"
    assert host.result.version == "0.1.60"
    assert Catena.LanguageVersion.foreign_frontend_versions() == ["0.1.61"]
    refute "0.1.61" in Catena.LanguageVersion.interface_versions()
    refute "0.1.61" in Catena.LanguageVersion.signed_format_versions()

    for revision <- Catena.LanguageVersion.before(:foreign_adapters) do
      assert {:error, :invalid_foreign_selection} =
               Descriptor.selection(
                 language_selection: Catena.LanguageVersion.legacy_selection(revision)
               )
    end

    assert {:error, _} = Descriptor.verify(%{host | version: "0.1.60"})

    assert {:error, :invalid_foreign_scope_setup} =
             Adapter.run([host], @limits, fn _ -> flunk("wrong selection entered") end,
               language_selection: Catena.LanguageVersion.legacy_selection("0.1.60")
             )
  end

  test "host code replacement invalidates the grant before execution" do
    forms = fn value ->
      [
        {:attribute, 1, :module, :foreign_host_replacement},
        {:attribute, 1, :export, [value: 1]},
        {:function, 1, :value, 1,
         [{:clause, 1, [{:var, 1, :_Control}], [], [{:integer, 1, value}]}]}
      ]
    end

    {:ok, module, binary, _} =
      Catena.OTP.Compiler.compile(forms.(1), frontend: "foreign-replacement-test")

    {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"foreign-replacement", binary)

    {:ok, host} =
      Descriptor.new({module, :value}, [], integer(), "test://replacement",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    Adapter.run([host], @limits, fn scope ->
      {:ok, ^module, replacement, _} =
        Catena.OTP.Compiler.compile(forms.(2), frontend: "foreign-replacement-test")

      {:module, ^module} = Catena.OTP.Compiler.load(module, ~c"foreign-replacement", replacement)
      assert {:error, :foreign_authority_denied} = Adapter.start(scope, host, [])
      assert {:ok, []} = Adapter.events(scope)
    end)

    :code.purge(module)
    :code.delete(module)
  end

  test "a stalled cleanup hits the existing mandatory-release deadline and terminates its manager" do
    parent = self()

    assert {:catena_trap, {:mandatory_release_failed, :deadline_exhausted}} =
             catch_error(
               Adapter.run(
                 [],
                 @limits,
                 fn {Adapter, _, manager, _} ->
                   send(parent, {:stalled_manager, manager})
                   :sys.suspend(manager)
                 end,
                 release_grace_ns: 1_000_000
               )
             )

    assert_receive {:stalled_manager, manager}
    monitor = Process.monitor(manager)
    assert_receive {:DOWN, ^monitor, :process, ^manager, _}, 1000
  end

  defp callback do
    {:ok, core} =
      Catena.check_kernel("""
      (module ForeignCapture (edition 0.1) (revision 0.1.8) (origin "test://foreign-capture")
        (export value add)
        (def add (signature (Fn Int (effects) (Fn Int (effects) Int)) (uses))
          (fn (captured Int) (fn (value Int) (add (var captured) (var value))))))
      """)

    {:ok, artifact} = Catena.Calling.Artifact.build(core)
    {:ok, callback} = Callback.new(artifact, core, "add", [2], integer(), integer(), @limits)
    callback
  end

  defp await_persistent(key, remaining \\ 1000)

  defp await_persistent(_, 0),
    do: flunk("host fixture did not publish its synchronization handle")

  defp await_persistent(key, remaining) do
    case :persistent_term.get({Host, key}, nil) do
      nil ->
        Process.sleep(1)
        await_persistent(key, remaining - 1)

      value ->
        value
    end
  end
end
