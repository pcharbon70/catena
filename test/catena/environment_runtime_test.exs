defmodule Catena.EnvironmentRuntimeTest do
  use ExUnit.Case, async: false
  alias Catena.Runtime.Environment, as: E
  alias Catena.Runtime.Environment.{Policy, Schema, Authority}
  @limits %{nodes: 10000, bytes: 100_000, depth: 100}
  defp grant(service, ops, extra \\ %{}) do
    {:ok, value} =
      Policy.new(service, Map.merge(%{operations: ops, max_bytes: 4096, ttl_ms: 5000}, extra))

    value
  end

  test "scripted services validate ownership, complete codecs and explicit attenuation" do
    g = grant(:random, [:bytes])
    script = [{:random, :bytes, 3, Schema.success(<<1, 2, 3>>), 0}]

    escaped =
      E.run(
        [g],
        @limits,
        fn bundle ->
          authority = bundle.authorities.random
          assert {:ok, Schema.success(<<1, 2, 3>>)} == E.call(authority, :bytes, 3)
          {:ok, narrow} = E.attenuate(authority, %{g.policy | max_bytes: 1})
          assert {:ok, Schema.failure(:denied)} == E.call(narrow, :bytes, 2)
          assert :ok = E.revoke(authority)
          assert {:ok, Schema.failure(:revoked)} == E.call(narrow, :bytes, 1)
          assert {:ok, manager, owner, _} = Authority.decode(authority)
          forged = Authority.issue(:random, manager, owner, :crypto.strong_rand_bytes(32))
          assert {:error, :invalid_environment_authority} = E.call(forged, :bytes, 1)
          parent = self()
          spawn(fn -> send(parent, {:foreign_owner, E.validate(authority, :random)}) end)
          assert_receive {:foreign_owner, {:error, :invalid_environment_owner}}
          authority
        end,
        fake: script
      )

    assert {:error, :expired_environment_authority} = E.validate(escaped, :random)
  end

  test "cancellation and revocation retire pending work with one terminal answer" do
    g = grant(:time, [:sleep])

    E.run([g], @limits, fn bundle ->
      a = bundle.authorities.time
      {:ok, job} = E.start(a, :sleep, 200)
      assert :ok = E.cancel(job)
      assert {:ok, Schema.failure(:cancelled)} == E.await(job)
      assert :already_completed = E.cancel(job)
      {:ok, job} = E.start(a, :sleep, 200)
      assert :ok = E.revoke(a)
      assert {:ok, Schema.failure(:revoked)} == E.await(job)
      {:ok, events} = E.events(bundle)
      assert Enum.count(events, &match?({:completed, _, _, _}, &1)) == 2
    end)
  end

  test "expiry, request limits, scope return and owner death stop work" do
    g = grant(:time, [:sleep], %{ttl_ms: 40})

    E.run([g], @limits, fn bundle ->
      {:ok, job} = E.start(bundle.authorities.time, :sleep, 40)
      assert {:ok, Schema.failure(:expired)} == E.await(job)
    end)

    parent = self()

    owner =
      spawn(fn ->
        E.run([grant(:time, [:sleep])], @limits, fn bundle ->
          {:ok, _} = E.start(bundle.authorities.time, :sleep, 4000)
          send(parent, {:manager, bundle.manager})

          receive do
            :stay -> :ok
          end
        end)
      end)

    assert_receive {:manager, manager}
    monitor = Process.monitor(manager)
    Process.exit(owner, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^manager, :normal}, 2000

    E.run(
      [grant(:random, [:bytes])],
      @limits,
      fn bundle ->
        assert {:ok, _} = E.call(bundle.authorities.random, :bytes, 0)
        assert {:ok, Schema.failure(:limit)} == E.call(bundle.authorities.random, :bytes, 0)
      end,
      max_requests: 1
    )
  end

  test "real explicitly supplied I/O, logging, clocks, randomness and environment work" do
    {:ok, device} = StringIO.open("input")
    name = "CATENA_ENVIRONMENT_TEST_106"
    System.put_env(name, "configured")

    grants = [
      grant(:io, [:read, :write], %{device: device}),
      grant(:logging, [:emit], %{device: device, levels: ["info"]}),
      grant(:time, [:monotonic, :wall]),
      grant(:random, [:bytes]),
      grant(:environment, [:get], %{names: [name]})
    ]

    try do
      E.run(grants, @limits, fn bundle ->
        a = bundle.authorities
        assert {:ok, Schema.success("input")} == E.call(a.io, :read, 5)
        assert {:ok, Schema.success(:unit)} == E.call(a.io, :write, "output")
        assert {:ok, Schema.success(:unit)} == E.call(a.logging, :emit, {"info", "message"})
        assert {:ok, {:catena_variant, :ok, bytes}} = E.call(a.random, :bytes, 32)
        assert byte_size(bytes) == 32
        assert {:ok, {:catena_variant, :ok, mono}} = E.call(a.time, :monotonic, :unit)
        assert {:ok, {:catena_variant, :ok, wall}} = E.call(a.time, :wall, :unit)
        assert is_integer(mono) and is_integer(wall)

        assert {:ok, Schema.success({:catena_variant, :present, "configured"})} ==
                 E.call(a.environment, :get, name)

        assert {:ok, Schema.failure(:denied)} == E.call(a.environment, :get, "PATH")
      end)

      {_, output} = StringIO.contents(device)
      assert output =~ "output"
      assert output =~ "message"
    after
      System.delete_env(name)
      StringIO.close(device)
    end
  end

  test "real filesystem refuses path escape and symlinks while process grants own child lifetime" do
    directory =
      Path.join(System.tmp_dir!(), "catena-env-test-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    File.write!(Path.join(directory, "read"), "abc")
    File.ln_s!("/etc/passwd", Path.join(directory, "link"))

    commands = %{
      "echo" => %{
        executable: System.find_executable("cat"),
        arguments: [],
        environment: %{},
        cwd: directory
      },
      "wait" => %{
        executable: System.find_executable("sleep"),
        arguments: ["10"],
        environment: %{},
        cwd: directory
      }
    }

    grants = [
      grant(:filesystem, [:read, :write], %{root: directory, paths: ["read", "write", "link"]}),
      grant(:process, [:run], %{commands: commands})
    ]

    try do
      E.run(grants, @limits, fn bundle ->
        a = bundle.authorities
        assert {:ok, Schema.success("abc")} == E.call(a.filesystem, :read, {"read", 3})
        assert {:ok, Schema.success(:unit)} == E.call(a.filesystem, :write, {"write", "saved"})
        assert File.read!(Path.join(directory, "write")) == "saved"
        assert {:ok, {:catena_variant, :error, _}} = E.call(a.filesystem, :read, {"link", 10})
        assert {:ok, Schema.failure(:denied)} == E.call(a.filesystem, :read, {"../outside", 1})
        assert {:ok, Schema.success({0, "child"})} == E.call(a.process, :run, {"echo", "child"})
        {:ok, job} = E.start(a.process, :run, {"wait", <<>>})
        Process.sleep(50)
        assert :ok = E.cancel(job)
        assert {:ok, Schema.failure(:cancelled)} == E.await(job)
      end)
    after
      File.rm_rf!(directory)
    end
  end

  test "real TCP exchange uses only an explicitly granted numeric endpoint" do
    {:ok, listener} =
      :gen_tcp.listen(0, [:binary, active: false, ip: {127, 0, 0, 1}, reuseaddr: true])

    {:ok, {_, port}} = :inet.sockname(listener)

    server =
      Task.async(fn ->
        {:ok, socket} = :gen_tcp.accept(listener, 2000)
        {:ok, "ping"} = :gen_tcp.recv(socket, 4, 2000)
        :ok = :gen_tcp.send(socket, "pong")
        :gen_tcp.close(socket)
      end)

    try do
      g = grant(:network, [:exchange], %{endpoints: %{"echo" => {{127, 0, 0, 1}, port}}})

      E.run([g], @limits, fn bundle ->
        assert {:ok, Schema.success("pong")} ==
                 E.call(bundle.authorities.network, :exchange, {"echo", "ping", 4})

        assert {:ok, Schema.failure(:denied)} ==
                 E.call(bundle.authorities.network, :exchange, {"other", "ping", 4})
      end)

      Task.await(server)
    after
      :gen_tcp.close(listener)
    end
  end

  test "child cancellation and owner death reap the launched OS child before scope completion" do
    directory =
      Path.join(System.tmp_dir!(), "catena-env-reap-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    script = Path.join(directory, "child")
    pidfile = Path.join(directory, "pid")

    File.write!(
      script,
      "#!/usr/bin/python3\nimport os,time\nopen('pid','w').write(str(os.getpid()))\ntime.sleep(30)\n"
    )

    File.chmod!(script, 0o700)

    g =
      grant(:process, [:run], %{
        commands: %{
          "child" => %{executable: script, arguments: [], environment: %{}, cwd: directory}
        }
      })

    try do
      E.run([g], @limits, fn bundle ->
        {:ok, job} = E.start(bundle.authorities.process, :run, {"child", <<>>})
        pid = await_child(pidfile, 200)
        assert :ok = E.cancel(job)
        assert {:ok, Schema.failure(:cancelled)} == E.await(job)
        refute File.exists?("/proc/#{pid}")
      end)

      File.rm!(pidfile)
      parent = self()

      owner =
        spawn(fn ->
          E.run([g], @limits, fn bundle ->
            {:ok, _} = E.start(bundle.authorities.process, :run, {"child", <<>>})
            send(parent, {:child_manager, bundle.manager})

            receive do
              :stay -> :ok
            end
          end)
        end)

      assert_receive {:child_manager, manager}
      pid = await_child(pidfile, 200)
      monitor = Process.monitor(manager)
      Process.exit(owner, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^manager, :normal}, 3000
      refute File.exists?("/proc/#{pid}")
    after
      File.rm_rf!(directory)
    end
  end

  test "scripted wrong-service failures, oversized answers and forged completion messages are refused" do
    g = grant(:random, [:bytes])

    E.run(
      [g],
      @limits,
      fn bundle ->
        {:ok, job = {_, _, manager, reference}} = E.start(bundle.authorities.random, :bytes, 1)
        send(manager, {:finished, reference, make_ref(), Schema.success("forged")})
        assert {:ok, Schema.success("x")} == E.await(job)
        assert {:error, _} = E.call(bundle.authorities.random, :bytes, 1)
      end,
      fake: [
        {:random, :bytes, 1, Schema.success("x"), 10},
        {:random, :bytes, 1, Schema.failure(:network_failure), 0}
      ]
    )

    E.run(
      [g],
      %{nodes: 100, bytes: 20, depth: 20},
      fn bundle ->
        assert {:error, _} = E.call(bundle.authorities.random, :bytes, 1)
      end,
      fake: [{:random, :bytes, 1, Schema.success(:binary.copy("x", 100)), 0}]
    )

    assert {:error, :invalid_environment_scope} =
             E.run([g], @limits, fn _ -> flunk("entered") end, max_requests: 1, max_requests: 2)
  end

  test "scope traps still revoke authority and finish pending cleanup" do
    parent = self()

    assert catch_error(
             E.run([grant(:time, [:sleep])], @limits, fn bundle ->
               {:ok, _} = E.start(bundle.authorities.time, :sleep, 4000)
               send(parent, {:trapped_manager, bundle.manager})
               :erlang.error({:catena_trap, :witness})
             end)
           ) == {:catena_trap, :witness}

    assert_receive {:trapped_manager, manager}
    refute Process.alive?(manager)
  end

  test "unresponsive cooperative release is reported as mandatory cleanup failure" do
    reason =
      catch_error(
        E.run([grant(:time, [:sleep])], @limits, fn bundle ->
          {:ok, {_, _, _, reference}} = E.start(bundle.authorities.time, :sleep, 4000)
          worker = :sys.get_state(bundle.manager).workers[reference]
          # Fault injection: the owned worker cannot receive its cooperative cancel.
          true = :erlang.suspend_process(worker)
          :unit
        end)
      )

    assert {:catena_trap,
            {:mandatory_release_failed,
             {:environment_cleanup_failed, {:error, :unconfirmed_environment_cleanup}}}} = reason
  end

  defp await_child(_, 0), do: flunk("child did not write PID")

  defp await_child(path, remaining) do
    case File.read(path) do
      {:ok, value} when byte_size(value) > 0 ->
        String.to_integer(value)

      _ ->
        Process.sleep(5)
        await_child(path, remaining - 1)
    end
  end
end
