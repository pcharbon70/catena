defmodule Catena.SecretTest.Host do
  def echo(bytes, _) do
    send(:persistent_term.get({__MODULE__, :observer}), {:credential_received, bytes})
    {bytes, Base.encode64(bytes)}
  end

  def fail(bytes, _), do: raise(bytes)

  def wait(bytes, control) do
    send(:persistent_term.get({__MODULE__, :observer}), {:credential_worker, self(), bytes})
    loop(control)
  end

  defp loop(control) do
    Catena.Foreign.Control.checkpoint(control)

    receive do
      :finish -> {<<>>, <<>>}
    after
      5 -> loop(control)
    end
  end
end

defmodule Catena.SecretTest do
  use ExUnit.Case, async: false
  import ExUnit.CaptureLog
  alias Catena.Runtime.Secret, as: S
  alias Catena.Runtime.Secret.{Ref, Program}
  alias Catena.Runtime.Environment.Policy
  @sentinel "synthetic-catena-credential-51d8"

  setup do
    :persistent_term.put({Catena.SecretTest.Host, :observer}, self())
    on_exit(fn -> :persistent_term.erase({Catena.SecretTest.Host, :observer}) end)
    :ok
  end

  defp input do
    {:ok, value} = S.input(@sentinel)
    value
  end

  defp recipient(function \\ :echo) do
    {:ok, bytes} = Catena.Foreign.Codec.new({:data, :bytes})
    {:ok, result} = Catena.Foreign.Codec.new({:data, {:tuple, [:bytes, :bytes]}})

    {:ok, d} =
      Catena.Foreign.Descriptor.new(
        {Catena.SecretTest.Host, function},
        [bytes],
        result,
        "test://secret-recipient",
        trust: :trusted_beam,
        scheduler: :owned_process,
        cancellation: :cooperative
      )

    %{kind: :foreign, declaration: d}
  end

  defp process_recipient(directory) do
    {:ok, grant} =
      Policy.new(:process, %{
        operations: [:run],
        max_bytes: 65536,
        ttl_ms: 5000,
        commands: %{
          "echo" => %{
            executable: System.find_executable("cat"),
            arguments: [],
            environment: %{},
            cwd: directory
          }
        }
      })

    %{kind: :environment, grant: grant, target: "echo", response_bytes: 0}
  end

  defp no_secret(value) do
    printed = inspect(value, limit: :infinity, printable_limit: :infinity)
    refute printed =~ @sentinel
    refute printed =~ Base.encode64(@sentinel)
    refute printed =~ Base.encode16(@sentinel, case: :lower)
  end

  test "opaque values stay sealed under transformations and nested recipient results" do
    S.run(%{"token" => input()}, %{"host" => recipient()}, fn scope ->
      {:ok, ref} = S.fetch(scope, "token")
      assert %Ref{} = ref
      no_secret(ref)
      {:ok, encoded} = S.derive(scope, {:base64, ref})
      {:ok, prefixed} = S.derive(scope, {:concat, {:public, "prefix:"}, encoded})
      assert {:ok, %Ref{} = reply} = S.deliver(scope, prefixed, "host")
      assert_receive {:credential_received, "prefix:" <> seen}
      assert seen == Base.encode64(@sentinel)
      {:ok, nested} = S.derive(scope, {:element, reply, 1})
      assert {:ok, %Ref{}} = S.deliver(scope, nested, "host")
      assert_receive {:credential_received, seen}
      assert seen == Base.encode64("prefix:" <> Base.encode64(@sentinel))

      for value <- [
            ref,
            encoded,
            prefixed,
            reply,
            nested,
            S.audit(scope),
            :sys.get_status(elem(scope, 2))
          ],
          do: no_secret(value)
    end)
  end

  test "diagnostics traces inspection and artifacts preserve sensitive markers" do
    secret = input()
    no_secret(secret)

    diagnostic =
      Catena.Diagnostic.new("SECRET001", "test",
        details: %{nested: [secret]},
        fixes: [%{value: secret}]
      )

    no_secret(Catena.Report.diagnostic(diagnostic))

    {_, trace} =
      Catena.Effect.Runtime.capture_trace(fn ->
        Catena.Effect.Runtime.trace({:nested, %{secret => [secret]}})
      end)

    no_secret(trace)
    assert {:error, :sensitive_artifact_input} = S.artifact_input(%{nested: [secret]})

    assert_raise ArgumentError, "sensitive artifact input", fn ->
      Catena.Assurance.build(
        %{package: "test", profile: "test", action: "build", secret: secret},
        [],
        [],
        nil
      )
    end

    assert {:ok, %{public: "metadata"}} = S.artifact_input(%{public: "metadata"})

    S.run(%{"token" => secret}, %{}, fn scope ->
      {:ok, ref} = S.fetch(scope, "token")
      assert {:error, :sensitive_artifact_input} = S.artifact_input(ref)
      no_secret(Catena.Report.diagnostic(%{diagnostic | details: %{ref: ref}}))
    end)
  end

  test "scope narrowing ownership forgery expiry and ancestor revocation are enforced" do
    escaped =
      S.run(%{"token" => input()}, %{"host" => recipient()}, fn scope ->
        {:ok, ref} = S.fetch(scope, "token")
        forged = %{ref | token: make_ref()}
        assert {:error, _} = S.deliver(scope, forged, "host")
        assert {:error, :secret_provider_denied} = S.fetch(scope, "other")
        assert {:error, _} = S.deliver(scope, ref, "other")
        {:ok, empty} = S.attenuate(scope, [], [], 5000)
        assert {:error, _} = S.derive(empty, {:base64, ref})
        assert {:error, _} = S.attenuate(empty, ["token"], ["host"], 5000)

        assert Task.async(fn -> S.fetch(scope, "token") end) |> Task.await() ==
                 {:error, :invalid_secret_owner}

        {:ok, child} = S.attenuate(scope, ["token"], ["host"], 5000)
        {:ok, child_ref} = S.fetch(child, "token")
        assert :ok = S.revoke(child)
        assert {:error, _} = S.deliver(scope, child_ref, "host")
        assert {:ok, %Ref{}} = S.deliver(scope, ref, "host")
        assert_receive {:credential_received, @sentinel}
        assert :ok = S.revoke(scope)
        assert {:error, _} = S.fetch(scope, "token")
        {scope, ref}
      end)

    {scope, ref} = escaped
    assert {:error, :expired_secret_scope} = S.deliver(scope, ref, "host")
  end

  test "derived secrets and pending delivery retain originating child revocation in a parent scope" do
    S.run(%{"token" => input()}, %{"host" => recipient(:wait)}, fn scope ->
      {:ok, child} = S.attenuate(scope, ["token"], ["host"], 5000)
      {:ok, ref} = S.fetch(child, "token")
      {:ok, parent_derived} = S.derive(scope, {:base64, ref})
      {:ok, job} = S.start(scope, parent_derived, "host")
      assert_receive {:credential_worker, _, bytes}
      assert bytes == Base.encode64(@sentinel)
      assert :ok = S.revoke(child)
      assert {:error, :revoked} = S.await(job)
      assert {:error, _} = S.derive(scope, {:hex, parent_derived})
      assert {:error, _} = S.deliver(scope, parent_derived, "host")
      assert {:ok, %Ref{}} = S.fetch(scope, "token")
    end)
  end

  test "completed recipient replies retain originating scope revocation" do
    S.run(%{"token" => input()}, %{"host" => recipient()}, fn scope ->
      {:ok, child} = S.attenuate(scope, ["token"], ["host"], 5000)
      {:ok, ref} = S.fetch(child, "token")
      {:ok, reply} = S.deliver(scope, ref, "host")
      assert_receive {:credential_received, @sentinel}
      {:ok, projected} = S.derive(scope, {:element, reply, 0})
      assert :ok = S.revoke(child)
      assert {:error, _} = S.derive(scope, {:element, reply, 0})
      assert {:error, _} = S.deliver(scope, projected, "host")
      refute_receive {:credential_received, _}
    end)
  end

  test "cancelled and crashing recipients never expose their result or exception payload" do
    logs =
      capture_log(fn ->
        S.run(
          %{"token" => input()},
          %{"fail" => recipient(:fail), "wait" => recipient(:wait)},
          fn scope ->
            {:ok, ref} = S.fetch(scope, "token")
            assert {:error, :secret_delivery_failed} = S.deliver(scope, ref, "fail")
            {:ok, job} = S.start(scope, ref, "wait")
            assert_receive {:credential_worker, worker, @sentinel}
            assert Process.info(worker, :dictionary) == {:dictionary, []}
            {:links, managers} = Process.info(worker, :links)
            for manager <- managers, do: no_secret(:sys.get_status(manager))
            assert :ok = S.cancel(job)
            assert {:error, :cancelled} = S.await(job)
            no_secret(S.audit(scope))
          end
        )
      end)

    no_secret(logs)
  end

  test "abnormal vault reports redact message state and reason, with no false cleanup success" do
    logs =
      capture_log(fn ->
        failure =
          catch_error(
            S.run(%{"token" => input()}, %{}, fn scope ->
              {:ok, _} = S.fetch(scope, "token")
              :sys.terminate(elem(scope, 2), {:synthetic_failure, @sentinel})
            end)
          )

        no_secret(failure)
        assert match?({:catena_trap, {:mandatory_release_failed, _}}, failure)
      end)

    no_secret(logs)
  end

  test "secret-context terminal failures and traces never retain unmarked exception payloads" do
    {failure, trace} =
      Catena.Effect.Runtime.capture_trace(fn ->
        catch_error(S.run(%{"token" => input()}, %{}, fn _ -> raise(@sentinel) end))
      end)

    assert failure == {:catena_trap, :secret_scope_failed}
    assert trace != []
    assert Enum.all?(trace, &(&1 == :secret_activity))
    no_secret(trace)
    no_secret(failure)
    refute S.context?()
  end

  test "explicit environment retrieval and private process stdin keep replies out of public output" do
    name = "CATENA_TEST_SECRET_PROVIDER"
    System.put_env(name, @sentinel)
    directory = System.tmp_dir!()

    {:ok, grant} =
      Policy.new(:environment, %{
        operations: [:get],
        names: [name],
        max_bytes: 65536,
        ttl_ms: 5000
      })

    try do
      S.run(
        %{"token" => %{kind: :environment, grant: grant, name: name}},
        %{"process" => process_recipient(directory), "host" => recipient()},
        fn scope ->
          {:ok, ref} = S.fetch(scope, "token")
          {:ok, reply} = Catena.Runtime.Environment.deliver_secret(scope, ref, "process")
          {:ok, output} = S.derive(scope, {:element, {:element, reply, 2}, 1})
          assert {:ok, %Ref{}} = Catena.Foreign.Adapter.deliver_secret(scope, output, "host")
          assert_receive {:credential_received, @sentinel}
          no_secret(S.audit(scope))
          no_secret(:sys.get_status(elem(scope, 2)))
        end
      )
    after
      System.delete_env(name)
    end
  end

  test "owner death stops pending credential work" do
    observer = self()
    r = recipient(:wait)
    secret = input()

    owner =
      spawn(fn ->
        S.run(%{"token" => secret}, %{"host" => r}, fn scope ->
          {:ok, ref} = S.fetch(scope, "token")
          {:ok, _} = S.start(scope, ref, "host")
          send(observer, {:vault, elem(scope, 2)})

          receive do
            :never -> :ok
          end
        end)
      end)

    assert_receive {:vault, vault}
    assert_receive {:credential_worker, worker, @sentinel}
    wm = Process.monitor(worker)
    vm = Process.monitor(vault)
    Process.exit(owner, :kill)
    assert_receive {:DOWN, ^wm, :process, ^worker, _}, 3000
    assert_receive {:DOWN, ^vm, :process, ^vault, :normal}, 3000
  end

  test "compiled credential entry uses references and cannot publish secret-dependent results" do
    {:ok, core} =
      Catena.Kernel.CapabilityKernel.check(
        """
        (module SecretCompiledEntry (edition 0.1) (revision 0.1.8) (origin "test://secret-compiled")
          (export value main)
          (effect Credential (operation get (params Int) Int) (operation encode (params Int) Int) (operation send (params Int Int) Unit))
          (def main (signature Unit (uses Credential)) (request Credential send 0 (request Credential encode (request Credential get 0)))))
        """,
        %{"Credential" => Program.family()}
      )

    [slot] = Map.keys(core.capabilities)

    {:ok, program} =
      Program.build(
        core,
        "main",
        %{slot => %{"get" => :fetch, "encode" => :base64, "send" => :deliver}},
        ["token"],
        ["host"]
      )

    no_secret(program.binary)

    try do
      assert {:ok, :unit} =
               S.run(
                 %{"token" => input()},
                 %{"host" => recipient()},
                 &Program.invoke(program, &1)
               )

      assert_receive {:credential_received, bytes}
      assert bytes == Base.encode64(@sentinel)

      assert {:error, :secret_program_failed} =
               S.run(%{}, %{"host" => recipient()}, &Program.invoke(program, &1))

      assert {:error, _} = Program.verify(%{program | binary: <<0>>})
    after
      :code.delete(program.module)
      :code.purge(program.module)
    end
  end

  test "expiry cancels pending work and public encoders refuse sensitive inputs without payload-bearing errors" do
    for encoder <- [&Catena.CanonicalJCS.encode/1, &Catena.CanonicalJSON.encode/1] do
      error =
        assert_raise ArgumentError, "sensitive artifact input", fn ->
          encoder.(%{"nested" => [input()]})
        end

      no_secret(Exception.message(error))
    end

    S.run(%{"token" => input()}, %{"host" => recipient(:wait)}, fn scope ->
      {:ok, child} = S.attenuate(scope, ["token"], ["host"], 50)
      {:ok, ref} = S.fetch(child, "token")
      {:ok, job} = S.start(scope, ref, "host")
      assert_receive {:credential_worker, _, @sentinel}
      assert {:error, :expired} = S.await(job)
      assert {:error, _} = S.derive(scope, {:hex, ref})
    end)
  end

  test "explicit loopback broker receives credentials while undeclared remote transport is refused" do
    {:ok, listener} = :gen_tcp.listen(0, [:binary, active: false, ip: {127, 0, 0, 1}])
    {:ok, {_, port}} = :inet.sockname(listener)
    owner = self()

    server =
      spawn(fn ->
        {:ok, socket} = :gen_tcp.accept(listener)
        {:ok, bytes} = :gen_tcp.recv(socket, byte_size(@sentinel), 3000)
        send(owner, {:broker_received, bytes})
        :gen_tcp.send(socket, "ok")
        :gen_tcp.close(socket)
      end)

    {:ok, g} =
      Policy.new(:network, %{
        operations: [:exchange],
        max_bytes: 4096,
        ttl_ms: 5000,
        endpoints: %{"broker" => {{127, 0, 0, 1}, port}}
      })

    recipient = %{kind: :environment, grant: g, target: "broker", response_bytes: 2}

    try do
      S.run(%{"token" => input()}, %{"broker" => recipient}, fn scope ->
        {:ok, ref} = S.fetch(scope, "token")
        assert {:ok, %Ref{}} = S.deliver(scope, ref, "broker")
        assert_receive {:broker_received, @sentinel}
        no_secret(S.audit(scope))
      end)

      {:ok, remote} =
        Policy.new(:network, %{g.policy | endpoints: %{"broker" => {{192, 0, 2, 1}, port}}})

      assert {:error, :invalid_secret_setup} =
               S.run(%{"token" => input()}, %{"broker" => %{recipient | grant: remote}}, fn _ ->
                 flunk("remote transport admitted")
               end)
    after
      :gen_tcp.close(listener)
      if Process.alive?(server), do: Process.exit(server, :kill)
    end
  end

  test "scoped sensitivity restores the caller flag and discloses residual host limits" do
    old = Process.flag(:sensitive, false)

    try do
      S.run(%{}, %{}, fn _ ->
        assert Process.flag(:sensitive, true)
        S.run(%{}, %{}, fn _ -> assert Process.flag(:sensitive, true) end)
        assert Process.flag(:sensitive, true)
      end)

      refute Process.flag(:sensitive, false)
      refute S.profile().secure_erasure
      refute S.profile().hostile_host_secrecy
    after
      Process.flag(:sensitive, old)
    end
  end

  test "diagnostic observation in a secret context uses a closed redacted record" do
    report =
      S.run(%{"token" => input()}, %{}, fn _ ->
        d = Catena.Diagnostic.new("OTHER", @sentinel, details: %{raw: Base.encode64(@sentinel)})
        assert d.id == "SEC001"
        Catena.Report.diagnostic(%{d | message: @sentinel})
      end)

    assert report.id == "SEC001"
    no_secret(report)
  end

  @tag timeout: 15000
  test "stalled secret delivery reaches its real deadline and reports unconfirmed cleanup" do
    started = System.monotonic_time(:millisecond)

    failure =
      catch_error(
        S.run(%{"token" => input()}, %{"host" => recipient(:wait)}, fn scope ->
          {:ok, ref} = S.fetch(scope, "token")
          {:ok, {:catena_secret_job, _, vault, token} = job} = S.start(scope, ref, "host")
          assert_receive {:credential_worker, _, @sentinel}
          # Privileged fixture inspection is outside the protected observation API.
          worker = :sys.get_state(vault).jobs[token].worker
          true = :erlang.suspend_process(worker)
          assert {:error, :secret_cleanup_unconfirmed} = S.await(job)
        end)
      )

    assert failure == {:catena_trap, {:mandatory_release_failed, :secret_cleanup_unconfirmed}}
    assert System.monotonic_time(:millisecond) - started >= 6500
    no_secret(failure)
  end

  test "exact storage transformation scope and setup limits refuse rather than truncate" do
    assert {:error, :secret_input_limit} = S.input(:binary.copy("x", 65537))

    assert {:error, :invalid_secret_setup} =
             S.run(%{"x" => "unclassified"}, %{}, fn _ -> flunk("entered") end)

    {:ok, large} = S.input(:binary.copy("x", 40000))

    S.run(%{"token" => large}, %{}, fn scope ->
      {:ok, ref} = S.fetch(scope, "token")
      assert {:error, :secret_derivation_denied} = S.derive(scope, {:hex, ref})
      for _ <- 1..63, do: assert({:ok, _} = S.attenuate(scope, [], [], 5000))
      assert {:error, :secret_attenuation_denied} = S.attenuate(scope, [], [], 5000)
    end)

    S.run(%{"token" => input()}, %{}, fn scope ->
      for _ <- 1..256, do: assert({:ok, %Ref{}} = S.fetch(scope, "token"))
      assert {:error, :secret_storage_limit} = S.fetch(scope, "token")
    end)
  end
end
