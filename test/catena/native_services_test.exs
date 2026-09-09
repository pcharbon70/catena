defmodule Catena.NativeServicesTest do
  use ExUnit.Case, async: false
  alias Catena.Foreign.Native
  alias Catena.Foreign.Native.Package

  @moduletag obligations:
               ~w(NI-OBL-001 NI-OBL-002 NI-OBL-003 NI-OBL-004 NI-OBL-005 NI-OBL-006 NI-OBL-007 NI-OBL-008)

  setup_all do
    directory =
      Path.join(System.tmp_dir!(), "catena-native-fixtures-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    on_exit(fn -> File.rm_rf!(directory) end)

    assert {_, 0} =
             System.cmd(
               "cc",
               [
                 "-std=c11",
                 "-Wall",
                 "-Wextra",
                 "-Werror",
                 "test/fixtures/native-service.c",
                 "-o",
                 Path.join(directory, "service")
               ],
               stderr_to_stdout: true
             )

    assert {_, 0} =
             System.cmd(
               "cc",
               [
                 "-std=c11",
                 "-Wall",
                 "-Wextra",
                 "-Werror",
                 "-fPIC",
                 "-shared",
                 "-I",
                 Path.join(to_string(:code.root_dir()), "usr/include"),
                 "test/fixtures/native-service-nif.c",
                 "-o",
                 Path.join(directory, "catena_native_service.so")
               ],
               stderr_to_stdout: true
             )

    {:ok, :catena_native_service, beam} =
      :compile.file(~c"test/fixtures/native-service.erl", [:binary])

    File.write!(Path.join(directory, "catena_native_service.beam"), beam)
    {:ok, directory: directory}
  end

  defp package(directory) do
    payloads = %{"service" => File.read!(Path.join(directory, "service"))}

    {:ok, description} =
      Package.describe(:port, payloads, scheduler: :os_process, timeout_ms: 50, max_work_units: 1)

    {pub, key} = :crypto.generate_key(:eddsa, :ed25519)
    signature = :crypto.sign(:eddsa, :none, Package.signing_payload(description), [key, :ed25519])

    package =
      Package.assemble(
        description,
        payloads,
        Base.encode16(pub, case: :lower),
        Base.encode16(signature, case: :lower)
      )

    policy = %{
      kinds: ["port"],
      publishers: [package.publisher],
      max_package_bytes: 1_000_000,
      unsafe_acknowledgements: Package.obligations(:port)
    }

    {package, policy}
  end

  test "signed package admission rejects missing trust, tampering, wrong scheduler and unsafe omissions",
       %{directory: directory} do
    {package, policy} = package(directory)
    assert {:ok, ready} = Catena.Package.Linker.link_native(package, policy)
    assert :ok = Package.verify_ready(ready)

    assert {:ok, _} =
             Catena.Package.Manifest.decode_native(
               Catena.CanonicalJCS.encode(package.description)
             )

    for changed <- [
          %{policy | publishers: []},
          %{policy | kinds: []},
          %{policy | max_package_bytes: 1},
          %{policy | unsafe_acknowledgements: []}
        ] do
      assert {:error, :native_package_admission_denied} = Package.verify(package, changed)
    end

    for changed <- [
          %{package | signature: ""},
          put_in(package.description["scheduler"], "normal"),
          put_in(package.payloads["service"], "changed")
        ] do
      assert {:error, :native_package_admission_denied} = Package.verify(changed, policy)
    end

    assert {:error, _} =
             Package.describe(:port, package.payloads,
               scheduler: :normal,
               timeout_ms: 1,
               max_work_units: 1
             )
  end

  test "bounded owned port preserves bits, refuses invalid values and confirms idempotent close",
       %{directory: directory} do
    {package, policy} = package(directory)

    assert :ok =
             Native.run(package, policy, fn scope ->
               for bits <- [0, 0x8000000000000000, 0x7FEFFFFFFFFFFFFF, 1] do
                 <<value::float-64>> = <<bits::64>>
                 assert {:ok, result} = Native.call(scope, value)
                 assert <<result::float-64>> == <<bits::64>>
               end

               assert {:error, :invalid_native_float} = Native.call(scope, self())
               assert {:error, :invalid_native_float} = Native.call(scope, 93.0)
               assert :ok = Native.close(scope)
               assert :ok = Native.close(scope)
               assert {:error, :closed_native_resource} = Native.call(scope, 1.0)
               :ok
             end)
  end

  test "scope authority expires and a killed owner releases its guardian", %{directory: directory} do
    {package, policy} = package(directory)
    owner = self()

    scope =
      Native.run(package, policy, fn {_, _, manager, _} = scope ->
        assert {:error, :invalid_native_authority} =
                 GenServer.call(manager, {:release, make_ref()})

        task = Task.async(fn -> Native.call(scope, 1.0) end)
        assert {:error, :invalid_native_owner} = Task.await(task)
        scope
      end)

    assert {:error, :expired_native_scope} = Native.call(scope, 1.0)

    worker =
      spawn(fn ->
        Native.run(package, policy, fn {_, _, manager, _} ->
          send(owner, {:manager, manager})

          receive do
            :never -> :ok
          end
        end)
      end)

    assert_receive {:manager, manager}, 3000
    monitor = Process.monitor(manager)
    state = :sys.get_state(manager)
    port = state.transport.handle
    Process.exit(worker, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^manager, :normal}, 3000
    assert Port.info(port) == nil
    refute File.exists?(state.transport.directory)
  end

  test "service death and deadline are isolated and reaped", %{directory: directory} do
    {package, policy} = package(directory)

    assert {:error, {:native_service_exit, 125}} =
             Native.run(package, policy, &Native.call(&1, 91.0))

    assert {:error, {:native_service_exit, 126}} =
             Native.run(package, policy, &Native.call(&1, 94.0))

    assert {:error, {:native_service_exit, 124}} =
             Native.run(package, policy, &Native.call(&1, 92.0))
  end

  test "NIF resource, scheduler and Float witnesses execute only in disposable VMs", %{
    directory: directory
  } do
    ebin = Path.expand(Path.join(Mix.Project.build_path(), "lib/catena/ebin"))

    args = [
      "20",
      "elixir",
      "--erl",
      "+S 2:2",
      "-pa",
      ebin,
      "test/fixtures/native-service-probe.exs",
      directory
    ]

    {output, status} = System.cmd("timeout", args ++ ["normal"], stderr_to_stdout: true)
    assert status == 0, output
    assert output =~ "bounded timeout verified"

    {output, status} =
      System.cmd("timeout", args ++ ["crash"],
        stderr_to_stdout: true,
        env: [{"ERL_CRASH_DUMP", Path.join(directory, "crash.dump")}]
      )

    assert status not in [0, 124], output
  end
end
