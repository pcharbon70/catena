alias Catena.Foreign.Native
alias Catena.Foreign.Native.Package
[directory, mode] = System.argv()
payloads = Map.new(Package.files(:nif), &{&1, File.read!(Path.join(directory, &1))})
{:ok, description} = Package.describe(:nif, payloads, scheduler: :dirty_cpu, timeout_ms: 50, max_work_units: 1)
{pub, private} = :crypto.generate_key(:eddsa, :ed25519)
signature = :crypto.sign(:eddsa, :none, Package.signing_payload(description), [private, :ed25519])
package = Package.assemble(description, payloads, Base.encode16(pub, case: :lower), Base.encode16(signature, case: :lower))
policy = %{kinds: ["nif"], publishers: [package.publisher], max_package_bytes: 1_000_000, unsafe_acknowledgements: Package.obligations(:nif)}
if mode == "crash" do
 Native.run(package, policy, fn scope -> Native.call(scope, 91.0) end)
 raise "native abort did not abort"
end
:ok = Native.run(package, policy, fn scope ->
 Enum.each([0, 0x8000000000000000, 0x7fefffffffffffff, 1], fn bits ->
  <<value::float-64>> = <<bits::64>>
  {:ok, result} = Native.call(scope, value)
  true = <<result::float-64>> == <<bits::64>>
 end)
 {:error, :invalid_native_float} = Native.call(scope, 1)
 {:error, :native_call_failed} = Native.call(scope, 93.0)
 {:error, :native_call_failed} = Native.call(scope, 94.0)
 {:error, :invalid_native_result_or_scheduler} = Native.call(scope, 95.0)
 :ok = Native.close(scope)
 :ok = Native.close(scope)
 {:error, :closed_native_resource} = Native.call(scope, 2.0)
 :ok
end)
{1, _} = :catena_native_service.stats()
# Drop a resource without explicit close; the destructor supplies fallback.
{worker, monitor} = spawn_monitor(fn -> :catena_native_service.open(); :ok end)
receive do {:DOWN, ^monitor, :process, ^worker, :normal} -> :ok end
Enum.reduce_while(1..100, nil, fn _, _ ->
 case :catena_native_service.stats() do
  {2, d} when d >= 1 -> {:halt, :ok}
  _ -> Process.sleep(10); {:cont, nil}
 end
end) |> case do :ok -> :ok; _ -> raise "destructor did not close" end
{:error, {:native_timeout, :native_work_may_continue}} = Native.run(package, policy, fn scope ->
 result = Native.call(scope, 92.0)
 {:error, :native_scope_poisoned} = Native.call(scope, 1.0)
 result
end)
IO.puts("finite bits, dirty scheduler, rejection, double close, GC fallback, bounded timeout verified")
