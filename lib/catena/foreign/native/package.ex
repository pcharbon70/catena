defmodule Catena.Foreign.Native.Package do
  @moduledoc "Signed native package envelope, separate from retained Catena governance formats."
  alias Catena.{CanonicalJCS, Governance.Crypto, OTP.Profile}
  @external_resource "priv/native/port_guardian.py"
  @guardian File.read!(@external_resource)
  @domain "catena:native-package:1\n"
  @keys ~w(format version language_revision kind module scheduler max_work_units timeout_ms files toolchain unsafe_obligations guardian)
  @port_obligations ~w(bounded-work direct-child-only process-isolation)
  @nif_obligations ~w(bounded-work gc-finalizer idempotent-close scheduler-correctness vm-crash-possible)

  def describe(kind, payloads, options) when kind in [:port, :nif] and is_map(payloads) do
    with {:ok, host} <- Profile.require_supported(),
         timeout when is_integer(timeout) and timeout > 0 and timeout <= 4_294_967_295 <-
           Keyword.get(options, :timeout_ms),
         work when is_integer(work) and work > 0 and work <= 9_007_199_254_740_991 <-
           Keyword.get(options, :max_work_units),
         true <- Keyword.get(options, :scheduler) == scheduler(kind),
         true <- Enum.sort(Map.keys(payloads)) == files(kind),
         true <-
           Enum.all?(payloads, fn {_, bytes} -> is_binary(bytes) and byte_size(bytes) > 0 end) do
      {:ok,
       %{
         "format" => "catena-native-package",
         "version" => "1",
         "language_revision" => "0.1.63",
         "kind" => Atom.to_string(kind),
         "module" => if(kind == :nif, do: "catena_native_service", else: nil),
         "scheduler" => Atom.to_string(scheduler(kind)),
         "max_work_units" => work,
         "timeout_ms" => timeout,
         "files" => Map.new(payloads, fn {path, bytes} -> {path, digest(bytes)} end),
         "toolchain" => Profile.digest(host),
         "unsafe_obligations" => obligations(kind),
         "guardian" => if(kind == :port, do: guardian_digest(), else: nil)
       }}
    else
      _ -> {:error, :invalid_native_package_description}
    end
  rescue
    _ -> {:error, :invalid_native_package_description}
  end

  def describe(_, _, _), do: {:error, :invalid_native_package_description}

  def signing_payload(description), do: @domain <> CanonicalJCS.encode(description)

  def assemble(description, payloads, publisher, signature) do
    %{description: description, payloads: payloads, publisher: publisher, signature: signature}
  end

  def verify(package, policy) do
    with true <-
           is_map(policy) and
             Enum.sort(Map.keys(policy)) == [
               :kinds,
               :max_package_bytes,
               :publishers,
               :unsafe_acknowledgements
             ],
         true <- is_integer(policy.max_package_bytes) and policy.max_package_bytes > 0,
         true <- is_list(policy.publishers) and package.publisher in policy.publishers,
         true <- Enum.sort(Map.keys(package)) == [:description, :payloads, :publisher, :signature],
         description <- package.description,
         true <- Enum.sort(Map.keys(description)) == Enum.sort(@keys),
         kind when kind in ["port", "nif"] <- description["kind"],
         true <- kind in policy.kinds,
         true <-
           Enum.all?(description["unsafe_obligations"], &(&1 in policy.unsafe_acknowledgements)),
         true <- is_map(package.payloads),
         true <- Enum.all?(package.payloads, fn {_, value} -> is_binary(value) end),
         true <-
           Enum.reduce(package.payloads, 0, fn {_, value}, total -> total + byte_size(value) end) <=
             policy.max_package_bytes,
         true <- Crypto.verify(signing_payload(description), package.publisher, package.signature),
         {:ok, ^description} <-
           describe(String.to_existing_atom(kind), package.payloads,
             scheduler: String.to_existing_atom(description["scheduler"]),
             timeout_ms: description["timeout_ms"],
             max_work_units: description["max_work_units"]
           ) do
      {:ok, %{package: package, digest: digest(signing_payload(description)), policy: policy}}
    else
      _ -> {:error, :native_package_admission_denied}
    end
  rescue
    _ -> {:error, :native_package_admission_denied}
  end

  def verify_ready(ready) do
    case verify(ready.package, ready.policy) do
      {:ok, ^ready} -> :ok
      _ -> {:error, :native_package_admission_denied}
    end
  rescue
    _ -> {:error, :native_package_admission_denied}
  end

  def decode_description(binary) do
    with {:ok, value} <- CanonicalJCS.decode(binary, canonical: true),
         true <-
           is_map(value) and value["format"] == "catena-native-package" and
             value["version"] == "1" do
      {:ok, value}
    else
      _ -> {:error, :invalid_native_package_description}
    end
  end

  def guardian_source, do: @guardian
  def guardian_digest, do: digest(@guardian)
  def obligations(:port), do: @port_obligations
  def obligations(:nif), do: @nif_obligations
  def scheduler(:port), do: :os_process
  def scheduler(:nif), do: :dirty_cpu
  def files(:port), do: ["service"]
  def files(:nif), do: ["catena_native_service.beam", "catena_native_service.so"]
  def digest(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
