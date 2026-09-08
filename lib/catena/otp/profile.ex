defmodule Catena.OTP.Profile do
  @moduledoc "Explicit tested toolchain matrix and required compiler/runtime facilities."
  alias Catena.{CanonicalJSON, Diagnostic}

  @tested %{
    "otp" => "29.0.4",
    "erts" => "17.0.4",
    "elixir" => "1.20.2",
    "compiler" => "10.0.3",
    "stdlib" => "8.0.3",
    "kernel" => "11.0.3",
    "architecture" => "x86_64-pc-linux-gnu",
    "word_bytes" => 8,
    "os" => "unix/linux",
    "emulator" => "jit",
    "build" => "opt"
  }
  @required ~w(noenv_forms aliases monotonic_time monitored_spawn)

  def supported, do: [@tested]

  def observe do
    fingerprint = %{
      "otp" => otp_version(),
      "erts" => to_string(:erlang.system_info(:version)),
      "elixir" => System.version(),
      "compiler" => app_version(:compiler),
      "stdlib" => app_version(:stdlib),
      "kernel" => app_version(:kernel),
      "architecture" => to_string(:erlang.system_info(:system_architecture)),
      "word_bytes" => :erlang.system_info(:wordsize),
      "emulator" => Atom.to_string(:erlang.system_info(:emu_flavor)),
      "build" => Atom.to_string(:erlang.system_info(:emu_type)),
      "os" => :os.type() |> Tuple.to_list() |> Enum.map_join("/", &Atom.to_string/1)
    }

    :code.ensure_loaded(:compile)

    %{
      fingerprint: fingerprint,
      features: %{
        "noenv_forms" => function_exported?(:compile, :noenv_forms, 2),
        "aliases" => alias_probe(),
        "monotonic_time" => clock_probe(),
        "monitored_spawn" => spawn_probe()
      }
    }
  end

  def validate(%{fingerprint: fingerprint, features: features}) do
    missing = Enum.reject(@required, &(Map.get(features, &1) == true))

    cond do
      fingerprint not in supported() ->
        {:error,
         Diagnostic.new("OTP001", "toolchain is not in the tested support matrix",
           details: %{observed: fingerprint, supported: supported()}
         )}

      missing != [] ->
        {:error,
         Diagnostic.new("OTP002", "required OTP facilities are unavailable",
           details: %{missing: missing}
         )}

      true ->
        :ok
    end
  end

  def require_supported do
    observed = observe()
    with :ok <- validate(observed), do: {:ok, observed.fingerprint}
  end

  def document do
    observed = observe()

    %{
      "policy" => %{
        "binary_reuse" => "exact-tested-fingerprint",
        "untagged_artifacts" => "rebuild-from-retained-source",
        "retirement_minimum_days" => 30,
        "retirement_minimum_subsequent_compiler_releases" => 1,
        "scheduled_retirements" => [],
        "urgent_withdrawal" => "explicit-reviewed-security-or-correctness-record"
      },
      "supported" => supported(),
      "observed" => observed.fingerprint,
      "features" => observed.features,
      "supported_host" => validate(observed) == :ok,
      "fingerprint" => digest(observed.fingerprint)
    }
  end

  def artifact_compatible(recorded, observed) do
    with :ok <- validate(observed),
         true <- recorded == observed.fingerprint do
      :ok
    else
      {:error, _} = error ->
        error

      _ ->
        {:error,
         Diagnostic.new(
           "OTP003",
           "artifact toolchain differs from the supported host; rebuild from retained source"
         )}
    end
  end

  def digest(fingerprint),
    do: :crypto.hash(:sha256, CanonicalJSON.encode(fingerprint)) |> Base.encode16(case: :lower)

  defp otp_version do
    path =
      Path.join([to_string(:code.root_dir()), "releases", System.otp_release(), "OTP_VERSION"])

    case File.read(path) do
      {:ok, value} -> String.trim(value)
      _ -> "unavailable"
    end
  end

  defp app_version(app) do
    :application.load(app)

    case :application.get_key(app, :vsn) do
      {:ok, version} -> to_string(version)
      _ -> "unavailable"
    end
  end

  defp alias_probe do
    if function_exported?(:erlang, :alias, 0) and function_exported?(:erlang, :unalias, 1) do
      alias = :erlang.alias()

      try do
        send(alias, {alias, :catena_probe})

        receive do
          {^alias, :catena_probe} -> true
        after
          0 -> false
        end
      after
        :erlang.unalias(alias)
      end
    else
      false
    end
  end

  defp clock_probe do
    is_integer(System.monotonic_time(:nanosecond))
  rescue
    _ -> false
  end

  defp spawn_probe do
    {pid, monitor} = spawn_monitor(fn -> :ok end)

    receive do
      {:DOWN, ^monitor, :process, ^pid, :normal} -> true
    end
  end
end
