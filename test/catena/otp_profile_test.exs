defmodule Catena.OTPProfileTest do
  use ExUnit.Case, async: false
  alias Catena.OTP.{Compiler, Profile}

  defp forms do
    [
      {:attribute, 0, :module, CatenaOTPProfileProbe},
      {:attribute, 0, :export, [{:value, 0}]},
      {:function, 0, :value, 0, [{:clause, 0, [], [], [{:integer, 0, 42}]}]}
    ]
  end

  test "measured support row and active required-feature probes leave no private messages" do
    observed = Profile.observe()
    assert :ok = Profile.validate(observed)
    assert [observed.fingerprint] == Profile.supported()
    assert Enum.all?(Map.values(observed.features))
    assert Profile.document() == Profile.document()
    assert Process.info(self(), :messages) == {:messages, []}
  end

  test "unsupported or incomplete hosts are rejected with distinct diagnostics" do
    observed = Profile.observe()

    for {key, value} <- [
          {"otp", "28.0"},
          {"otp", "29.0.5"},
          {"erts", "unknown"},
          {"elixir", "other"},
          {"compiler", "other"},
          {"architecture", "other"},
          {"word_bytes", 4}
        ] do
      assert {:error, %{id: "OTP001"}} =
               Profile.validate(%{
                 observed
                 | fingerprint: Map.put(observed.fingerprint, key, value)
               })
    end

    for key <- Map.keys(observed.features) do
      assert {:error, %{id: "OTP002", details: %{missing: [^key]}}} =
               Profile.validate(%{observed | features: Map.put(observed.features, key, false)})
    end
  end

  test "artifacts record exact provenance and load only under the admitted host" do
    assert {:ok, module, binary, _} = Compiler.compile(forms())
    assert {:ok, ^module, ^binary, _} = Compiler.compile(forms())
    {:ok, {^module, [compile_info: info]}} = :beam_lib.chunks(binary, [:compile_info])
    observed = Profile.observe()
    assert info[:catena_toolchain] == observed.fingerprint
    assert info[:catena_toolchain_digest] == Profile.digest(observed.fingerprint)
    assert {:module, ^module} = Compiler.load(module, ~c"profile-probe.beam", binary)
    assert apply(module, :value, []) == 42
    :code.purge(module)
    :code.delete(module)

    assert {:error, %{id: "OTP003"}} =
             Profile.artifact_compatible(Map.put(observed.fingerprint, "otp", "29.0.5"), observed)

    assert {:error, %{id: "OTP001"}} =
             Profile.artifact_compatible(
               observed.fingerprint,
               %{observed | fingerprint: Map.put(observed.fingerprint, "otp", "29.0.5")}
             )

    assert {:error, %{id: "OTP003"}} = Compiler.load(module, ~c"bad.beam", <<0>>)
  end

  test "untagged host artifacts are rejected by entry launch without raising a match error" do
    {:ok, module, binary} = :compile.noenv_forms(forms(), [:binary])

    package = %{
      entries: [%{name: "value"}],
      entry_modules: %{"value" => %{module: module, binary: binary}}
    }

    assert {:error, %{id: "OTP003"}} = Catena.Entry.launch(package, "value")
    refute :code.is_loaded(module)
  end

  test "a separate VM accepts and executes the provenance-bearing artifact" do
    {:ok, _, binary, _} = Compiler.compile(forms())

    directory =
      Path.join(System.tmp_dir!(), "catena-otp-profile-#{System.unique_integer([:positive])}")

    File.mkdir_p!(directory)
    path = Path.join(directory, "probe.beam")
    File.write!(path, binary)
    on_exit(fn -> File.rm_rf!(directory) end)

    script = """
    [path] = System.argv()
    {:module, CatenaOTPProfileProbe} = Catena.OTP.Compiler.load(CatenaOTPProfileProbe, ~c"isolated-probe", File.read!(path))
    IO.write(CatenaOTPProfileProbe.value())
    """

    ebin = Profile |> :code.which() |> to_string() |> Path.dirname()

    assert {"42", 0} =
             System.cmd("elixir", ["--erl", "+S 2", "-pa", ebin, "-e", script, "--", path],
               stderr_to_stdout: true
             )
  end
end
