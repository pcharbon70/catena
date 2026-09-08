defmodule Catena.ResourceKernelTest do
  use ExUnit.Case, async: true
  alias Catena.Kernel.{Parser, Verifier, Backend}
  alias Catena.Resource.Kernel

  @tag obligations: ~w(RS-OBL-001 RS-OBL-006 RS-OBL-010)
  test "resource scope expressions have independently verified acquisition and release types" do
    parsed = fixture()
    assert {:ok, core} = Kernel.check(parsed, %{})
    assert core.version == "0.1.51"
    assert :ok = Verifier.verify(core)

    assert {:ok, module, binary,
            %{artifact_version: "0.1.51", interface: nil, interface_binary: nil}} =
             Backend.compile(core)

    assert {:ok, ^module, ^binary, _} = Backend.compile(core)
    assert Catena.LanguageVersion.resource_frontend_versions() == ["0.1.51"]
    assert Catena.LanguageVersion.kernel_frontend_versions() == ["0.1.8"]
    assert "0.1.51" not in Catena.LanguageVersion.interface_versions()

    assert {:error, %{id: "EDN001"}} =
             Kernel.check(parsed, %{},
               language_selection: %Catena.LanguageSelection{
                 edition: "0.1",
                 language_revision: "0.1.50",
                 previews: []
               }
             )

    for version <- ["0.1.8", "0.1.50"] do
      forged =
        Enum.reduce(
          [:version, :frontend_format, :frontend_version, :language_revision],
          core,
          &Map.put(&2, &1, version)
        )

      assert {:error, _} = Verifier.verify(forged)
    end

    [main] = core.definitions
    forged = %{core | definitions: [%{main | expression: %{main.expression | grace_ns: -1}}]}
    assert {:error, _} = Verifier.verify(forged)
  end

  @tag obligations: ~w(RS-OBL-001)
  test "a release must consume the acquired payload and return Unit without a latent effect" do
    parsed = fixture()
    [main] = parsed.definitions
    bad_release = %{main.expression.release | parameter_type: :boolean}

    bad = %{
      parsed
      | definitions: [%{main | expression: %{main.expression | release: bad_release}}]
    }

    assert {:error, %{id: "T002"}} = Kernel.check(bad, %{})
    bad = %{parsed | definitions: [%{main | expression: %{main.expression | grace_ns: -1}}]}
    assert {:error, %{id: "T002"}} = Kernel.check(bad, %{})
  end

  @tag obligations: ~w(RS-OBL-010)
  test "a residual capability prevents production entry emission" do
    source = """
    (module OpenResource (edition 0.1) (revision 0.1.8) (origin "test://resource/open")
      (export value main)
      (effect Trace (operation mark (params Int) Int))
      (def main (signature Int (uses Trace)) (request Trace mark 7)))
    """

    assert {:ok, parsed} = Parser.parse(source)
    assert {:ok, core} = Kernel.check(parsed, %{"Trace" => "trace"})
    assert {:error, %{id: "EFX003"}} = Backend.compile(core)
  end

  defp fixture do
    source = """
    (module ResourceProbe (edition 0.1) (revision 0.1.8) (origin "test://resource/probe")
      (export value main)
      (def acquire (signature Int (uses)) 7)
      (def release (signature (Fn Int (effects) Unit) (uses)) (fn (payload Int) (unit)))
      (def main (signature Int (uses)) 42))
    """

    {:ok, parsed} = Parser.parse(source)
    [acquire, release, main] = parsed.definitions

    scope = %{
      tag: :resource_scope,
      acquire: acquire.expression,
      release: release.expression,
      body: main.expression,
      grace_ns: 1_000_000,
      span: main.span
    }

    %{parsed | definitions: [%{main | expression: scope}]}
  end
end
