defmodule Catena.ResourceExecutionTest do
  use ExUnit.Case, async: false
  alias Catena.Kernel.{Parser, Stepper, Backend, Verifier}

  @tag obligations: ~w(RS-OBL-003 RS-OBL-004)
  test "normal completion and real enclosing-handler abandonment both release nested scopes" do
    for {name, clause, expected} <- [
          {"ResourceResume", "(resume next (var n))", 6},
          {"ResourceAbort", "77", 77}
        ] do
      core = fixture(name, clause)
      assert {:ok, ^expected, outcome} = Stepper.run(core, "main")
      assert releases(outcome) == [22, 11]
      assert {{:ok, ^expected}, [22, 11]} = beam(core)
    end
  end

  @tag obligations: ~w(RS-OBL-004 RS-OBL-005)
  test "a primary trap survives failed inner release and outer release still runs" do
    core =
      fixture("ResourcePrimaryTrap", "(resume next (var n))",
        body: "(sequence (request Trace mark 5) (trap 99))",
        release: "(trap 88)"
      )

    assert {:trap, 99, outcome} = Stepper.run(core, "main")
    assert releases(outcome) == [22, 11]
    assert {{:trap, 99}, [22, 11]} = beam(core)
  end

  @tag obligations: ~w(RS-OBL-005)
  test "failed mandatory release turns normal completion into a terminal failure" do
    core = fixture("ResourceReleaseTrap", "(resume next (var n))", release: "(trap 88)")
    assert {:trap, {:mandatory_release_failed, 88}, outcome} = Stepper.run(core, "main")
    assert releases(outcome) == [22, 11]

    assert {{:trap, {:mandatory_release_failed, 88}}, [22, 11]} =
             beam(core)
  end

  @tag obligations: ~w(RS-OBL-002)
  test "failed acquisition registers no release for that resource" do
    core = fixture("ResourceAcquisitionTrap", "(resume next (var n))", acquire: "(trap 66)")
    assert {:trap, 66, outcome} = Stepper.run(core, "main")
    assert releases(outcome) == [11]
    assert {{:trap, 66}, [11]} = beam(core)
  end

  @tag obligations: ~w(RS-OBL-008)
  test "blocked mandatory release has an explicit virtual deadline and bounded BEAM failure" do
    core =
      fixture("ResourceBlockedRelease", "(resume next (var n))",
        release: "(call (var spin) (var payload))",
        grace_ns: 1_000_000
      )

    assert {:ok, initial} = Stepper.initial(core, "main")
    first = until_releases(initial, 1)
    assert {:error, :release_deadline_not_due} = Stepper.expire_resource_release(first, 0)
    assert {:ok, first} = Stepper.advance_resource_clock(first, 1_000_000)
    assert {:ok, second} = Stepper.expire_resource_release(first, 0)
    second = until_releases(second, 2)
    assert {:ok, second} = Stepper.advance_resource_clock(second, 2_000_000)
    assert {:ok, terminal} = Stepper.expire_resource_release(second, 0)

    assert {:trap, {:mandatory_release_failed, :deadline_exhausted}, outcome} =
             Stepper.run_configuration(terminal)

    assert releases(outcome) == [22, 11]
    assert {{:trap, {:mandatory_release_failed, :deadline_exhausted}}, [22, 11]} = beam(core)
  end

  @tag obligations: ~w(RS-OBL-002 RS-OBL-004)
  test "abort during acquisition releases earlier resources without registering the abandoned one" do
    core = fixture("ResourceAcquireAbort", "77", acquire: "(request Trace mark 22)")
    assert {:ok, 77, outcome} = Stepper.run(core, "main")
    assert releases(outcome) == [11]
    assert {{:ok, 77}, [11]} = beam(core)
  end

  @tag obligations: ~w(RS-OBL-004 RS-OBL-005)
  test "a declining handler cannot hide a mandatory release failure" do
    core = fixture("ResourceAbortReleaseTrap", "77", release: "(trap 88)")
    assert {:trap, {:mandatory_release_failed, 88}, outcome} = Stepper.run(core, "main")
    assert releases(outcome) == [22, 11]
    assert {{:trap, {:mandatory_release_failed, 88}}, [22, 11]} = beam(core)
  end

  @tag obligations: ~w(RS-OBL-005)
  test "release cannot reenter acquisition in its closing scope" do
    core = fixture("ResourceReentry", "(resume next (var n))", reentrant: true)

    assert {:trap, {:mandatory_release_failed, :resource_cleanup_reentry}, outcome} =
             Stepper.run(core, "main")

    assert releases(outcome) == [22, 11]

    assert {{:trap, {:mandatory_release_failed, :resource_cleanup_reentry}}, [22, 11]} =
             beam(core)
  end

  @tag obligations: ~w(RS-OBL-003 RS-OBL-005)
  test "mandatory cleanup runs before the enclosing suffix can trap" do
    core =
      fixture("ResourceBeforeSuffix", "(resume next (var n))",
        release: "(trap 88)",
        suffix_trap: 99
      )

    assert {:trap, {:mandatory_release_failed, 88}, _} = Stepper.run(core, "main")
    assert {{:trap, {:mandatory_release_failed, 88}}, [22, 11]} = beam(core)
  end

  defp until_releases(configuration, count, budget \\ 1000)
  defp until_releases(_, _, 0), do: flunk("release boundary was not reached")

  defp until_releases(configuration, count, budget) do
    if length(releases(Stepper.outcome(configuration))) == count do
      configuration
    else
      assert {:ok, next} = Stepper.step(configuration, 0)
      until_releases(next, count, budget - 1)
    end
  end

  defp fixture(name, clause, options \\ []) do
    body = Keyword.get(options, :body, "(add 1 (request Trace mark 5))")
    release = Keyword.get(options, :release, "(unit)")
    acquire = Keyword.get(options, :acquire, "22")

    source = """
    (module #{name} (edition 0.1) (revision 0.1.8) (origin "test://resource/#{name}")
      (export value main)
      (effect Trace (operation mark (params Int) Int))
      (handler Echo (effect Trace) (input Int) (output Int)
        (return value (var value)) (operation mark (params (n Int)) (resume next) #{clause}))
      (def spin (signature (Fn Int (effects) Unit) (uses)) (fn (n Int) (call (var spin) (var n))))
      (def acquire (signature Int (uses)) #{acquire})
      (def release (signature (Fn Int (effects) Unit) (uses)) (fn (payload Int) #{release}))
      (def main (signature Int (uses)) (handle Echo #{body})))
    """

    assert {:ok, parsed} = Parser.parse(source)
    [spin, acquisition, release, main] = parsed.definitions

    release =
      if Keyword.get(options, :reentrant, false) do
        nested = %{
          tag: :resource_scope,
          acquire: %{tag: :integer, value: 0, span: main.span},
          release: release.expression,
          body: release.expression.body,
          grace_ns: 1_000_000_000,
          span: main.span
        }

        %{release | expression: %{release.expression | body: nested}}
      else
        release
      end

    scope = %{
      tag: :resource_scope,
      acquire: acquisition.expression,
      release: release.expression,
      body: main.expression.expression,
      grace_ns: Keyword.get(options, :grace_ns, 1_000_000_000),
      span: main.span
    }

    outer = %{
      scope
      | acquire: %{tag: :integer, value: 11, span: acquisition.expression.span},
        body: scope
    }

    outer =
      case Keyword.get(options, :suffix_trap) do
        nil ->
          outer

        reason ->
          %{
            tag: :sequence,
            first: outer,
            second: %{
              tag: :trap,
              expression: %{tag: :integer, value: reason, span: main.span},
              span: main.span
            },
            span: main.span
          }
      end

    parsed = %{
      parsed
      | definitions: [spin, %{main | expression: %{main.expression | expression: outer}}]
    }

    assert {:ok, core} = Catena.Resource.Kernel.check(parsed, %{"Trace" => "TraceFamily"})
    core
  end

  defp releases(outcome),
    do: for(%{label: :resource_release_started, payload: payload} <- outcome.trace, do: payload)

  defp beam(core) do
    assert :ok = Verifier.verify(core)
    # Exercise the verified production artifact boundary.
    assert {:ok, module, binary, _} =
             Backend.compile(core)

    assert {:module, ^module} = :code.load_binary(module, ~c"resource-experiment.beam", binary)

    try do
      {outcome, trace} =
        Catena.Effect.Runtime.capture_trace(fn ->
          try do
            {:ok, apply(module, :main, [])}
          catch
            :error, {:catena_trap, reason} -> {:trap, reason}
          end
        end)

      {outcome, for({:resource_release_started, payload} <- trace, do: payload)}
    after
      :code.delete(module)
      :code.purge(module)
    end
  end
end
