defmodule Catena.C047CapabilityComprehensionTest do
  use ExUnit.Case, async: false

  alias Catena.Comprehension
  alias Catena.Kernel.Stepper

  # Versioned closed-capability core, with one handler enclosing the traversal.
  # Expected events are the contract-derived oracle, not inferred from lowering.
  @events [100, 201, 311, 411, 511, 312, 202, 203, 331, 431, 531, 332, 432, 532]
  @values [110, 310, 320]
  @failures [
    {:source, :first, 100},
    {:source, :last, 203},
    {:filter, :first, 311},
    {:filter, :last, 332},
    {:binding, :first, 411},
    {:binding, :last, 432},
    {:yield, :first, 511},
    {:yield, :last, 532}
  ]

  @tag obligations: ~w(LC-OBL-004 LC-OBL-005 LC-OBL-008 LC-OBL-012)
  test "enclosing-handler requests preserve nested source, filter, binding, and yield order" do
    source = fixture("C047CapabilityEffectOrder") |> elaborate!()

    # 312 is the false filter: its request occurs, but neither 412 nor 512
    # runs. 202 is the empty inner source: it occurs once and has no suffix.
    assert_success(source, @values, @events)
  end

  @tag obligations: ~w(LC-OBL-003 LC-OBL-004 LC-OBL-008 LC-OBL-012)
  test "an empty outer list still performs its enclosing-handler source request once" do
    source = fixture("C047CapabilityEffectEmpty", outer: []) |> elaborate!()
    assert_success(source, [], [100])
  end

  for {position, edge, event} <- @failures do
    @tag obligations: ~w(LC-OBL-005 LC-OBL-008 LC-OBL-012)
    test "a trap after the enclosing-handler #{edge} #{position} request abandons the suffix" do
      event = unquote(event)
      module = "C047CapabilityTrap#{unquote(position)}#{unquote(edge)}"
      source = fixture(module, trap_at: event) |> elaborate!()
      expected = Enum.take_while(@events, &(&1 != event)) ++ [event]

      core = source
      assert {:trap, ^event, outcome} = Stepper.run(core, "main")
      assert outcome.root_status == :trapped
      assert request_events(outcome) == expected

      assert {{:trap, {:catena_trap, ^event}}, ^expected} = run_beam(source)
    end
  end

  @tag obligations: ~w(LC-OBL-005 LC-OBL-008 LC-OBL-012)
  test "failure inside a would-be false filter propagates instead of becoming skip" do
    source = fixture("C047CapabilityFalseFilterTrap", trap_at: 312) |> elaborate!()
    expected = [100, 201, 311, 411, 511, 312]

    core = source
    assert {:trap, 312, outcome} = Stepper.run(core, "main")
    assert request_events(outcome) == expected
    assert {{:trap, {:catena_trap, 312}}, ^expected} = run_beam(source)
  end

  @tag obligations: ~w(LC-OBL-005 LC-OBL-008 LC-OBL-012)
  test "declining the enclosing request handler aborts the entire comprehension" do
    source = fixture("C047CapabilityDecline", decline_at: 312) |> elaborate!()

    # The whole traversal is abandoned at the false-filter request.
    assert_success(source, [], [100, 201, 311, 411, 511, 312])
  end

  @tag obligations: ~w(CK-OBL-006 LC-OBL-005)
  test "an enclosing handler can change the complete computation's result type" do
    spec = fixture("C047CapabilityCount")

    types =
      Enum.map(spec.types, fn declaration ->
        declaration
        |> String.replace("(output (List Int))", "(output Int)")
        |> String.replace("(return result (var result))", "(return result 0)")
        |> String.replace("(case true (construct Nil))", "(case true 0)")
        |> String.replace(
          "(case false (resume next (var observed)))",
          "(case false (add 1 (resume next (var observed))))"
        )
      end)

    core = elaborate!(%{spec | types: types})
    assert {:ok, 14, outcome} = Stepper.run(core, "main")
    assert request_events(outcome) == @events
    assert {{:ok, 14}, @events} = run_beam(core)
  end

  @tag obligations: ~w(CK-OBL-005)
  test "nonrecursive probes reject understated and overstated fragment rows" do
    spec = fixture("C047CapabilityRows")
    [{kind, fields} | rest] = spec.qualifiers
    understated = %{spec | qualifiers: [{kind, Keyword.put(fields, :uses, [])} | rest]}

    assert {:error, %{path: "$.qualifiers[0]"}} =
             Catena.Comprehension.Capability.check(understated, %{"Trace" => "TraceFamily"},
               handlers: ["Echo"]
             )

    pure_source = Keyword.put(fields, :source, "(construct Nil)")
    overstated = %{spec | qualifiers: [{kind, pure_source} | rest]}

    assert {:error, %{path: "$.qualifiers[0]"}} =
             Catena.Comprehension.Capability.check(overstated, %{"Trace" => "TraceFamily"},
               handlers: ["Echo"]
             )

    assert {:error, %{path: "$.uses"}} =
             Catena.Comprehension.Capability.check(
               %{spec | uses: []},
               %{"Trace" => "TraceFamily"},
               handlers: ["Echo"]
             )
  end

  @tag obligations: ~w(CK-OBL-005)
  test "context evaluation rows are checked before fragment use and recursive context cannot self-justify" do
    spec = fixture("C047CapabilityContext")
    bad = %{spec | context: [{"outer_values", "(List Int)", "(var outer_values)", ["Trace"]}]}

    assert {:error, %{path: "$.context.outer_values"}} =
             Catena.Comprehension.Capability.check(bad, %{"Trace" => "TraceFamily"},
               handlers: ["Echo"]
             )
  end

  @tag obligations: ~w(CK-OBL-005 LC-OBL-008)
  test "effectful contexts run at each reached reference in dependency order" do
    spec = fixture("C047ContextOrder")
    [{kind, fields} | suffix] = spec.qualifiers
    source = marked("100", "(let ignored (var outer_values) (var outer_values))", -1)

    spec = %{
      spec
      | context: [
          {"seed", "Int", marked("80", "1", -1), ["Trace"]},
          {"outer_values", "(List Int)",
           marked(
             "90",
             "(construct Cons (var seed) (construct Cons 2 (construct Cons 3 (construct Nil))))",
             -1
           ), ["Trace"]},
          {"unused", "Int", marked("99", "0", -1), ["Trace"]}
        ],
        qualifiers: [{kind, Keyword.put(fields, :source, source)} | suffix]
    }

    assert_success(elaborate!(spec), @values, [100, 90, 80, 90, 80 | tl(@events)])
  end

  @tag obligations: ~w(CK-OBL-005 LC-OBL-008)
  test "a context failure stops before dependencies and traversal suffix" do
    spec = fixture("C047ContextTrap")

    spec = %{
      spec
      | context: [{"outer_values", "(List Int)", marked("90", ints([1, 2, 3]), 90), ["Trace"]}]
    }

    core = elaborate!(spec)
    assert {:trap, 90, outcome} = Stepper.run(core, "main")
    assert request_events(outcome) == [100, 90]
    assert {{:trap, {:catena_trap, 90}}, [100, 90]} = run_beam(core)
  end

  @tag obligations: ~w(LC-OBL-004 LC-OBL-006 LC-OBL-008)
  test "dependent filtering patterns preserve source effects and skip only their suffix" do
    spec = fixture("C047DependentCase", outer: [1, 2])
    [outer | _] = spec.qualifiers

    candidates =
      "(construct Cons (construct None) (construct Cons (construct Some (var x)) (construct Cons (construct Some (add (var x) 10)) (construct Nil))))"

    spec = %{
      spec
      | types: [
          "(data Option (params a) (constructor None (fields)) (constructor Some (fields a)))"
          | spec.types
        ],
        qualifiers: [
          outer,
          {:case_generator,
           [
             uses: ["Trace"],
             pattern: "(constructor Some (bind y))",
             element_type: "(Option Int)",
             source: marked("(add 200 (var x))", candidates, -1),
             binds: [{"y", "Int"}]
           ]},
          {:filter,
           [
             uses: ["Trace"],
             expr: marked("(add 300 (var y))", "(not_equal (var marker) 312)", -1)
           ]}
        ],
        yield: marked("(add 400 (var y))", "(var y)", -1)
    }

    assert_success(elaborate!(spec), [1, 11, 2], [
      100,
      201,
      301,
      401,
      311,
      411,
      202,
      302,
      402,
      312
    ])
  end

  @tag obligations: ~w(LC-OBL-006 LC-OBL-008)
  test "totality advisories use the selected effectful target" do
    spec = fixture("C047CapabilityTotalMarker")
    [{:generator, fields} | rest] = spec.qualifiers
    spec = %{spec | qualifiers: [{:case_generator, fields} | rest]}

    assert {:ok, core, advisories} =
             Catena.Comprehension.Capability.check(spec, %{"Trace" => "TraceFamily"},
               handlers: ["Echo"]
             )

    assert [%{id: "LCP003", path: "$.qualifiers[0]"}] = advisories
    assert_success(core, @values, @events)
  end

  defp fixture(module, options \\ []) do
    trap_at = Keyword.get(options, :trap_at, -99_999)
    decline_at = Keyword.get(options, :decline_at, -1)
    outer = Keyword.get(options, :outer, [1, 2, 3])

    outer_source = """
    (match (equal (var marker) 100)
      (case true (var outer_values))
      (case false (construct Nil)))
    """

    inner_source = """
    (let current (subtract (var marker) 200)
      (match (equal (var current) 2)
        (case true (construct Nil))
        (case false
          (construct Cons (add (multiply (var current) 10) 1)
            (construct Cons (add (multiply (var current) 10) 2) (construct Nil))))))
    """

    Comprehension.new(
      module: module,
      origin: "test://c047/local-effects/#{module}",
      types: declarations(decline_at),
      uses: ["Trace"],
      yield_uses: ["Trace"],
      context: [{"outer_values", "(List Int)", ints(outer)}],
      qualifiers: [
        {:generator,
         [
           uses: ["Trace"],
           pattern: "(bind x)",
           element_type: "Int",
           source: marked("100", outer_source, trap_at),
           binds: [{"x", "Int"}]
         ]},
        {:generator,
         [
           uses: ["Trace"],
           pattern: "(bind y)",
           element_type: "Int",
           source: marked("(add 200 (var x))", inner_source, trap_at),
           binds: [{"y", "Int"}]
         ]},
        {:filter,
         [
           uses: ["Trace"],
           expr: marked("(add 300 (var y))", "(not_equal (var marker) 312)", trap_at)
         ]},
        {:let,
         [
           uses: ["Trace"],
           pattern: "(bind z)",
           value_type: "Int",
           expr:
             marked("(add 400 (var y))", "(multiply (subtract (var marker) 400) 10)", trap_at),
           binds: [{"z", "Int"}]
         ]}
      ],
      yield:
        marked(
          "(add 500 (var y))",
          "(add (var z) (subtract (var marker) (add 500 (var y))))",
          trap_at
        ),
      result_element_type: "Int"
    )
  end

  defp declarations(decline_at) do
    [
      "(effect Trace (operation mark (params Int) Int))",
      "(export value observe)",
      "(def observe (signature (Fn Int (effects) Int) (uses)) (fn (event Int) (var event)))",
      """
      (handler Echo
        (effect Trace)
        (input (List Int))
        (output (List Int))
        (return result (var result))
        (operation mark (params (event Int)) (resume next)
          (let observed (call (var observe) (var event))
            (match (equal (var observed) #{decline_at})
              (case true (construct Nil))
              (case false (resume next (var observed)))))))
      """
    ]
  end

  defp marked(event, result, trap_at) do
    """
    (let marker (request Trace mark #{event})
      (match (equal (var marker) #{trap_at})
        (case true (trap (var marker)))
        (case false #{result})))
    """
  end

  defp elaborate!(spec) do
    assert {:ok, core, _advisories} =
             Catena.Comprehension.Capability.check(spec, %{"Trace" => "TraceFamily"},
               handlers: ["Echo"]
             )

    core
  end

  defp assert_success(source, expected_values, expected_events) do
    core = source
    assert {:ok, values, outcome} = Stepper.run(core, "main")
    assert outcome.root_status == :terminated
    assert flatten(values) == expected_values
    assert request_events(outcome) == expected_events

    assert {{:ok, beam_values}, traced_events} = run_beam(source)
    assert flatten(beam_values) == expected_values
    assert traced_events == expected_events
  end

  defp request_events(outcome) do
    for %{label: :request, operation: "mark", arguments: [event]} <-
          outcome.trace,
        do: event
  end

  defp run_beam(source) do
    assert :ok = Catena.Kernel.Verifier.verify(source)

    assert {:ok, module, binary, %{artifact_version: "0.1.50"}} =
             Catena.Kernel.Backend.compile(source)

    assert {:module, ^module} = :code.load_binary(module, ~c"c047_effect_evidence.beam", binary)

    parent = self()

    {pid, monitor} =
      spawn_monitor(fn ->
        receive do
          :run ->
            outcome =
              try do
                {:ok, apply(module, :main, [])}
              catch
                kind, reason -> {kind, reason}
              end

            send(parent, {:c047_finished, self(), outcome})
            receive do: (:stop -> :ok)
        end
      end)

    session = :trace.session_create(:c047_effect_evidence, self(), [])

    try do
      assert 1 == :trace.process(session, pid, true, [:call])
      assert 1 == :trace.function(session, {module, :observe, 1}, [], [:local])
      send(pid, :run)
      {outcome, reversed_events} = collect_run(pid, module, [])
      barrier = :trace.delivered(session, pid)
      events = collect_delivered(pid, module, barrier, reversed_events) |> Enum.reverse()

      outcome =
        case outcome do
          {:error, {:catena_trap, _} = reason} -> {:trap, reason}
          other -> other
        end

      {outcome, events}
    after
      :trace.session_destroy(session)
      Process.exit(pid, :kill)
      assert_receive {:DOWN, ^monitor, :process, ^pid, _reason}, 5_000
      :code.delete(module)
      :code.purge(module)
    end
  end

  defp collect_run(pid, module, events) do
    receive do
      {:trace, ^pid, :call, {^module, :observe, [event]}} ->
        collect_run(pid, module, [event | events])

      {:c047_finished, ^pid, outcome} ->
        {outcome, events}
    after
      5_000 -> flunk("compiled comprehension did not finish")
    end
  end

  defp collect_delivered(pid, module, barrier, events) do
    receive do
      {:trace, ^pid, :call, {^module, :observe, [event]}} ->
        collect_delivered(pid, module, barrier, [event | events])

      {:trace_delivered, ^pid, ^barrier} ->
        events
    after
      5_000 -> flunk("BEAM trace delivery barrier was not acknowledged")
    end
  end

  defp ints(values) do
    Enum.reduce(Enum.reverse(values), "(construct Nil)", fn value, tail ->
      "(construct Cons #{value} #{tail})"
    end)
  end

  defp flatten({:catena_constructor, :Nil, {}}), do: []
  defp flatten({:catena_constructor, :Cons, {head, tail}}), do: [head | flatten(tail)]
end
