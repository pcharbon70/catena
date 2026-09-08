defmodule Catena.C047EffectsCompletionTest do
  use ExUnit.Case, async: false

  alias Catena.Comprehension
  alias Catena.Kernel.Stepper

  # These are source-order observations, independent of the elaborator. Each
  # request is handled locally and leaves an empty residual kernel effect row.
  # This evidence does not claim general outward-effect comprehension support.
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
  test "locally handled requests preserve nested source, filter, binding, and yield order" do
    source = fixture("C047LocalEffectOrder") |> elaborate!()

    # 312 is the false filter: its request occurs, but neither 412 nor 512
    # runs. 202 is the empty inner source: it occurs once and has no suffix.
    assert_success(source, @values, @events)
  end

  @tag obligations: ~w(LC-OBL-003 LC-OBL-004 LC-OBL-008 LC-OBL-012)
  test "an empty outer list still performs its locally handled source request once" do
    source = fixture("C047LocalEffectEmpty", outer: []) |> elaborate!()
    assert_success(source, [], [100])
  end

  for {position, edge, event} <- @failures do
    @tag obligations: ~w(LC-OBL-005 LC-OBL-008 LC-OBL-012)
    test "a trap after the locally handled #{edge} #{position} request abandons the suffix" do
      event = unquote(event)
      module = "C047LocalTrap#{unquote(position)}#{unquote(edge)}"
      source = fixture(module, trap_at: event) |> elaborate!()
      expected = Enum.take_while(@events, &(&1 != event)) ++ [event]

      assert {:ok, core} = Catena.check_kernel(source)
      assert {:trap, ^event, outcome} = Stepper.run(core, "main")
      assert outcome.root_status == :trapped
      assert request_events(outcome) == expected

      assert {{:trap, {:catena_trap, ^event}}, ^expected} = run_beam(source)
    end
  end

  @tag obligations: ~w(LC-OBL-005 LC-OBL-008 LC-OBL-012)
  test "failure inside a would-be false filter propagates instead of becoming skip" do
    source = fixture("C047LocalFalseFilterTrap", trap_at: 312) |> elaborate!()
    expected = [100, 201, 311, 411, 511, 312]

    assert {:ok, core} = Catena.check_kernel(source)
    assert {:trap, 312, outcome} = Stepper.run(core, "main")
    assert request_events(outcome) == expected
    assert {{:trap, {:catena_trap, 312}}, ^expected} = run_beam(source)
  end

  @tag obligations: ~w(LC-OBL-005 LC-OBL-008 LC-OBL-012)
  test "declining the local request handler changes that fragment without aborting the comprehension" do
    source = fixture("C047LocalDecline", decline_at: 312) |> elaborate!()

    # The handler declines its local request continuation and returns -1.
    # The filter uses that result and now accepts candidate 12. This is not
    # evidence for an outer handler aborting a whole comprehension.
    assert_success(
      source,
      [110, 120, 310, 320],
      [100, 201, 311, 411, 511, 312, 412, 512, 202, 203, 331, 431, 531, 332, 432, 532]
    )
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
      context: [{"outer_values", "(List Int)", ints(outer)}],
      qualifiers: [
        {:generator,
         [
           pattern: "(bind x)",
           element_type: "Int",
           source: marked("100", outer_source, trap_at),
           binds: [{"x", "Int"}]
         ]},
        {:generator,
         [
           pattern: "(bind y)",
           element_type: "Int",
           source: marked("(add 200 (var x))", inner_source, trap_at),
           binds: [{"y", "Int"}]
         ]},
        {:filter, [expr: marked("(add 300 (var y))", "(not_equal (var marker) 312)", trap_at)]},
        {:let,
         [
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
        (input Int)
        (output Int)
        (return result (var result))
        (operation mark (params (event Int)) (resume next)
          (let observed (call (var observe) (var event))
            (match (equal (var observed) #{decline_at})
              (case true -1)
              (case false (resume next (var observed)))))))
      """
    ]
  end

  defp marked(event, result, trap_at) do
    """
    (let marker (handle Echo (request Trace mark #{event}))
      (match (equal (var marker) #{trap_at})
        (case true (trap (var marker)))
        (case false #{result})))
    """
  end

  defp elaborate!(spec) do
    assert {:ok, source, _advisories} = Comprehension.elaborate(spec)
    source
  end

  defp assert_success(source, expected_values, expected_events) do
    assert {:ok, core} = Catena.check_kernel(source)
    assert {:ok, values, outcome} = Stepper.run(core, "main")
    assert outcome.root_status == :terminated
    assert flatten(values) == expected_values
    assert request_events(outcome) == expected_events

    assert {{:ok, beam_values}, traced_events} = run_beam(source)
    assert flatten(beam_values) == expected_values
    assert traced_events == expected_events
  end

  defp request_events(outcome) do
    for %{label: :request, effect: "Trace", operation: "mark", arguments: [event]} <-
          outcome.trace,
        do: event
  end

  defp run_beam(source) do
    assert {:ok, module, binary, _metadata} = Catena.compile_kernel(source)
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
