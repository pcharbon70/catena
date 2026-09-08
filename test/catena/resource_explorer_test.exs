defmodule Catena.ResourceExplorerTest do
  use ExUnit.Case, async: true
  alias Catena.Resource.{Lifecycle, Explorer}

  @tag obligations: ~w(RS-OBL-007)
  test "bounded cancellation versus completion schedules produce one selected outcome" do
    {:ok, state} = Lifecycle.step(Lifecycle.new("owner"), {:open, "scope", 10})

    assert {:ok, %{terminals: terminals}} =
             Explorer.explore(state, [
               {:cancel, :stop},
               :safe_point,
               {:finish, {:ok, 4}}
             ])

    outcomes =
      MapSet.new(
        Enum.map(terminals, fn terminal ->
          assert [completion] = terminal.completed
          completion.outcome
        end)
      )

    assert outcomes == MapSet.new([{:ok, 4}, {:cancelled, :stop}])
  end

  @tag obligations: ~w(RS-OBL-008)
  test "release deadline, successful release and late cancellation have the exact bounded outcome set" do
    state =
      Enum.reduce(
        [{:open, "scope", 10}, {:begin_acquire, "r"}, {:acquired, "r"}, {:finish, {:ok, 4}}],
        Lifecycle.new("owner"),
        fn event, s ->
          {:ok, next} = Lifecycle.step(s, event)
          next
        end
      )

    assert {:ok, %{terminals: terminals}} =
             Explorer.explore(state, [
               {:advance, 10},
               :release_deadline,
               {:release_result, "r", :ok},
               {:cancel, :late}
             ])

    outcomes =
      MapSet.new(
        Enum.map(terminals, fn terminal ->
          assert [completion] = terminal.completed
          assert completion.primary == {:ok, 4}
          assert terminal.resources["r"].status in [:released, :release_failed]
          completion.outcome
        end)
      )

    assert outcomes ==
             MapSet.new([
               {:ok, 4},
               {:trap, {:mandatory_release_failed, {"r", :deadline_exhausted}}}
             ])

    assert {:exhausted, %{pending: n}} =
             Explorer.explore(state, [{:advance, 10}, {:cancel, :late}], transition_limit: 1)

    assert n > 0
  end
end
