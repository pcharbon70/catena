defmodule Catena.CollectionIteratorTest.Host do
  def pull(state, _) do
    send(__MODULE__, {:pull, state})

    if state == 3,
      do: {:catena_variant, :end, state},
      else: {:catena_variant, :yield, {state * 10, state + 1}}
  end

  def release(state, _) do
    send(__MODULE__, {:released, state})
    :unit
  end

  def fail_release(state, _) do
    send(__MODULE__, {:released, state})
    :erlang.error({:catena_trap, :release_failed})
  end

  def slow(state, control) do
    send(__MODULE__, {:pull, state})
    Process.sleep(50)
    Catena.Foreign.Control.checkpoint(control)
    {:catena_variant, :yield, {state, state + 1}}
  end
end

defmodule Catena.CollectionIteratorTest do
  use ExUnit.Case, async: false
  alias Catena.Standard.Collections.Iterator, as: I
  alias Catena.Standard.Outcomes, as: O
  alias Catena.Foreign.{Codec, Descriptor}
  alias Catena.CollectionIteratorTest.Host
  @limits %{nodes: 1000, bytes: 10000, depth: 100}
  @moduletag obligations: ~w(CL-OBL-007 CL-OBL-008 CL-OBL-009 CL-OBL-010)
  setup do
    Process.register(self(), Host)
    :ok
  end

  defp description(pull \\ :pull, release \\ :release) do
    {:ok, state} = Codec.new({:data, :integer})

    {:ok, step} =
      Codec.new(
        {:data, {:variant, %{"end" => :integer, "yield" => {:tuple, [:integer, :integer]}}}}
      )

    {:ok, unit} = Codec.new({:data, :unit})
    opts = [trust: :trusted_beam, scheduler: :owned_process, cancellation: :cooperative]
    {:ok, pull} = Descriptor.new({Host, pull}, [state], step, "test://pull", opts)
    {:ok, release} = Descriptor.new({Host, release}, [state], unit, "test://release", opts)
    {:ok, d} = I.describe(pull, release, :integer, :integer)
    {d, [pull, release]}
  end

  test "demand controls pulls; end and explicit close release once with the last accepted state" do
    {d, grants} = description()

    handle =
      I.run(d, 0, grants, @limits, fn handle ->
        refute_received {:pull, _}
        assert {:ok, present} = I.next(handle)
        assert present == O.present(0)
        assert_received {:pull, 0}
        refute_received {:pull, _}
        assert {:ok, [10, 20]} = I.collect(handle, 10)
        assert_received {:pull, 1}
        assert_received {:pull, 2}
        assert_received {:pull, 3}
        assert_received {:released, 3}
        assert {:ok, absent} = I.next(handle)
        assert absent == O.absent()
        assert :ok = I.close(handle)
        assert :ok = I.close(handle)
        handle
      end)

    assert {:error, :expired_iterator} = I.next(handle)
    refute_received {:released, _}
  end

  test "early truncation and abandonment release without pulling the unused suffix" do
    {d, grants} = description()
    assert {:ok, [0]} = I.run(d, 0, grants, @limits, &I.collect(&1, 1))
    assert_received {:pull, 0}
    assert_received {:released, 1}
    refute_received {:pull, _}
    assert :done = I.run(d, 2, grants, @limits, fn _ -> :done end)
    assert_received {:released, 2}
    refute_received {:pull, _}
  end

  test "owner identity and exact authority are enforced before advancing" do
    {d, grants} = description()

    assert {:error, :pull_authority_denied} =
             I.run(d, 0, [], @limits, fn _ -> flunk("not reached") end)

    I.run(d, 0, grants, @limits, fn handle ->
      task = Task.async(fn -> I.next(handle) end)
      assert {:error, :invalid_iterator_owner} = Task.await(task)
      refute_received {:pull, _}
    end)

    assert_received {:released, 0}
  end

  test "owner death releases the last state" do
    {d, grants} = description()
    test = self()

    {owner, monitor} =
      spawn_monitor(fn ->
        I.run(d, 1, grants, @limits, fn handle ->
          assert {:ok, _} = I.next(handle)
          send(test, :ready)

          receive do
            :never -> :ok
          end
        end)
      end)

    assert_receive :ready
    Process.exit(owner, :kill)
    assert_receive {:DOWN, ^monitor, :process, ^owner, :killed}
    assert_receive {:released, 2}
    refute_received {:released, _}
  end

  test "step exhaustion and timed-out pulls close admission and still attempt release" do
    {d, grants} = description()

    I.run(
      d,
      0,
      grants,
      @limits,
      fn handle ->
        assert {:ok, _} = I.next(handle)
        assert {:error, :pull_step_limit} = I.next(handle)
        assert {:error, :closed_iterator} = I.next(handle)
      end,
      max_steps: 1
    )

    assert_received {:released, 1}
    {d, grants} = description(:slow)

    I.run(
      d,
      0,
      grants,
      @limits,
      fn handle ->
        assert {:error, :pull_deadline} = I.next(handle)
        assert {:error, :closed_iterator} = I.next(handle)
      end,
      timeout_ms: 10
    )

    assert_received {:released, 0}
  end

  test "release failure is a mandatory cleanup trap and is never retried" do
    {d, grants} = description(:pull, :fail_release)

    assert {:catena_trap, {:mandatory_release_failed, _}} =
             catch_error(
               I.run(d, 0, grants, @limits, fn handle ->
                 assert {:error, {:pull_outcome, {:trap, :release_failed}}} = I.close(handle)
                 :done
               end)
             )

    assert_received {:released, 0}
    refute_received {:released, _}
  end

  test "a compiled pure consumer stops without another pull" do
    {d, grants} = description()
    type = "(Variant (row (field continue Int) (field stop Int)))"

    {:ok, core} =
      Catena.check_kernel("""
      (module PullStop (edition 0.1) (revision 0.1.8) (origin "test://pull-stop")
        (export value work)
        (def work (signature (Fn (Tuple Int Int) (effects) #{type}) (uses))
          (fn (value (Tuple Int Int)) (annotate (inject stop 99) #{type}))))
      """)

    {:ok, artifact} = Catena.Calling.Artifact.build(core)
    {:ok, input} = Codec.new({:data, {:tuple, [:integer, :integer]}})
    {:ok, output} = Codec.new({:data, {:variant, %{"continue" => :integer, "stop" => :integer}}})

    {:ok, callback} =
      Catena.Foreign.Callback.new(artifact, core, "work", [], input, output, @limits)

    try do
      assert {:ok, 99} = I.run(d, 0, grants, @limits, &I.fold_while(&1, callback, 0, :integer))
      assert_received {:pull, 0}
      assert_received {:released, 1}
      refute_received {:pull, _}
    after
      :code.delete(artifact.module)
      :code.purge(artifact.module)
    end
  end
end
