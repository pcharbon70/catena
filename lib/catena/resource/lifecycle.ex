defmodule Catena.Resource.Lifecycle do
  @moduledoc """
  Resource-lifetime transition oracle for the bounded 0.1.51 contract.

  Events describe already classified computation outcomes. This model does not
  execute callbacks, establish static handle safety, or promise cleanup after
  external loss. Runtime and checked-core execution are verified by separate behavioral tests.
  """

  def new(owner) when is_binary(owner) and owner != "" do
    %{
      owner: owner,
      clock: 0,
      stack: [],
      scopes: %{},
      resources: %{},
      release: nil,
      cancellation: nil,
      completed: [],
      trace: [],
      lost: false
    }
  end

  def step(%{lost: true}, _), do: {:error, :owner_lost}

  def step(state, {:open, id, grace})
      when is_binary(id) and id != "" and is_integer(grace) and grace >= 0 do
    if not Map.has_key?(state.scopes, id) and active?(state) do
      scope = %{
        id: id,
        grace: grace,
        phase: :open,
        acquired: [],
        pending: nil,
        primary: nil,
        errors: []
      }

      {:ok,
       record(
         %{state | stack: [id | state.stack], scopes: Map.put(state.scopes, id, scope)},
         {:opened, id}
       )}
    else
      {:error, :invalid_scope_open}
    end
  end

  def step(state, {:begin_acquire, id}) when is_binary(id) and id != "" do
    with {:ok, scope} <- top_open(state),
         true <- is_nil(scope.pending) and not Map.has_key?(state.resources, id) do
      resource = %{scope: scope.id, owner: state.owner, status: :acquiring}
      state = %{state | resources: Map.put(state.resources, id, resource)}
      {:ok, state |> put_scope(%{scope | pending: id}) |> record({:acquiring, id})}
    else
      _ -> {:error, :invalid_acquisition}
    end
  end

  def step(state, {:acquired, id}) do
    with {:ok, scope} <- top_open(state),
         true <- scope.pending == id,
         %{status: :acquiring} <- Map.get(state.resources, id) do
      {:ok,
       state
       |> put_resource(id, :acquired)
       |> put_scope(%{scope | pending: nil, acquired: [id | scope.acquired]})
       |> record({:acquired, id})}
    else
      _ -> {:error, :invalid_acquisition_completion}
    end
  end

  def step(state, {:acquire_failed, id, outcome}) do
    with {:ok, scope} <- top_open(state),
         true <- scope.pending == id and outcome?(outcome) and elem(outcome, 0) != :ok,
         %{status: :acquiring} <- Map.get(state.resources, id) do
      state = state |> put_resource(id, :acquisition_failed) |> put_scope(%{scope | pending: nil})
      close(record(state, {:acquisition_failed, id, outcome}), outcome)
    else
      _ -> {:error, :invalid_acquisition_failure}
    end
  end

  def step(state, {:use, owner, id}) do
    with {:ok, _} <- top_open(state),
         %{owner: ^owner, scope: scope, status: :acquired} <- Map.get(state.resources, id),
         true <- owner == state.owner and scope in state.stack do
      {:ok, record(state, {:used, id})}
    else
      _ -> {:error, :invalid_resource_use}
    end
  end

  def step(state, {:finish, outcome}), do: close(state, outcome)

  def step(state, {:cancel, reason}) do
    if is_nil(state.cancellation),
      do:
        {:ok,
         record(%{state | cancellation: {:cancelled, reason}}, {:cancellation_requested, reason})},
      else: {:ok, state}
  end

  def step(state, :safe_point) do
    with {:ok, scope} <- top_open(state), true <- is_nil(scope.pending) do
      if is_nil(state.cancellation), do: {:ok, state}, else: close(state, state.cancellation)
    else
      _ -> {:error, :invalid_safe_point}
    end
  end

  def step(%{release: %{id: id}} = state, {:release_result, id, result}) do
    case result do
      :ok -> release_finished(state, :released, nil)
      {:error, reason} -> release_finished(state, :release_failed, reason)
      _ -> {:error, :invalid_release_result}
    end
  end

  def step(state, {:advance, now}) when is_integer(now) and now >= state.clock,
    do: {:ok, %{state | clock: now}}

  def step(%{release: %{deadline: deadline}} = state, :release_deadline)
      when state.clock >= deadline,
      do: release_finished(state, :release_failed, :deadline_exhausted)

  def step(state, {:external_loss, kind}) when kind in [:process_kill, :vm_loss] do
    resources =
      Map.new(state.resources, fn {id, entry} ->
        if entry.status in [:acquiring, :acquired, :releasing],
          do: {id, %{entry | status: :lost}},
          else: {id, entry}
      end)

    {:ok,
     record(%{state | lost: true, release: nil, resources: resources}, {:external_loss, kind})}
  end

  def step(_, _), do: {:error, :invalid_transition}

  defp close(state, outcome) do
    with {:ok, scope} <- top_open(state), true <- is_nil(scope.pending) and outcome?(outcome) do
      scope = %{scope | phase: :closing, primary: outcome}
      {:ok, state |> put_scope(scope) |> record({:closing, scope.id, outcome}) |> next_release()}
    else
      _ -> {:error, :invalid_scope_completion}
    end
  end

  defp release_finished(state, status, error) do
    id = state.release.id
    scope = Map.fetch!(state.scopes, hd(state.stack))
    errors = if status == :released, do: scope.errors, else: scope.errors ++ [{id, error}]
    state = %{state | release: nil}

    {:ok,
     state
     |> put_resource(id, status)
     |> put_scope(%{scope | errors: errors})
     |> record({status, id, error})
     |> next_release()}
  end

  defp next_release(state) do
    scope = Map.fetch!(state.scopes, hd(state.stack))

    case scope.acquired do
      [id | rest] ->
        state = %{state | release: %{id: id, deadline: state.clock + scope.grace}}

        state
        |> put_scope(%{scope | acquired: rest})
        |> put_resource(id, :releasing)
        |> record({:release_started, id, state.release.deadline})

      [] ->
        outcome =
          case {scope.primary, scope.errors} do
            {{:trap, _} = trap, _} -> trap
            {primary, []} -> primary
            {_, [first | _]} -> {:trap, {:mandatory_release_failed, first}}
          end

        completion = %{
          scope: scope.id,
          primary: scope.primary,
          outcome: outcome,
          secondary: scope.errors
        }

        state = %{state | stack: tl(state.stack), completed: state.completed ++ [completion]}
        state |> put_scope(%{scope | phase: :closed}) |> record({:completed, completion})
    end
  end

  defp outcome?({kind, _}) when kind in [:ok, :failure, :abort, :trap, :cancelled, :exit],
    do: true

  defp outcome?(_), do: false
  defp active?(%{stack: []}), do: true
  defp active?(state), do: match?({:ok, %{pending: nil}}, top_open(state))

  defp top_open(%{stack: [id | _]} = state) do
    case state.scopes[id] do
      %{phase: :open} = scope -> {:ok, scope}
      _ -> {:error, :scope_closing}
    end
  end

  defp top_open(_), do: {:error, :no_scope}
  defp put_scope(state, scope), do: %{state | scopes: Map.put(state.scopes, scope.id, scope)}

  defp put_resource(state, id, status),
    do: %{state | resources: Map.update!(state.resources, id, &%{&1 | status: status})}

  defp record(state, event), do: %{state | trace: state.trace ++ [event]}
end
