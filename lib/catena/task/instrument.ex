defmodule Catena.Task.Instrument do
  @moduledoc "Verified function-entry cancellation and managed receive instrumentation for task targets."
  alias Catena.Kernel.{Backend, Verifier}

  def lower(core) do
    with :ok <- Verifier.verify(core) do
      {forms, _} = Enum.map_reduce(Backend.lower(core), 0, &walk/2)
      {:ok, forms}
    end
  end

  defp walk({:function, ann, name, arity, clauses}, counter) do
    {clauses, counter} = Enum.map_reduce(clauses, counter, &function_clause/2)
    {{:function, ann, name, arity, clauses}, counter}
  end

  defp walk({:fun, ann, {:clauses, clauses}}, counter) do
    {clauses, counter} = Enum.map_reduce(clauses, counter, &function_clause/2)
    {{:fun, ann, {:clauses, clauses}}, counter}
  end

  defp walk({:named_fun, ann, name, clauses}, counter) do
    {clauses, counter} = Enum.map_reduce(clauses, counter, &function_clause/2)
    {{:named_fun, ann, name, clauses}, counter}
  end

  defp walk({:receive, ann, clauses}, counter) do
    {clauses, next} = walk(clauses, counter + 1)
    {receive_form(ann, clauses, nil, counter), next}
  end

  defp walk({:receive, ann, clauses, timeout, fallback}, counter) do
    {clauses, next} = walk(clauses, counter + 1)
    {timeout, next} = walk(timeout, next)
    {fallback, next} = walk(fallback, next)
    {receive_form(ann, clauses, {timeout, fallback}, counter), next}
  end

  defp walk(value, counter) when is_tuple(value) do
    {elements, counter} = value |> Tuple.to_list() |> Enum.map_reduce(counter, &walk/2)
    {List.to_tuple(elements), counter}
  end

  defp walk(value, counter) when is_list(value), do: Enum.map_reduce(value, counter, &walk/2)
  defp walk(value, counter), do: {value, counter}

  defp receive_form(ann, clauses, timing, counter) do
    receive_ast = fn clauses ->
      case timing do
        nil -> {:receive, ann, clauses}
        {timeout, fallback} -> {:receive, ann, clauses, timeout, fallback}
      end
    end

    token = {:var, ann, String.to_atom("__Catena_ManagedReceiveToken_#{counter}")}
    reason = {:var, ann, String.to_atom("__Catena_ManagedReceiveReason_#{counter}")}

    worker = {:var, ann, String.to_atom("__Catena_ReceiveWorker_#{counter}")}
    scopes = {:var, ann, String.to_atom("__Catena_ReceiveScopes_#{counter}")}
    scope = {:var, ann, String.to_atom("__Catena_ReceiveScope_#{counter}")}
    runtime = {:atom, ann, Catena.Task.Runtime}

    setup =
      {:match, ann, {:tuple, ann, [worker, scopes]},
       {:call, ann, {:remote, ann, runtime, {:atom, ann, :receive_context}}, []}}

    worker_cancel =
      {:clause, ann, [{:tuple, ann, [worker, {:atom, ann, :cancel}, reason]}],
       [[{:call, ann, {:atom, ann, :is_reference}, [worker]}]],
       [
         {:call, ann, {:remote, ann, {:atom, ann, :erlang}, {:atom, ann, :throw}},
          [{:tuple, ann, [{:atom, ann, :catena_resource_cancelled}, reason]}]}
       ]}

    child_failure =
      {:clause, ann, [{:tuple, ann, [{:atom, ann, :task_failed}, scope, reason]}],
       [[{:call, ann, {:atom, ann, :is_map_key}, [scope, scopes]}]],
       [
         {:call, ann, {:remote, ann, {:atom, ann, :erlang}, {:atom, ann, :exit}},
          [{:tuple, ann, [{:atom, ann, :catena_resource_exit}, reason]}]}
       ]}

    task_controls = [worker_cancel, child_failure]

    wrapped =
      Enum.map(clauses, fn {:clause, a, [pattern], guards, body} ->
        {:clause, a, [{:tuple, a, [{:atom, a, :managed_message}, token, pattern]}], guards, body}
      end)

    stop =
      {:clause, ann,
       [{:tuple, ann, [{:atom, ann, :managed_control}, token, {:atom, ann, :stop}, reason]}], [],
       [
         {:call, ann, {:remote, ann, {:atom, ann, :erlang}, {:atom, ann, :exit}},
          [{:tuple, ann, [{:atom, ann, :catena_resource_exit}, reason]}]}
       ]}

    context =
      {:call, ann, {:remote, ann, {:atom, ann, Catena.Task.Managed}, {:atom, ann, :context}}, []}

    select =
      {:case, ann, context,
       [
         {:clause, ann, [{:atom, ann, nil}], [], [receive_ast.(task_controls ++ clauses)]},
         {:clause, ann, [{:tuple, ann, [{:var, ann, :_}, token]}], [],
          [receive_ast.(task_controls ++ [stop | wrapped])]}
       ]}

    {:call, ann, {:fun, ann, {:clauses, [{:clause, ann, [], [], [setup, select]}]}}, []}
  end

  defp function_clause({:clause, ann, parameters, guards, body}, counter) do
    checkpoint =
      {:call, ann, {:remote, ann, {:atom, ann, Catena.Task.Runtime}, {:atom, ann, :checkpoint}},
       []}

    {body, counter} = walk(body, counter)
    {{:clause, ann, parameters, guards, [checkpoint | body]}, counter}
  end
end
