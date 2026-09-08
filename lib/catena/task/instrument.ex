defmodule Catena.Task.Instrument do
  @moduledoc "Verified function-entry cancellation and managed receive instrumentation for task targets."
  alias Catena.Kernel.{Backend, Verifier}

  def lower(core) do
    with :ok <- Verifier.verify(core) do
      {:ok, Enum.map(Backend.lower(core), &walk/1)}
    end
  end

  defp walk({:function, ann, name, arity, clauses}),
    do: {:function, ann, name, arity, Enum.map(clauses, &function_clause/1)}

  defp walk({:fun, ann, {:clauses, clauses}}),
    do: {:fun, ann, {:clauses, Enum.map(clauses, &function_clause/1)}}

  defp walk({:named_fun, ann, name, clauses}),
    do: {:named_fun, ann, name, Enum.map(clauses, &function_clause/1)}

  defp walk({:receive, ann, clauses}), do: receive_form(ann, clauses, nil)

  defp walk({:receive, ann, clauses, timeout, fallback}),
    do: receive_form(ann, clauses, {walk(timeout), walk(fallback)})

  defp walk(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.map(&walk/1) |> List.to_tuple()

  defp walk(value) when is_list(value), do: Enum.map(value, &walk/1)
  defp walk(value), do: value

  defp receive_form(ann, clauses, timing) do
    receive_ast = fn clauses ->
      case timing do
        nil -> {:receive, ann, clauses}
        {timeout, fallback} -> {:receive, ann, clauses, timeout, fallback}
      end
    end

    clauses = Enum.map(clauses, &walk/1)
    token = {:var, ann, :__Catena_ManagedReceiveToken}
    reason = {:var, ann, :__Catena_ManagedReceiveReason}

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
         {:clause, ann, [{:atom, ann, nil}], [], [receive_ast.(clauses)]},
         {:clause, ann, [{:tuple, ann, [{:var, ann, :_}, token]}], [],
          [receive_ast.([stop | wrapped])]}
       ]}

    {:call, ann, {:fun, ann, {:clauses, [{:clause, ann, [], [], [select]}]}}, []}
  end

  defp function_clause({:clause, ann, parameters, guards, body}) do
    checkpoint =
      {:call, ann, {:remote, ann, {:atom, ann, Catena.Task.Runtime}, {:atom, ann, :checkpoint}},
       []}

    {:clause, ann, parameters, guards, [checkpoint | Enum.map(body, &walk/1)]}
  end
end
