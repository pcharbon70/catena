defmodule Catena.Calling.Scope do
  @moduledoc "Exact 0.1.59 owner-local checked closure lifetime for a verified call artifact."
  alias Catena.Calling.{Adapter, Artifact}
  alias Catena.ValueBoundary.{Budget, Data}

  def run(artifact, core, limits, body, options \\ []) when is_function(body, 1) do
    maximum = Keyword.get(options, :max_handles, 1024)

    with true <- Budget.valid?(limits) and is_integer(maximum) and maximum > 0,
         :ok <- Artifact.verify(artifact, core, options),
         {:module, module} <-
           load_artifact(artifact, options),
         {:ok, {^module, digest}} <- :beam_lib.md5(artifact.binary) do
      token = make_ref()
      key = {__MODULE__, token}
      scope = {__MODULE__, self(), token}

      Process.put(key, %{
        artifact: artifact,
        core: core,
        limits: limits,
        maximum: maximum,
        handles: %{},
        allow_callbacks: Keyword.get(options, :allow_callbacks, false) == true,
        module_digest: digest
      })

      try do
        body.(scope)
      after
        Process.delete(key)
      end
    else
      false -> {:error, :invalid_scope_limits}
      error -> error
    end
  end

  defp load_artifact(artifact, options) do
    {:ok, {module, digest}} = :beam_lib.md5(artifact.binary)

    if Keyword.get(options, :reuse_loaded, false) and :code.is_loaded(module) != false and
         module.module_info(:md5) == digest do
      {:module, module}
    else
      Catena.OTP.Compiler.load(module, ~c"calling-scope.beam", artifact.binary)
    end
  end

  def entry(scope, name) do
    with {:ok, state} <- state(scope),
         %{kind: :value} = entry <-
           Enum.find(state.artifact.descriptor.entries, &(&1.name == name)),
         definition <- Enum.find(state.core.definitions, &(&1.name == name)),
         true <- Adapter.initial_pure?(definition),
         type <- Map.get(definition, :signature) || definition.scheme.type,
         :ok <- admissible(type),
         :ok <- capacity(state, type) do
      capture(scope, state, type, fn ->
        apply(state.artifact.module, String.to_existing_atom(entry.factory), [])
      end)
    else
      false -> {:error, :unadmitted_initial_effect}
      nil -> {:error, :unknown_call_entry}
      {:error, _} = error -> error
      _ -> {:error, :unsupported_call_entry}
    end
  end

  def call(scope, handle, argument) do
    with {:ok, state} <- state(scope),
         {:ok, {function, type}} <- handle(state, scope, handle),
         {:ok, parameter, result} <- stage(type),
         {:ok, argument} <- argument(state, scope, parameter, argument),
         :ok <- capacity(state, result) do
      capture(scope, state, result, fn -> function.(argument) end)
    end
  end

  @doc "Create a checked synchronous unary host callback under explicit harness authority."
  def callback(scope, callback_handle) do
    with {:ok, state} <- state(scope),
         true <- state.allow_callbacks,
         {:ok, {_function, type}} <- handle(state, scope, callback_handle),
         {:ok, parameter, result} <- stage(type),
         {:ok, _} <- Adapter.schema(parameter),
         {:ok, result_schema} <- Adapter.schema(result) do
      Catena.Effect.Runtime.trace({:foreign_callback, :created})

      {:ok,
       fn argument ->
         Catena.Effect.Runtime.trace({:foreign_callback, :entered})

         case call(scope, callback_handle, argument) do
           {:ok, semantic} ->
             case Data.encode(result_schema, semantic, state.limits) do
               {:ok, value} -> value
               {:error, reason} -> :erlang.error({:catena_trap, {:callback_boundary, reason}})
             end

           {:error, {:execution_failure, :error, {:catena_trap, reason}}} ->
             :erlang.error({:catena_trap, reason})

           {:error, reason} ->
             :erlang.error({:catena_trap, {:callback_boundary, reason}})
         end
       end}
    else
      false -> {:error, :missing_callback_authority}
      {:error, _} = error -> error
      _ -> {:error, :unsupported_callback}
    end
  end

  defp argument(state, scope, type, value) do
    if function?(type) do
      case handle(state, scope, value) do
        {:ok, {function, ^type}} -> {:ok, function}
        _ -> {:error, :callback_type_mismatch}
      end
    else
      with {:ok, schema} <- Adapter.schema(type),
           {:ok, _} <- Data.decode(schema, value, state.limits),
           do: {:ok, value}
    end
  end

  def revoke(scope, handle) do
    with {:ok, state} <- state(scope),
         {:ok, _} <- handle(state, scope, handle) do
      {_, _, token} = handle
      put_state(scope, %{state | handles: Map.delete(state.handles, token)})
      :ok
    end
  end

  defp state({__MODULE__, owner, token}) when owner == self() do
    case Process.get({__MODULE__, token}) do
      nil ->
        {:error, :expired_call_scope}

      state ->
        # Replacing loaded code invalidates the scope before another call.
        if :code.is_loaded(state.artifact.module) != false and
             state.artifact.module.module_info(:md5) == state.module_digest,
           do: {:ok, state},
           else: {:error, :call_artifact_replaced}
    end
  rescue
    _ -> {:error, :call_artifact_replaced}
  end

  defp state(_), do: {:error, :invalid_call_scope_owner}

  defp handle(state, {__MODULE__, _, scope_token}, {__MODULE__, scope_token, token}) do
    case Map.fetch(state.handles, token) do
      {:ok, value} -> {:ok, value}
      :error -> {:error, :invalid_call_handle}
    end
  end

  defp handle(_, _, _), do: {:error, :invalid_call_handle}

  defp put_state({__MODULE__, _, token}, state), do: Process.put({__MODULE__, token}, state)

  defp capture(scope, state, type, body) do
    value = body.()

    case type do
      {:function, _, _, _} ->
        store(scope, state, type, value)

      {:function, _, _} ->
        store(scope, state, type, value)

      _ ->
        with {:ok, schema} <- Adapter.schema(type),
             {:ok, value} <- Data.decode(schema, value, state.limits),
             do: {:ok, value}
    end
  catch
    kind, reason -> {:error, {:execution_failure, kind, reason}}
  end

  defp store({__MODULE__, _, scope_token} = scope, state, type, value)
       when is_function(value, 1) do
    token = make_ref()
    put_state(scope, %{state | handles: Map.put(state.handles, token, {value, type})})
    {:ok, {__MODULE__, scope_token, token}}
  end

  defp store(_, _, _, _), do: {:error, :invalid_compiled_closure}

  defp capacity(state, type) do
    if function?(type) and map_size(state.handles) >= state.maximum,
      do: {:error, :call_handle_limit},
      else: :ok
  end

  defp function?({:function, _, _, _}), do: true
  defp function?({:function, _, _}), do: true
  defp function?(_), do: false

  defp stage({:function, parameter, [], result}), do: {:ok, parameter, result}
  defp stage({:function, parameter, result}), do: {:ok, parameter, result}
  defp stage(_), do: {:error, :unadmitted_application_effect}

  # This first scoped adapter admits data inputs and pure curried data results.
  # Effectful/native callback admission belongs to the subsequent explicit authority layer.
  defp admissible({:function, parameter, [], result}),
    do: admissible({:function, parameter, result})

  defp admissible({:function, parameter, result}) do
    with :ok <- admissible(parameter), do: admissible(result)
  end

  defp admissible(type) do
    case Adapter.schema(type) do
      {:ok, _} -> :ok
      error -> error
    end
  end
end
