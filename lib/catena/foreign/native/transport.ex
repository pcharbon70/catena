defmodule Catena.Foreign.Native.Transport do
  @moduledoc false
  alias Catena.Foreign.Native.Package
  @module :catena_native_service

  def open(ready) do
    directory =
      Path.join(
        System.tmp_dir!(),
        "catena-native-" <> Base.url_encode64(:crypto.strong_rand_bytes(18), padding: false)
      )

    try do
      :ok = File.mkdir(directory)
      :ok = File.chmod(directory, 0o700)

      Enum.each(ready.package.payloads, fn {name, bytes} ->
        File.write!(Path.join(directory, name), bytes)
      end)

      description = ready.package.description

      state = %{
        directory: directory,
        timeout: description["timeout_ms"],
        kind: description["kind"],
        handle: nil,
        ended: false,
        blocked: false
      }

      result =
        case state.kind do
          "port" -> open_port(state)
          "nif" -> open_nif(state, ready)
        end

      case result do
        {:ok, _} ->
          result

        error ->
          File.rm_rf(directory)
          error
      end
    rescue
      _ ->
        File.rm_rf(directory)
        {:error, :native_load_failed}
    end
  end

  defp open_port(state) do
    python = System.find_executable("python3")
    if is_nil(python), do: raise("Python 3 guardian unavailable")
    service = Path.join(state.directory, "service")
    guardian = Path.join(state.directory, "guardian.py")
    File.chmod!(service, 0o700)
    File.write!(guardian, Package.guardian_source())

    port =
      Port.open({:spawn_executable, python}, [
        :binary,
        :exit_status,
        {:packet, 2},
        {:args, [guardian, service, Integer.to_string(state.timeout)]}
      ])

    receive do
      {^port, {:data, "R"}} -> {:ok, %{state | handle: port}}
      {^port, {:exit_status, _}} -> {:error, :native_start_failed}
    after
      2000 ->
        safe_port_close(port)
        {:error, :native_start_timeout}
    end
  end

  defp open_nif(state, ready) do
    identity = ready.package.description["files"]

    result =
      :global.trans({__MODULE__, self()}, fn ->
        case :persistent_term.get({__MODULE__, :loaded}, nil) do
          ^identity ->
            :ok

          nil ->
            beam = ready.package.payloads["catena_native_service.beam"]

            with {:ok, {@module, _}} <- :beam_lib.chunks(beam, [:exports]),
                 {:module, @module} <- :code.load_binary(@module, ~c"signed-native-package", beam),
                 true <-
                   Enum.all?([init_library: 1, open: 0, call: 2, close: 1], fn {f, a} ->
                     function_exported?(@module, f, a)
                   end),
                 :ok <-
                   apply(@module, :init_library, [
                     String.to_charlist(Path.join(state.directory, "catena_native_service"))
                   ]) do
              :persistent_term.put({__MODULE__, :loaded}, identity)
              :ok
            else
              _ -> {:error, :native_nif_load_failed}
            end

          _ ->
            {:error, :native_nif_replacement_denied}
        end
      end)

    with :ok <- result,
         {:ok, handle} <- bounded(fn -> apply(@module, :open, []) end, state.timeout),
         do: {:ok, %{state | handle: handle}}
  end

  def call(state, value) do
    {:ok, codec} = Catena.Foreign.Codec.new({:data, :float})
    limits = %{nodes: 1, bytes: 8, depth: 0}

    case Catena.Foreign.Codec.to_native(codec, value, limits) do
      {:ok, value} ->
        {result, state} = native_call(state, value)

        case result do
          {:ok, result} ->
            case Catena.Foreign.Codec.from_native(codec, result, limits) do
              {:ok, result} -> {{:ok, result}, state}
              _ -> {{:error, :invalid_native_float}, state}
            end

          _ ->
            {result, state}
        end

      _ ->
        {{:error, :invalid_native_float}, state}
    end
  end

  defp native_call(%{ended: true} = state, _), do: {{:error, :native_service_ended}, state}

  defp native_call(%{blocked: true} = state, _), do: {{:error, :native_scope_poisoned}, state}

  defp native_call(state, value) when is_float(value) do
    case state.kind do
      "port" ->
        port = state.handle

        try do
          true = Port.command(port, <<value::float-big-64>>)

          receive do
            {^port, {:data, <<result::float-big-64>>}} ->
              {{:ok, result}, state}

            {^port, {:data, _}} ->
              {{:error, :invalid_native_float}, state}

            {^port, {:exit_status, status}} ->
              {{:error, {:native_service_exit, status}}, %{state | ended: true}}
          after
            state.timeout + 1000 -> {{:error, :native_transport_timeout}, state}
          end
        rescue
          _ -> {{:error, :native_service_ended}, state}
        end

      "nif" ->
        result =
          case bounded(fn -> apply(@module, :call, [state.handle, value]) end, state.timeout) do
            {:ok, {:dirty_cpu, result}} when is_float(result) -> {:ok, result}
            {:ok, _} -> {:error, :invalid_native_result_or_scheduler}
            error -> error
          end

        blocked = match?({:error, {:native_timeout, _}}, result)
        {result, %{state | blocked: blocked}}
    end
  end

  defp native_call(state, _), do: {{:error, :invalid_native_float}, state}

  def close(%{ended: true} = state), do: {:ok, state}

  def close(%{kind: "nif"} = state) do
    case bounded(fn -> apply(@module, :close, [state.handle]) end, state.timeout) do
      {:ok, :ok} -> {:ok, %{state | ended: true}}
      error -> {error, state}
    end
  end

  def close(state) do
    port = state.handle

    result =
      try do
        true = Port.command(port, <<>>)
        await_close(port, false, System.monotonic_time(:millisecond) + 1000)
      rescue
        _ -> {:error, :native_close_unconfirmed}
      end

    {result, %{state | ended: result == :ok}}
  end

  defp await_close(port, acknowledged, deadline) do
    receive do
      {^port, {:data, "C"}} ->
        await_close(port, true, deadline)

      {^port, {:data, _}} ->
        await_close(port, acknowledged, deadline)

      {^port, {:exit_status, status}} ->
        if acknowledged and status == 0, do: :ok, else: {:error, {:native_close_exit, status}}
    after
      max(0, deadline - System.monotonic_time(:millisecond)) -> {:error, :native_close_timeout}
    end
  end

  def dispose(state) do
    unless state.ended do
      close(state)
      if state.kind == "port", do: safe_port_close(state.handle)
    end

    File.rm_rf(state.directory)
    :ok
  end

  defp safe_port_close(port) do
    Port.close(port)
  rescue
    _ -> :ok
  end

  defp bounded(fun, timeout) do
    tag = make_ref()
    owner = self()

    {worker, monitor} =
      :erlang.spawn_opt(
        fn ->
          result =
            try do
              {:ok, fun.()}
            catch
              _, _ -> {:error, :native_call_failed}
            end

          send(owner, {tag, result})
        end,
        [:link, :monitor]
      )

    receive do
      {^tag, result} ->
        Process.demonitor(monitor, [:flush])
        result

      {:DOWN, ^monitor, :process, ^worker, _} ->
        {:error, :native_worker_exit}
    after
      timeout ->
        Process.exit(worker, :kill)
        Process.demonitor(monitor, [:flush])
        {:error, {:native_timeout, :native_work_may_continue}}
    end
  end
end
