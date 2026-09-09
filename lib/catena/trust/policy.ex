defmodule Catena.Trust.Policy do
  @moduledoc "Owner-bound admission of exact transitive trusted boundaries, with monotone revocation."
  alias Catena.Trust.{Obligations, Policy.Session}

  def run(graph, grants, body) when is_function(body, 1) do
    with :ok <- Obligations.verify(graph),
         :ok <- valid_grants(grants),
         {:ok, pid} <- Session.start(self(), graph, grants) do
      scope = {__MODULE__, self(), pid, make_ref()}
      :ok = GenServer.call(pid, {:bind, scope})

      try do
        body.(scope)
      after
        if Process.alive?(pid), do: GenServer.stop(pid)
      end
    end
  end

  def valid_grants(grants) when is_map(grants) and map_size(grants) <= 256 do
    if Enum.all?(grants, fn {id, acknowledgements} ->
         is_binary(id) and Regex.match?(~r/^[0-9a-f]{64}$/, id) and is_list(acknowledgements) and
           length(acknowledgements) <= 32 and
           acknowledgements == Enum.sort(Enum.uniq(acknowledgements)) and
           Enum.all?(
             acknowledgements,
             &(is_binary(&1) and byte_size(&1) <= 128 and String.valid?(&1))
           )
       end), do: :ok, else: {:error, :invalid_trusted_grants}
  rescue
    _ -> {:error, :invalid_trusted_grants}
  end

  def valid_grants(_), do: {:error, :invalid_trusted_grants}

  def attenuate(scope, grants), do: request(scope, {:attenuate, grants})
  def revoke(scope, boundary), do: request(scope, {:revoke, boundary})
  def admit(scope, name), do: request(scope, {:admit, name})

  def invoke(scope, name, entry, arguments, limits) do
    with {:ok, implementation} <- admit(scope, name) do
      case implementation do
        %{kind: :pure, artifact: artifact, core: core} ->
          Catena.Calling.Adapter.invoke(artifact, core, entry, arguments, limits)

        %{kind: :foreign, program: program} ->
          if entry == program.description.name and arguments == [] do
            declarations =
              program.description.bindings |> Map.values() |> Enum.flat_map(&Map.values/1)

            Catena.Foreign.Adapter.run(declarations, limits, fn adapter ->
              Catena.Foreign.Program.invoke(program, adapter, limits, 5000)
            end)
          else
            {:error, :invalid_trusted_entry}
          end

        %{kind: :native, ready: ready} ->
          case {entry, arguments} do
            {"call", [value]} ->
              Catena.Foreign.Native.run(ready.package, ready.policy, fn native ->
                Catena.Foreign.Native.call(native, value)
              end)

            _ ->
              {:error, :invalid_trusted_entry}
          end
      end
    end
  end

  defp request({__MODULE__, owner, pid, _} = scope, operation) when owner == self() do
    GenServer.call(pid, {scope, operation}, :infinity)
  catch
    :exit, _ -> {:error, :expired_trusted_scope}
  end

  defp request(_, _), do: {:error, :invalid_trusted_scope_owner}
end

defmodule Catena.Trust.Policy.Session do
  @moduledoc false
  use GenServer
  alias Catena.Trust.{Obligations, Policy}
  def start(owner, graph, grants), do: GenServer.start(__MODULE__, {owner, graph, grants})

  def init({owner, graph, grants}) do
    with :ok <- Obligations.verify(graph), :ok <- Policy.valid_grants(grants) do
      {:ok,
       %{owner: owner, monitor: Process.monitor(owner), graph: graph, grants: grants, scopes: %{}}}
    else
      error -> {:stop, error}
    end
  end

  def handle_call({:bind, scope}, {owner, _}, %{owner: owner, scopes: scopes} = s)
      when map_size(scopes) == 0 do
    {:reply, :ok, %{s | scopes: %{scope => %{grants: s.grants, parents: [], revoked: []}}}}
  end

  def handle_call({scope, operation}, {owner, _}, %{owner: owner} = state) do
    case state.scopes[scope] do
      nil -> {:reply, {:error, :invalid_trusted_scope}, state}
      entry -> operation(operation, scope, entry, state)
    end
  end

  def handle_call(_, _, s), do: {:reply, {:error, :invalid_trusted_scope}, s}

  def handle_info({:DOWN, monitor, :process, _, _}, %{monitor: monitor} = s),
    do: {:stop, :normal, s}

  def handle_info(_, s), do: {:noreply, s}

  defp operation({:attenuate, grants}, scope, entry, s) do
    if Policy.valid_grants(grants) == :ok and map_size(s.scopes) < 64 and
         Enum.all?(grants, fn {id, acks} ->
           Map.has_key?(entry.grants, id) and acks -- entry.grants[id] == []
         end) do
      child = {Policy, s.owner, self(), make_ref()}
      value = %{grants: grants, parents: [scope | entry.parents], revoked: []}
      {:reply, {:ok, child}, put_in(s.scopes[child], value)}
    else
      {:reply, {:error, :trusted_attenuation_denied}, s}
    end
  end

  defp operation({:revoke, boundary}, scope, entry, s) do
    if Map.has_key?(entry.grants, boundary) do
      {:reply, :ok, put_in(s.scopes[scope].revoked, Enum.uniq([boundary | entry.revoked]))}
    else
      {:reply, {:error, :unknown_trusted_boundary}, s}
    end
  end

  defp operation({:admit, name}, _scope, entry, s) do
    revoked = entry.revoked ++ Enum.flat_map(entry.parents, &s.scopes[&1].revoked)
    records = Obligations.obligations(s.graph, name)

    valid =
      is_list(records) and
        Enum.all?(records, fn r ->
          id = r["boundary"]

          id not in revoked and Map.has_key?(entry.grants, id) and
            r["obligations"] -- entry.grants[id] == []
        end)

    if valid,
      do: {:reply, {:ok, s.graph.inputs[name].implementation}, s},
      else: {:reply, {:error, :trusted_admission_denied}, s}
  end

  defp operation(_, _, _, s), do: {:reply, {:error, :invalid_trusted_request}, s}
end
