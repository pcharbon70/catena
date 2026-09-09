defmodule Catena.Foreign.NativeValue do
  @moduledoc "Opaque registered native roles; no payload-selected authority or generic host term type."
  @enforce_keys [:scope, :token, :role]
  defstruct [:scope, :token, :role]
  alias Catena.Foreign.{Adapter, Codec, Descriptor}

  def role({:local_process, mailbox}) do
    with :ok <- Codec.verify(mailbox) do
      {:ok,
       %{
         version: "0.1.62",
         kind: :local_process,
         mailbox: mailbox,
         transferable: :send_only,
         equality: false
       }}
    end
  end

  def role(:reference),
    do: {:ok, %{version: "0.1.62", kind: :reference, transferable: false, equality: false}}

  def role(:port), do: {:error, :native_port_role_not_admitted}
  def role(:function), do: {:error, :use_checked_callback_role}
  def role(_), do: {:error, :unsupported_native_role}

  def verify_role(%{kind: :local_process, mailbox: mailbox} = description) do
    case role({:local_process, mailbox}) do
      {:ok, ^description} -> :ok
      _ -> {:error, :invalid_native_role}
    end
  end

  def verify_role(%{kind: :reference} = description) do
    case role(:reference) do
      {:ok, ^description} -> :ok
      _ -> {:error, :invalid_native_role}
    end
  end

  def verify_role(_), do: {:error, :invalid_native_role}

  def run(declarations, grants, limits, body, options \\ []) do
    options =
      options
      |> Keyword.put_new(:language_selection, Catena.LanguageVersion.legacy_selection("0.1.62"))
      |> Keyword.put(:native_grants, grants)

    with {:ok, %{language_revision: "0.1.62"}} <- Descriptor.selection(options) do
      Adapter.run(declarations, limits, body, options)
    else
      _ -> {:error, :invalid_native_selection}
    end
  end

  def declaration(host, arguments, result, effect, options) do
    options =
      Keyword.put_new(
        options,
        :language_selection,
        Catena.LanguageVersion.legacy_selection("0.1.62")
      )

    with {:ok, %{language_revision: "0.1.62"}} <- Descriptor.selection(options) do
      Descriptor.new(host, arguments, result, effect, options)
    else
      _ -> {:error, :invalid_native_selection}
    end
  end

  def fetch(scope, index), do: Adapter.request(scope, {:native_fetch, index})

  def send_message(%__MODULE__{scope: {Adapter, _, manager, _}} = handle, message) do
    GenServer.call(manager, {:native_send, handle, message}, :infinity)
  catch
    :exit, _ -> {:error, :expired_native_value}
  end

  def send_message(_, _), do: {:error, :invalid_native_handle}

  def check_operation(role, operation) do
    with :ok <- verify_role(role) do
      case {role.kind, operation} do
        {:local_process, :send} -> {:ok, :unit}
        {:local_process, :transfer_send_authority} -> {:ok, role}
        {_, :foreign_argument} -> {:ok, role}
        {_, :equality} -> {:error, :native_handle_equality_forbidden}
        {_, :reflection} -> {:error, :native_handle_reflection_forbidden}
        _ -> {:error, :native_role_operation_forbidden}
      end
    end
  end

  # Called only by trusted scope setup. The payload cannot construct this grant.
  def prepare_grants(grants, maximum) when is_list(grants) and length(grants) <= maximum do
    grants
    |> Enum.reduce_while({:ok, []}, fn grant, {:ok, acc} ->
      case prepare(grant) do
        {:ok, value} -> {:cont, {:ok, [value | acc]}}
        error -> {:halt, error}
      end
    end)
    |> case do
      {:ok, values} -> {:ok, Enum.reverse(values)}
      error -> error
    end
  end

  def prepare_grants(_, _), do: {:error, :invalid_native_grants_or_capacity}

  defp prepare({:local_process, pid, mailbox}) when is_pid(pid) and node(pid) == node() do
    with {:ok, role} <- role({:local_process, mailbox}), do: {:ok, %{role: role, value: pid}}
  end

  defp prepare(:fresh_reference) do
    {:ok, role} = role(:reference)
    {:ok, %{role: role, value: make_ref()}}
  end

  defp prepare(_), do: {:error, :unsupported_native_grant}
end
