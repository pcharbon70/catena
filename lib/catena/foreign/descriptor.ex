defmodule Catena.Foreign.Descriptor do
  @moduledoc "Explicit trusted host declarations; payloads never supply their own authority."
  alias Catena.Foreign.Codec

  def new(host, arguments, result, effect, options \\ []) do
    with {:ok, selection} <- selection(options),
         true <- is_list(arguments) and length(arguments) <= 254,
         true <- is_binary(effect) and effect != "" and String.valid?(effect),
         true <- Keyword.get(options, :trust) == :trusted_beam,
         true <- Keyword.get(options, :scheduler) == :owned_process,
         true <- Keyword.get(options, :cancellation) == :cooperative,
         :ok <- each(arguments, &argument(&1, selection.language_revision)),
         :ok <- Codec.verify(result),
         {:ok, identity} <- identity(host, length(arguments) + 1) do
      {:ok,
       %{
         format: :foreign_declaration,
         version: selection.language_revision,
         selection: selection,
         host: host,
         identity: identity,
         arguments: arguments,
         result: result,
         effect: effect,
         trust: :trusted_beam,
         scheduler: :owned_process,
         cancellation: :cooperative
       }}
    else
      _ -> {:error, :invalid_foreign_declaration}
    end
  rescue
    _ -> {:error, :invalid_foreign_declaration}
  end

  def verify(%{host: host, arguments: arguments, result: result, effect: effect} = descriptor) do
    case new(host, arguments, result, effect,
           language_selection: descriptor.selection,
           trust: descriptor.trust,
           scheduler: descriptor.scheduler,
           cancellation: descriptor.cancellation
         ) do
      {:ok, ^descriptor} -> :ok
      _ -> {:error, :foreign_declaration_changed}
    end
  rescue
    _ -> {:error, :invalid_foreign_declaration}
  end

  def verify(_), do: {:error, :invalid_foreign_declaration}

  def selection(options) do
    requested =
      Keyword.get(options, :language_selection, Catena.LanguageVersion.legacy_selection("0.1.61"))

    case Catena.LanguageVersion.resolve_selection(requested) do
      {:ok, %{language_revision: version, previews: []} = selection}
      when version in ["0.1.61", "0.1.62"] ->
        {:ok, selection}

      _ ->
        {:error, :invalid_foreign_selection}
    end
  end

  def id(descriptor), do: Catena.Calling.Descriptor.digest(descriptor)

  defp argument({:callback, input, output}, _) do
    with :ok <- Codec.verify(input), do: Codec.verify(output)
  end

  defp argument({:native, role}, "0.1.62"), do: Catena.Foreign.NativeValue.verify_role(role)
  defp argument(codec, _), do: Codec.verify(codec)

  defp identity({module, function}, arity) when is_atom(module) and is_atom(function) do
    with {:module, ^module} <- Code.ensure_loaded(module),
         true <- function_exported?(module, function, arity) do
      {:ok, %{module: module, function: function, arity: arity, md5: module.module_info(:md5)}}
    else
      _ -> {:error, :missing_host_export}
    end
  end

  defp identity(_, _), do: {:error, :invalid_host_identity}

  defp each(values, fun),
    do:
      Enum.reduce_while(values, :ok, fn value, :ok ->
        case fun.(value) do
          :ok -> {:cont, :ok}
          error -> {:halt, error}
        end
      end)
end
