defmodule Catena.Runtime.Environment.Schema do
  @moduledoc "Closed service roles, arguments and typed answers for the explicit environment entry."
  alias Catena.Foreign.Codec

  @errors ~w(denied expired revoked cancelled timeout limit unavailable invalid_request)
  @operations %{
    io: %{read: {:integer, :bytes}, write: {:bytes, :unit}},
    filesystem: %{
      read: {{:tuple, [:text, :integer]}, :bytes},
      write: {{:tuple, [:text, :bytes]}, :unit}
    },
    network: %{exchange: {{:tuple, [:text, :bytes, :integer]}, :bytes}},
    time: %{monotonic: {:unit, :integer}, wall: {:unit, :integer}, sleep: {:integer, :unit}},
    random: %{bytes: {:integer, :bytes}},
    environment: %{get: {:text, {:variant, %{"absent" => :unit, "present" => :bytes}}}},
    logging: %{emit: {{:tuple, [:text, :text]}, :unit}},
    process: %{run: {{:tuple, [:text, :bytes]}, {:tuple, [:integer, :bytes]}}}
  }
  def services, do: Map.keys(@operations) |> Enum.sort()
  def operations(service), do: Map.get(@operations, service, %{})
  def family(service), do: "catena://environment/0.1.68/" <> Atom.to_string(service)

  def error_schema(service) do
    specific =
      case service do
        :io -> ~w(io_failure)
        :filesystem -> ~w(not_found io_failure)
        :network -> ~w(network_failure)
        :logging -> ~w(io_failure)
        :process -> ~w(not_found process_failure)
        _ -> []
      end

    {:variant, Map.new(@errors ++ specific, &{&1, :unit})}
  end

  def result_schema(payload, service),
    do: {:variant, %{"ok" => payload, "error" => error_schema(service)}}

  def success(value), do: {:catena_variant, :ok, value}

  def failure(reason)
      when reason in [
             :denied,
             :expired,
             :revoked,
             :cancelled,
             :timeout,
             :limit,
             :unavailable,
             :not_found,
             :io_failure,
             :network_failure,
             :process_failure,
             :invalid_request
           ],
      do: {:catena_variant, :error, {:catena_variant, reason, :unit}}

  def operation(service, name) do
    case get_in(@operations, [service, name]) do
      {argument, payload} ->
        with {:ok, input} <- Codec.new({:data, argument}),
             {:ok, result} <- Codec.new({:data, result_schema(payload, service)}) do
          {:ok,
           %{
             version: "0.1.68",
             service: service,
             operation: name,
             family: family(service),
             input: input,
             result: result
           }}
        end

      _ ->
        {:error, :unknown_environment_operation}
    end
  end

  def verify(description) do
    case operation(description.service, description.operation) do
      {:ok, ^description} -> :ok
      _ -> {:error, :invalid_environment_operation}
    end
  rescue
    _ -> {:error, :invalid_environment_operation}
  end

  def core_type({tag, fields}) when tag in [:record, :variant],
    do: {tag, %{fields: Map.new(fields, fn {k, v} -> {k, core_type(v)} end), tail: nil}}

  def core_type({:tuple, fields}), do: {:tuple, Enum.map(fields, &core_type/1)}
  def core_type(type), do: type
end
