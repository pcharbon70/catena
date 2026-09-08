defmodule Catena.Protocol.Program do
  @moduledoc "Checked local protocol library-application builder; no public source vocabulary."
  alias Catena.Kernel.{Backend, Verifier}
  alias Catena.Protocol.Contract
  alias Catena.{Diagnostic, ImplementationLimits, LanguageVersion}
  @entry :__catena_protocol_run
  @errors ~w(overloaded cancelled timed_out peer_lost invalid_response session_lost invalid_request_handle already_terminal invalid_payload invalid_duration)

  def outcome_type(response) do
    {:variant, %{tail: nil, fields: Map.new(@errors, &{&1, :unit}) |> Map.put("reply", response)}}
  end

  def check(core, contract, operations, options \\ []) do
    selection = Keyword.get(options, :selection, LanguageVersion.legacy_selection("0.1.55"))
    capacity = Keyword.get(options, :capacity, 1)
    handshake = Keyword.get(options, :handshake_ns, 1_000_000_000)
    grace = Keyword.get(options, :shutdown_ns, 1_000_000_000)

    with {:ok, selection} <- LanguageVersion.resolve_selection(selection),
         true <- selection.language_revision == "0.1.55" and selection.previews == [],
         :ok <- Verifier.verify(core),
         true <- core.version == "0.1.8",
         :ok <- Contract.validate(contract),
         true <- nominal_context?(core, contract),
         true <-
           is_integer(capacity) and capacity > 0 and is_integer(handshake) and handshake >= 0 and
             is_integer(grace) and grace >= 0,
         true <- is_list(operations) and operations != [],
         {:ok, output_types} <- check_operations(operations, core, contract) do
      body_type = if output_types == [], do: :unit, else: {:tuple, output_types}

      result_type =
        {:variant,
         %{
           tail: nil,
           fields: %{
             "completed" => body_type,
             "unavailable" =>
               {:variant,
                %{
                  tail: nil,
                  fields:
                    Map.new(
                      ~w(schema_mismatch negotiation_timeout peer_lost invalid_protocol_contract),
                      &{&1, :unit}
                    )
                }}
           }
         }}

      {:ok,
       %{
         profile: :local_protocol_application,
         selection: selection,
         core: core,
         contract: contract,
         operations: operations,
         capacity: capacity,
         handshake_ns: handshake,
         shutdown_ns: grace,
         result_type: result_type
       }}
    else
      _ -> error("invalid protocol application, producer signature, operation or bounds")
    end
  end

  def verify(program) do
    with %{profile: :local_protocol_application} <- program,
         {:ok, expected} <-
           check(program.core, program.contract, program.operations,
             selection: program.selection,
             capacity: program.capacity,
             handshake_ns: program.handshake_ns,
             shutdown_ns: program.shutdown_ns
           ),
         true <- expected == program do
      :ok
    else
      _ -> error("forged or inconsistent protocol application evidence")
    end
  end

  def compile(program) do
    with :ok <- verify(program),
         forms <- forms(program),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, module, binary, warnings} <-
           Catena.OTP.Compiler.compile(forms,
             source: program.core.origin,
             artifact_version: "0.1.55",
             frontend_version: "0.1.55",
             frontend: "local-protocol-application-0.1.55",
             specification: "0.1.55",
             language_selection: program.selection
           ) do
      {:ok, module, binary,
       %{
         program: program,
         entry: @entry,
         forms: forms,
         warnings: warnings,
         interface: nil,
         interface_binary: nil,
         result_type: program.result_type
       }}
    end
  end

  defp check_operations(operations, core, contract) do
    definitions = Map.new(core.definitions, &{&1.name, &1})

    Enum.reduce_while(operations, {:ok, MapSet.new(), []}, fn operation, {:ok, keys, outputs} ->
      case operation do
        {:submit, key, producer, duration}
        when is_binary(key) and byte_size(key) > 0 and is_integer(duration) and duration >= 0 ->
          case definitions[producer] do
            %{arity: 0, signature: type, uses: []} when type == contract.request ->
              if MapSet.member?(keys, key),
                do: {:halt, :error},
                else: {:cont, {:ok, MapSet.put(keys, key), outputs}}

            _ ->
              {:halt, :error}
          end

        {:await, key} ->
          if MapSet.member?(keys, key),
            do: {:cont, {:ok, keys, outputs ++ [outcome_type(contract.response)]}},
            else: {:halt, :error}

        {:cancel, key} ->
          type =
            {:variant, %{tail: nil, fields: Map.new(["cancel_selected" | @errors], &{&1, :unit})}}

          if MapSet.member?(keys, key),
            do: {:cont, {:ok, keys, outputs ++ [type]}},
            else: {:halt, :error}

        _ ->
          {:halt, :error}
      end
    end)
    |> case do
      {:ok, _, outputs} -> {:ok, outputs}
      _ -> :error
    end
  end

  defp nominal_context?(core, contract) do
    not nominal?(contract.request) or
      Catena.Kernel.Interface.build(core)["digest"] ==
        case Catena.Kernel.Interface.decode(contract.source) do
          {:ok, interface} -> interface.digest
          _ -> nil
        end
  end

  defp nominal?({:nominal, _, _}), do: true

  defp nominal?(tuple) when is_tuple(tuple),
    do: tuple |> Tuple.to_list() |> Enum.any?(&nominal?/1)

  defp nominal?(list) when is_list(list), do: Enum.any?(list, &nominal?/1)
  defp nominal?(map) when is_map(map), do: map |> Map.values() |> Enum.any?(&nominal?/1)
  defp nominal?(_), do: false

  defp forms(program) do
    base = Backend.lower(program.core)

    wire_roles =
      Map.new(program.contract.roles, fn {role, label} -> {role, String.to_atom(label)} end)

    session = {:var, 0, :ProtocolSession}

    {body, _, outputs} =
      program.operations
      |> Enum.with_index()
      |> Enum.reduce({[], %{}, []}, fn {operation, index}, {body, keys, outputs} ->
        variable = {:var, 0, String.to_atom("ProtocolStep#{index}")}

        case operation do
          {:submit, key, producer, duration} ->
            value = {:call, 0, {:atom, 0, String.to_existing_atom(producer)}, []}

            call =
              remote(Catena.Protocol.Session, :submit, [session, value, {:integer, 0, duration}])

            {body ++ [{:match, 0, variable, call}], Map.put(keys, key, variable), outputs}

          {operation, key} when operation in [:await, :cancel] ->
            call = remote(Catena.Protocol.ProgramRuntime, operation, [Map.fetch!(keys, key)])
            {body ++ [{:match, 0, variable, call}], keys, outputs ++ [variable]}
        end
      end)

    result = if outputs == [], do: {:atom, 0, :unit}, else: {:tuple, 0, outputs}
    callback = {:fun, 0, {:clauses, [{:clause, 0, [session], [], body ++ [result]}]}}

    run =
      remote(Catena.Protocol.ProgramRuntime, :run, [
        {:var, 0, :ProtocolPeer},
        :erl_parse.abstract({program.contract, wire_roles}),
        {:integer, 0, program.capacity},
        {:integer, 0, program.handshake_ns},
        {:integer, 0, program.shutdown_ns},
        callback
      ])

    entry = {:function, 0, @entry, 1, [{:clause, 0, [{:var, 0, :ProtocolPeer}], [], [run]}]}

    base
    |> Enum.map(fn
      {:attribute, ann, :export, exports} -> {:attribute, ann, :export, [{@entry, 1} | exports]}
      form -> form
    end)
    |> Kernel.++([entry])
  end

  defp remote(module, function, arguments),
    do: {:call, 0, {:remote, 0, {:atom, 0, module}, {:atom, 0, function}}, arguments}

  defp error(message), do: {:error, Diagnostic.new("PRT001", message)}
end

defmodule Catena.Protocol.ProgramRuntime do
  @moduledoc false
  alias Catena.Protocol.Session
  alias Catena.Task.Runtime

  def run(peer, {contract, wire_roles}, capacity, handshake, grace, body) do
    {:ok, ^wire_roles} = Catena.Protocol.Contract.wire_roles(contract)

    result =
      Runtime.scope(
        fn scope ->
          Session.with_session(scope, contract, peer, capacity, handshake, fn session ->
            {:program_value, body.(session)}
          end)
        end,
        grace
      )
      |> Runtime.value()

    case result do
      {:program_value, value} -> {:catena_variant, :completed, value}
      {:error, reason} -> {:catena_variant, :unavailable, {:catena_variant, reason, :unit}}
    end
  end

  def await({:ok, request}), do: outcome(Session.await(request))
  def await({:error, reason}), do: outcome({:error, reason})

  def cancel({:ok, request}) do
    case Session.cancel(request) do
      :ok -> {:catena_variant, :cancel_selected, :unit}
      error -> outcome(error)
    end
  end

  def cancel({:error, reason}), do: outcome({:error, reason})
  defp outcome({:ok, value}), do: {:catena_variant, :reply, value}
  defp outcome({:error, reason}), do: {:catena_variant, reason, :unit}
end
