defmodule Catena.Supervision.Description do
  @moduledoc "Exact checked static supervision descriptions over managed process entries."
  alias Catena.Kernel.Verifier
  alias Catena.{Diagnostic, ImplementationLimits, LanguageVersion}

  def check(core, children, flags, options \\ []) do
    requested = Keyword.get(options, :selection, LanguageVersion.legacy_selection("0.1.56"))

    with {:ok, selection} <- LanguageVersion.resolve_selection(requested),
         true <- selection.language_revision == "0.1.56" and selection.previews == [],
         :ok <- Verifier.verify(core),
         true <- core.version in ["0.1.52", "0.1.53"],
         true <- valid_flags?(flags),
         true <- is_list(children) and children != [],
         true <- Enum.all?(children, &valid_child?(&1, core)),
         true <- length(Enum.uniq_by(children, & &1.id)) == length(children) do
      {:ok,
       %{
         profile: :typed_supervision,
         selection: selection,
         core: core,
         children: children,
         flags: flags
       }}
    else
      _ -> error("invalid supervision policy, child signature, provisioning or shutdown bounds")
    end
  end

  def verify(description) do
    with %{profile: :typed_supervision} <- description,
         {:ok, expected} <-
           check(description.core, description.children, description.flags,
             selection: description.selection
           ),
         true <- expected == description do
      :ok
    else
      _ -> error("forged supervision description")
    end
  end

  def compile(description) do
    with :ok <- verify(description),
         {:ok, base} <- Catena.Task.Instrument.lower(description.core),
         {forms, specs} <- forms(base, description),
         :ok <- ImplementationLimits.validate_generated_arities(forms),
         {:ok, module, binary, warnings} <-
           Catena.OTP.Compiler.compile(forms,
             source: description.core.origin,
             frontend: "typed-supervision-0.1.56",
             specification: "0.1.56",
             artifact_version: "0.1.56",
             language_selection: description.selection
           ) do
      {:ok, manifest} = manifest(description)
      manifest = Map.put(manifest, "artifact_digest", digest(binary))

      {:ok, module, binary,
       %{
         manifest: manifest,
         description: description,
         children: specs,
         flags: description.flags,
         warnings: warnings,
         interface: nil
       }}
    end
  end

  def start_artifact(description, module, binary, manifest) do
    with :ok <- verify_artifact(description, module, binary, manifest),
         {:module, ^module} <- :code.load_binary(module, ~c"catena-supervision.beam", binary),
         {:ok, _, _, metadata} <- compile(description) do
      {:ok, Catena.Task.Managed.start_supervision(metadata.flags, metadata.children)}
    else
      {:error, %Diagnostic{}} = error -> error
      _ -> error("supervision artifact could not be loaded")
    end
  end

  def manifest(description) do
    with :ok <- verify(description) do
      data = %{
        "format" => "catena-supervision-0.1.56",
        "origin" => description.core.origin,
        "module" => description.core.module,
        "producer_revision" => description.core.version,
        "core_digest" => digest(:erlang.term_to_binary(description.core, [:deterministic])),
        "flags" =>
          Map.new(description.flags, fn {key, value} -> {Atom.to_string(key), external(value)} end),
        "children" =>
          Enum.map(description.children, fn child ->
            Map.new(child, fn {key, value} -> {Atom.to_string(key), external(value)} end)
          end)
      }

      {:ok, Map.put(data, "digest", digest(Catena.CanonicalJSON.encode(data)))}
    end
  end

  def verify_artifact(description, module, binary, manifest) do
    with {:ok, ^module, ^binary, metadata} <- compile(description),
         true <- metadata.manifest == manifest do
      :ok
    else
      _ -> error("supervision artifact does not match its checked origin and description")
    end
  end

  defp external(value) when is_atom(value), do: Atom.to_string(value)
  defp external(value), do: value
  defp digest(binary), do: :crypto.hash(:sha256, binary) |> Base.encode16(case: :lower)

  defp valid_flags?(%{strategy: strategy, intensity: intensity, period: period} = flags) do
    map_size(flags) == 3 and strategy in [:one_for_one, :one_for_all, :rest_for_one] and
      is_integer(intensity) and intensity > 0 and is_integer(period) and period > 0
  end

  defp valid_flags?(_), do: false

  defp valid_child?(
         %{
           id: id,
           process: name,
           restart: restart,
           grace_ns: grace,
           shutdown_ms: shutdown,
           provisioning: :fresh_empty
         } = child,
         core
       ) do
    shape =
      map_size(child) == 6 and is_binary(id) and id != "" and
        restart in [:permanent, :transient, :temporary] and
        is_integer(grace) and grace >= 0 and is_integer(shutdown) and shutdown > 0 and
        shutdown <= 4_294_967_295 and grace < shutdown * 1_000_000

    if shape do
      match?({:ok, _}, Catena.Entry.validate_supervised_process(core, name))
    else
      false
    end
  end

  defp valid_child?(_, _), do: false

  defp forms(base, description) do
    module = String.to_existing_atom(description.core.module)

    {functions, specs, exports} =
      description.children
      |> Enum.with_index()
      |> Enum.reduce({[], [], []}, fn {child, index}, {functions, specs, exports} ->
        entry = String.to_atom("__catena_supervised_start_#{index}")
        process = String.to_existing_atom("__catena_process_#{child.process}")

        callback =
          {:fun, 0, {:clauses, [{:clause, 0, [], [], [{:call, 0, {:atom, 0, process}, []}]}]}}

        body =
          {:call, 0, {:remote, 0, {:atom, 0, Catena.Task.Managed}, {:atom, 0, :start_link}},
           [callback, {:integer, 0, child.grace_ns}]}

        function = {:function, 0, entry, 0, [{:clause, 0, [], [], [body]}]}

        spec = %{
          id: child.id,
          start: {module, entry, []},
          restart: child.restart,
          shutdown: child.shutdown_ms,
          type: :worker,
          modules: [module]
        }

        {functions ++ [function], specs ++ [spec], exports ++ [{entry, 0}]}
      end)

    forms =
      Enum.map(base, fn
        {:attribute, ann, :export, existing} -> {:attribute, ann, :export, existing ++ exports}
        other -> other
      end)

    {forms ++ functions, specs}
  end

  defp error(message), do: {:error, Diagnostic.new("SUP001", message)}
end
