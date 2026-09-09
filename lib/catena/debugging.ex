defmodule Catena.Debugging do
  @moduledoc "Rebuilt source-bound debugging sidecars with default-redacted runtime frames."
  alias Catena.Debugging.{JSONLocations, Lowering, Origins}
  alias Catena.Calling.Descriptor

  def build(input, options \\ []) do
    with {:ok, profile} <- profile(options),
         true <- is_binary(input.source) and byte_size(input.source) <= profile.max_source_bytes,
         {:ok, path} <- normalize_path(input.path),
         {:ok, core, locations} <- check(input),
         :ok <- Catena.ImplementationLimits.validate_integer_magnitudes(core),
         :ok <- Catena.ImplementationLimits.validate_source_arities(core) do
      {forms, origins} =
        Origins.capture(path, input.source, locations, fn -> lower(core, input) end)

      source_digest = hash(input.source)

      {forms, nodes, virtual_file} =
        Lowering.build(forms, origins, source_digest, profile, core.module)

      with {:ok, module, binary, warnings} <-
             Catena.OTP.Compiler.compile_debug(forms, profile, virtual_file) do
        sidecar = %{
          version: "0.1.64",
          profile: profile,
          module: module,
          source_digest: source_digest,
          input_digest: Descriptor.digest(Map.drop(input, [:source, :path])),
          source_path: path,
          virtual_file: virtual_file,
          forms_digest: Descriptor.digest(forms),
          binary_digest: hash(binary),
          compiler_digest: Descriptor.compiler_digest(),
          nodes: nodes,
          evidence: evidence(input, locations, path)
        }

        {:ok, %{module: module, binary: binary, sidecar: sidecar, warnings: warnings}}
      end
    else
      false -> {:error, :debug_source_limit}
      error -> error
    end
  rescue
    _ -> {:error, :invalid_debug_input}
  catch
    :debug_node_limit -> {:error, :debug_node_limit}
  end

  def verify(artifact, input, options \\ []) do
    case build(input, options) do
      {:ok, ^artifact} -> :ok
      _ -> {:error, :unverified_debug_sidecar}
    end
  end

  def frames(artifact, input, stack, options \\ []) when is_list(stack) do
    with :ok <- verify(artifact, input, Keyword.get(options, :build, [])) do
      {:ok, Enum.map(stack, &frame(artifact, &1, Keyword.get(options, :values, :redacted)))}
    end
  end

  def evidence_link(artifact, input, id, options \\ []) do
    with :ok <- verify(artifact, input, options),
         {:ok, reference} <- Map.fetch(artifact.sidecar.evidence, id),
         do: {:ok, reference}
  end

  def normalize_path(path) when is_binary(path) do
    parts = path |> String.replace("\\", "/") |> String.split("/", trim: true)

    if path != "" and String.valid?(path) and not String.starts_with?(path, ["/", "\\"]) and
         not String.contains?(path, [":", "\u0000", "\n", "\r"]) and ".." not in parts do
      case Enum.reject(parts, &(&1 == ".")) do
        [] -> {:error, :invalid_debug_path}
        parts -> {:ok, Enum.join(parts, "/")}
      end
    else
      {:error, :invalid_debug_path}
    end
  end

  def normalize_path(_), do: {:error, :invalid_debug_path}

  defp profile(options) do
    profile = %{
      mode: Keyword.get(options, :mode, :sidecar),
      inline_depth: Keyword.get(options, :inline_depth, 0),
      max_chain: Keyword.get(options, :max_chain, 8),
      max_nodes: Keyword.get(options, :max_nodes, 100_000),
      max_source_bytes: Keyword.get(options, :max_source_bytes, 1_048_576)
    }

    if Enum.all?(
         Keyword.keys(options),
         &(&1 in [:mode, :inline_depth, :max_chain, :max_nodes, :max_source_bytes])
       ) and profile.mode in [:sidecar, :stripped] and profile.inline_depth in 0..8 and
         profile.max_chain in 1..64 and is_integer(profile.max_nodes) and
         profile.max_nodes in 1..1_000_000 and is_integer(profile.max_source_bytes) and
         profile.max_source_bytes > 0 do
      {:ok, profile}
    else
      {:error, :invalid_debug_profile}
    end
  end

  defp check(%{kind: :kernel, source: source}) do
    with {:ok, core} <- Catena.check_kernel(source),
         :ok <- Catena.Kernel.Verifier.verify(core),
         do: {:ok, core, %{}}
  end

  defp check(%{kind: :json, source: source}) do
    with {:ok, core} <- Catena.check_json(source),
         :ok <- Catena.TypedCore.Verifier.verify(core),
         {:ok, locations} <- JSONLocations.index(source),
         do: {:ok, core, locations}
  end

  defp check(%{kind: :foreign} = input) do
    with {:ok, core} <- Catena.Kernel.CapabilityKernel.check(input.source, input.families),
         {:ok, _} <- Catena.Foreign.Program.describe(core, input.entry, input.bindings),
         do: {:ok, core, %{}}
  end

  defp check(_), do: {:error, :unsupported_debug_source}

  defp lower(core, %{kind: :foreign, entry: entry}) do
    definition = Enum.find(core.definitions, &(&1.name == entry))
    annotation = Origins.annotation(definition.span, 1)

    {:ok, forms, _} =
      Catena.Foreign.Lowering.entry(
        Catena.Kernel.Backend.lower(core),
        entry,
        "__catena_kernel_cps_",
        annotation,
        true
      )

    forms
  end

  defp lower(%{format: :kernel_core} = core, _), do: Catena.Kernel.Backend.lower(core)
  defp lower(core, _), do: Catena.Backend.ErlangAbstract.lower(core)

  defp evidence(%{kind: :json, source: source}, locations, path) do
    {:ok, document} = JSON.decode(source)

    definitions =
      for {d, i} <- Enum.with_index(document["definitions"] || []),
          d["verification_only"] == true,
          do: {"$.definitions[#{i}]", d}

    items =
      if Map.has_key?(document, "specifications"),
        do: [{"$.specifications", document["specifications"]} | definitions],
        else: definitions

    Map.new(items, fn {locator, value} ->
      digest = Descriptor.digest(value)
      id = Descriptor.digest({:erased_evidence, locator, digest})

      {id,
       %{
         source_path: path,
         locator: locator,
         span: Catena.SourceSpan.to_map(locations[locator]),
         digest: digest
       }}
    end)
  end

  defp evidence(_, _, _), do: %{}

  defp frame(artifact, {module, function, arguments, location}, policy)
       when is_atom(module) and is_atom(function) and is_list(location) do
    arity = if is_list(arguments), do: length(arguments), else: arguments

    entry =
      if artifact.sidecar.profile.mode == :sidecar and module == artifact.module and
           Keyword.get(location, :file) == String.to_charlist(artifact.sidecar.virtual_file),
         do: Map.get(artifact.sidecar.nodes, Keyword.get(location, :line))

    %{
      module: module,
      function: function,
      arity: arity,
      origin: if(matching_function?(entry, function, arity), do: entry),
      values: values(arguments, policy)
    }
  rescue
    _ -> %{origin: nil, values: :redacted, kind: :unrecognized_frame}
  end

  defp frame(_, _, _), do: %{origin: nil, values: :redacted, kind: :unrecognized_frame}

  defp matching_function?(%{function: {:function, name, arity}}, function, actual_arity) do
    actual = Atom.to_string(function)

    (actual == name and actual_arity == arity) or
      String.starts_with?(actual, "-#{name}/#{arity}-")
  end

  defp matching_function?(_, _, _), do: false

  defp values(_, :redacted), do: :redacted

  defp values(arguments, %{codec: codec, limits: limits}) when is_list(arguments) do
    case Catena.Foreign.Codec.from_native(codec, List.to_tuple(arguments), limits) do
      {:ok, data} -> {:disclosed, data}
      _ -> :redacted
    end
  end

  defp values(_, _), do: :redacted
  defp hash(bytes), do: :crypto.hash(:sha256, bytes) |> Base.encode16(case: :lower)
end
