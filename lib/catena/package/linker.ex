defmodule Catena.Package.Linker do
  @moduledoc "Deterministic C004 specialization and transactional C006 governed package linker."

  alias Catena.Categorical.TypeTerm
  alias Catena.Governance.{Crypto, TrustRoot}
  alias Catena.Package.Manifest

  alias Catena.{
    Assurance,
    CanonicalJSON,
    Categorical,
    Diagnostic,
    Governance,
    Interface,
    LanguageVersion
  }

  alias Catena.OTP.Compiler, as: OTPCompiler
  alias Catena.Type.Trait

  @budget 20_000
  @categorical_version LanguageVersion.introduced(:traits_and_categories)
  @governance_version LanguageVersion.introduced(:specifications_and_governance)

  @spec compile_manifest(Path.t(), keyword()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def compile_manifest(path, options \\ []) do
    directory = Path.dirname(Path.expand(path))

    with {:ok, manifest} <- path |> File.read!() |> Manifest.decode(),
         :ok <- validate_paths(manifest, directory),
         {:ok, imported_interfaces} <- load_interfaces(manifest.interfaces, directory),
         {:ok, interfaces, prepared_modules} <-
           compile_modules(manifest.modules, directory, imported_interfaces, options),
         {:ok, module, companion_binary, metadata} <- link(manifest, interfaces, source: path),
         {:ok, result} <-
           finalize_package(
             manifest,
             module,
             companion_binary,
             metadata,
             prepared_modules,
             imported_interfaces,
             directory,
             options
           ) do
      :ok = commit_outputs(result.prepared_outputs)

      {:ok,
       %{
         module: module,
         output: result.output,
         module_outputs: result.module_outputs,
         specialization_keys: metadata.specialization_keys,
         evidence_erased: true,
         assurance: result.assurance,
         assurance_digest: result.assurance_digest,
         signing_payload: result.signing_payload,
         signing_payload_digest: result.signing_payload_digest,
         governance: result.governance
       }}
    end
  rescue
    error in Catena.TypeError ->
      {:error, error.diagnostic}

    error in File.Error ->
      {:error, Diagnostic.new("LNK001", Exception.message(error), path: path)}
  end

  @spec link(map(), [map()], keyword()) ::
          {:ok, module(), binary(), map()} | {:error, Diagnostic.t()}
  def link(manifest, interfaces, options \\ []) do
    protect(fn ->
      templates = template_index!(interfaces)
      registry = registry!(interfaces)

      {functions, keys, _remaining} =
        Enum.map_reduce(manifest.roots, {[], @budget}, fn root, {keys, remaining} ->
          {function, key, remaining} =
            specialize_root!(root, templates, registry, remaining, manifest.companion_module)

          {function, {[key | keys], remaining}}
        end)
        |> then(fn {functions, {keys, remaining}} ->
          {functions, Enum.reverse(keys), remaining}
        end)

      forms =
        forms(manifest.companion_module, functions, Keyword.get(options, :source, "<package>"))

      case OTPCompiler.compile(forms,
             source: Keyword.get(options, :source, "<package>"),
             frontend_version: Map.get(manifest, :version, @categorical_version),
             specification: Map.get(manifest, :version, @categorical_version)
           ) do
        {:ok, module, binary, warnings} ->
          {:ok, module, binary,
           %{
             forms: forms,
             warnings: warnings,
             specialization_keys: keys,
             evidence_erased: true
           }}

        {:error, _} = error ->
          error
      end
    end)
  end

  defp load_interfaces(paths, directory) do
    Enum.reduce_while(paths, {:ok, []}, fn path, {:ok, decoded} ->
      case path |> resolve(directory) |> File.read!() |> Interface.decode() do
        {:ok, interface} -> {:cont, {:ok, [interface | decoded]}}
        {:error, diagnostic} -> {:halt, {:error, diagnostic}}
      end
    end)
    |> case do
      {:ok, decoded} -> {:ok, Enum.reverse(decoded)}
      error -> error
    end
  rescue
    error in File.Error -> {:error, Diagnostic.new("LNK001", Exception.message(error))}
  end

  defp compile_modules(modules, directory, interfaces, options) do
    Enum.reduce_while(modules, {:ok, interfaces, []}, fn declaration, {:ok, available, outputs} ->
      source = resolve(declaration["source"], directory)

      case source
           |> File.read!()
           |> Catena.compile_json(Keyword.merge(options, interfaces: available, source: source)) do
        {:ok, _module, binary, metadata} ->
          beam = resolve(declaration["beam"], directory)
          interface = resolve(declaration["interface"], directory)

          {:cont,
           {:ok, available ++ [decoded_interface!(metadata.interface_binary)],
            [
              %{
                beam: beam,
                beam_relative: declaration["beam"],
                interface: interface,
                interface_relative: declaration["interface"],
                beam_binary: binary,
                interface_binary: metadata.interface_binary,
                core: metadata.core
              }
              | outputs
            ]}}

        {:error, diagnostic} ->
          {:halt, {:error, diagnostic}}
      end
    end)
    |> case do
      {:ok, available, outputs} -> {:ok, available, Enum.reverse(outputs)}
      error -> error
    end
  rescue
    error in File.Error -> {:error, Diagnostic.new("LNK001", Exception.message(error))}
  end

  defp finalize_package(
         %{version: @categorical_version} = manifest,
         _module,
         companion_binary,
         _metadata,
         modules,
         _interfaces,
         directory,
         _options
       ) do
    output = resolve(manifest.output, directory)

    prepared =
      module_prepared_outputs(modules) ++ [%{path: output, binary: companion_binary}]

    {:ok,
     %{
       output: output,
       module_outputs: module_output_records(modules),
       prepared_outputs: prepared,
       assurance: nil,
       assurance_digest: nil,
       signing_payload: nil,
       signing_payload_digest: nil,
       governance: nil
     }}
  end

  defp finalize_package(
         %{version: @governance_version} = manifest,
         _module,
         companion_binary,
         _metadata,
         modules,
         interfaces,
         directory,
         options
       ) do
    action = requested_action!(manifest, options)
    output = resolve(manifest.output, directory)
    assurance_output = resolve(manifest.assurance, directory)

    artifacts =
      Enum.flat_map(modules, fn module ->
        [
          %{path: module.beam_relative, kind: "beam", binary: module.beam_binary},
          %{
            path: module.interface_relative,
            kind: "interface",
            binary: module.interface_binary
          }
        ]
      end) ++ [%{path: manifest.output, kind: "companion_beam", binary: companion_binary}]

    cores = Enum.map(modules, & &1.core)

    local_claims =
      cores
      |> Enum.flat_map(&get_in(&1, [:specifications, :claims]))
      |> then(&Catena.Specification.interface_payload(%{claims: &1}))

    inherited_claims = Enum.flat_map(interfaces, & &1.claims)
    claims = (local_claims ++ inherited_claims) |> Enum.uniq_by(& &1["id"])
    compiler_evidence = Enum.flat_map(cores, &Governance.compiler_evidence(&1.specifications))

    claim_digests = Enum.map(claims, & &1["semantic_digest"])

    dependency_digests = interfaces |> Enum.map(& &1.digest) |> Enum.uniq()

    artifact_digests =
      artifacts
      |> Enum.map(&(:crypto.hash(:sha256, &1.binary) |> Base.encode16(case: :lower)))
      |> Kernel.++(dependency_digests)

    compiler_evidence =
      Enum.map(compiler_evidence, &Map.put(&1, "artifact_digests", Enum.sort(artifact_digests)))

    context = %{
      action: action,
      package: manifest.package,
      profile: manifest.profile,
      modules: Enum.map(cores, & &1.module),
      subjects:
        [%{"kind" => "output", "name" => manifest.output}] ++
          Enum.map(modules, &%{"kind" => "interface", "name" => &1.interface_relative}) ++
          (claims |> Enum.map(& &1["subject"]) |> Enum.uniq()),
      compiler_evidence: compiler_evidence,
      claims: claims,
      claim_digests: claim_digests,
      artifact_digests: artifact_digests
    }

    with :ok <- validate_package_claim_subjects(local_claims, manifest, modules),
         {:ok, bundle, root, governance_result} <-
           evaluate_governance(manifest, context, directory, options),
         signatures <- if(bundle, do: bundle.manifest_signatures, else: []),
         assurance <-
           Assurance.build(
             %{
               package: manifest.package,
               profile: manifest.profile,
               action: action,
               claims: claims,
               dependency_digests: dependency_digests
             },
             artifacts,
             cores,
             governance_result,
             signatures
           ),
         :ok <- verify_assurance_signature(action, assurance, signatures, root, governance_result) do
      prepared =
        module_prepared_outputs(modules) ++
          [
            %{path: output, binary: companion_binary},
            %{path: assurance_output, binary: assurance.binary}
          ]

      {:ok,
       %{
         output: output,
         module_outputs: module_output_records(modules),
         prepared_outputs: prepared,
         assurance: assurance_output,
         assurance_digest: assurance.digest,
         signing_payload: assurance.payload,
         signing_payload_digest: assurance.payload_digest,
         governance: governance_result
       }}
    end
  end

  defp validate_package_claim_subjects(claims, manifest, modules) do
    output_names = [manifest.output | Enum.map(modules, & &1.beam_relative)]
    interface_names = Enum.map(modules, & &1.interface_relative)

    Enum.reduce_while(claims, :ok, fn claim, :ok ->
      subject = claim["subject"]

      valid? =
        case subject do
          %{"kind" => "output", "name" => name} -> name in output_names
          %{"kind" => "interface", "name" => name} -> name in interface_names
          %{"kind" => "action", "name" => name} -> name in ~w(build publish activate)
          %{"kind" => "profile", "name" => name} -> name == manifest.profile
          _ -> true
        end

      if valid? do
        {:cont, :ok}
      else
        {:halt,
         {:error,
          Diagnostic.new(
            "SPC001",
            "claim #{claim["id"]} names unknown package subject #{inspect(subject)}",
            path: "$.specifications"
          )}}
      end
    end)
  end

  defp evaluate_governance(%{governed?: false}, %{action: "build"}, _directory, _options),
    do: {:ok, nil, nil, nil}

  defp evaluate_governance(%{governed?: false}, _context, _directory, _options),
    do:
      {:error,
       Diagnostic.new("GOV001", "an ungoverned 0.1.6 package supports only the build action",
         path: "$"
       )}

  defp evaluate_governance(manifest, context, directory, options) do
    governance_path = resolve(manifest.governance, directory)
    trust_path = Keyword.get(options, :trust_root)

    with {:ok, bundle} <- governance_path |> File.read!() |> Governance.decode_bundle(),
         true <- bundle.profile == manifest.profile,
         {:ok, root} <- load_trust_root(trust_path),
         {:ok, result} <- Governance.evaluate(bundle, root, context) do
      {:ok, bundle, root, result}
    else
      false ->
        {:error,
         Diagnostic.new("GOV001", "governance profile does not match the package manifest",
           path: "$.profile"
         )}

      {:error, _} = result ->
        result
    end
  end

  defp load_trust_root(nil), do: {:ok, nil}
  defp load_trust_root(path), do: path |> File.read!() |> TrustRoot.decode()

  defp verify_assurance_signature("build", _assurance, [], _root, _governance), do: :ok

  defp verify_assurance_signature(action, assurance, signatures, root, governance)
       when action in ~w(build publish activate) do
    cond do
      signatures == [] and action == "build" ->
        :ok

      is_nil(root) ->
        {:error,
         signing_diagnostic(
           "manifest signatures require an explicit trust root",
           assurance
         )}

      true ->
        sequence = if(governance, do: governance.sequence, else: root.sequence)

        case Crypto.verify_threshold(
               root,
               "normal",
               "manifest",
               assurance.document["signed"],
               signatures,
               sequence
             ) do
          {:ok, _signers} ->
            :ok

          {:error, reason} ->
            {:error, signing_diagnostic("manifest signature rejected: #{reason}", assurance)}
        end
    end
  end

  defp signing_diagnostic(message, assurance) do
    Diagnostic.new("GOV003", message,
      path: "$",
      details: %{
        signing_payload: assurance.payload,
        signing_payload_digest: assurance.payload_digest
      }
    )
  end

  defp normalize_action(action) when action in [:build, :publish, :activate],
    do: Atom.to_string(action)

  defp normalize_action(action) when action in ~w(build publish activate), do: action

  defp normalize_action(action),
    do: fail("GOV001", "unknown governed action #{inspect(action)}", "$.action")

  defp requested_action!(%{governed?: true}, options) do
    if Keyword.has_key?(options, :action) do
      normalize_action(Keyword.fetch!(options, :action))
    else
      fail("GOV001", "a governed package requires --action build|publish|activate", "$.action")
    end
  end

  defp requested_action!(_manifest, options),
    do: normalize_action(Keyword.get(options, :action, "build"))

  defp module_prepared_outputs(modules) do
    Enum.flat_map(modules, fn module ->
      [
        %{path: module.beam, binary: module.beam_binary},
        %{path: module.interface, binary: module.interface_binary}
      ]
    end)
  end

  defp module_output_records(modules),
    do: Enum.map(modules, &%{beam: &1.beam, interface: &1.interface})

  defp commit_outputs(outputs) do
    nonce = System.unique_integer([:positive, :monotonic])

    temporary =
      Enum.with_index(outputs)
      |> Enum.map(fn {%{path: path, binary: binary}, index} ->
        File.mkdir_p!(Path.dirname(path))
        temp = path <> ".catena-#{nonce}-#{index}.tmp"
        File.write!(temp, binary, [:binary, :exclusive])
        %{temporary: temp, final: path}
      end)

    backups =
      Enum.with_index(temporary)
      |> Enum.map(fn {%{final: final}, index} ->
        %{final: final, backup: final <> ".catena-#{nonce}-#{index}.bak"}
      end)

    try do
      Enum.each(backups, fn item ->
        if File.exists?(item.final), do: File.rename!(item.final, item.backup)
      end)

      Enum.each(temporary, fn item -> File.rename!(item.temporary, item.final) end)
      Enum.each(backups, &File.rm(&1.backup))
      :ok
    rescue
      error ->
        Enum.each(Enum.zip(temporary, backups), fn {item, backup} ->
          cond do
            File.exists?(backup.backup) ->
              File.rm(item.final)
              File.rename(backup.backup, backup.final)

            not File.exists?(item.temporary) ->
              File.rm(item.final)

            true ->
              :ok
          end
        end)

        reraise error, __STACKTRACE__
    after
      Enum.each(temporary, &File.rm(&1.temporary))
      Enum.each(backups, &File.rm(&1.backup))
    end
  end

  defp validate_paths(%{version: @categorical_version}, _directory), do: :ok

  defp validate_paths(%{version: @governance_version} = manifest, directory) do
    input_paths =
      manifest.interfaces ++
        Enum.map(manifest.modules, & &1["source"]) ++
        List.wrap(manifest.governance)

    output_paths =
      [manifest.output, manifest.assurance] ++
        Enum.flat_map(manifest.modules, &[&1["beam"], &1["interface"]])

    all_paths = input_paths ++ output_paths

    cond do
      Enum.any?(all_paths, &(not safe_relative_path?(&1, directory))) ->
        {:error,
         Diagnostic.new("ART001", "0.1.6 package paths must remain inside the manifest directory",
           path: "$"
         )}

      length(output_paths) != length(Enum.uniq(output_paths)) ->
        {:error, Diagnostic.new("ART001", "package output paths must be unique", path: "$")}

      Enum.any?(output_paths, &(&1 in input_paths)) ->
        {:error, Diagnostic.new("ART001", "package output may not overwrite an input", path: "$")}

      true ->
        :ok
    end
  end

  defp safe_relative_path?(path, directory) when is_binary(path) do
    root = Path.expand(directory)
    expanded = Path.expand(path, root)

    lexical? =
      Path.type(path) == :relative and ".." not in Path.split(path) and
        String.starts_with?(expanded, root <> "/")

    with true <- lexical?,
         {:ok, root_real} <- real_existing_path(root),
         ancestor <- existing_ancestor(expanded, root),
         {:ok, ancestor_real} <- real_existing_path(ancestor) do
      ancestor_real == root_real or String.starts_with?(ancestor_real, root_real <> "/")
    else
      _ -> false
    end
  end

  defp safe_relative_path?(_path, _directory), do: false

  defp existing_ancestor(path, root) do
    cond do
      File.exists?(path) -> path
      path == root -> root
      true -> existing_ancestor(Path.dirname(path), root)
    end
  end

  defp real_existing_path(path), do: real_existing_path(path, MapSet.new())

  defp real_existing_path(path, visited) do
    path = Path.expand(path)

    if MapSet.member?(visited, path) do
      {:error, :symlink_cycle}
    else
      path
      |> Path.split()
      |> Enum.reduce_while({:ok, ""}, fn component, {:ok, current} ->
        candidate = if current == "", do: component, else: Path.join(current, component)

        case File.lstat(candidate) do
          {:ok, %File.Stat{type: :symlink}} ->
            with {:ok, target} <- File.read_link(candidate),
                 target <- Path.expand(target, Path.dirname(candidate)),
                 {:ok, resolved} <- real_existing_path(target, MapSet.put(visited, path)) do
              {:cont, {:ok, resolved}}
            else
              _ -> {:halt, {:error, :invalid_symlink}}
            end

          {:ok, _stat} ->
            {:cont, {:ok, candidate}}

          {:error, reason} ->
            {:halt, {:error, reason}}
        end
      end)
    end
  end

  defp specialize_root!(root, templates, registry, budget, companion_module) do
    template = fetch_template!(templates, root["template"])
    types = Enum.map(root["types"], &TypeTerm.decode!/1)

    {evidence, budget} =
      Enum.map_reduce(root["instances"], budget, fn predicate, remaining ->
        ensure_budget!(remaining)
        trait = Map.fetch!(predicate, "trait")
        arguments = Enum.map(Map.fetch!(predicate, "arguments"), &TypeTerm.decode!/1)
        {Trait.resolve!(registry, trait, arguments, budget: remaining), remaining - 1}
      end)

    key = specialization_key(template, types, evidence)
    parameters = template["parameters"]

    environment =
      Map.new(Enum.with_index(parameters), fn {name, index} -> {name, variable(index)} end)

    {body, budget} =
      lower_template!(
        template["body"],
        environment,
        types,
        templates,
        registry,
        budget,
        [template["id"]]
      )

    function = %{
      name: String.to_atom(root["export"]),
      arguments: variables(length(parameters)),
      body: body,
      module: String.to_atom(companion_module)
    }

    {function, key, budget}
  end

  defp lower_template!(
         %{"tag" => "argument", "name" => name},
         environment,
         _types,
         _templates,
         _registry,
         budget,
         _stack
       ),
       do: {Map.fetch!(environment, name), spend!(budget)}

  defp lower_template!(
         %{"tag" => "integer", "value" => value},
         _environment,
         _types,
         _templates,
         _registry,
         budget,
         _stack
       )
       when is_integer(value),
       do: {{:integer, 0, value}, spend!(budget)}

  defp lower_template!(
         %{"tag" => "boolean", "value" => value},
         _environment,
         _types,
         _templates,
         _registry,
         budget,
         _stack
       )
       when is_boolean(value),
       do: {{:atom, 0, value}, spend!(budget)}

  defp lower_template!(
         %{"tag" => "tuple", "elements" => elements},
         environment,
         types,
         templates,
         registry,
         budget,
         stack
       ) do
    {elements, budget} =
      lower_many!(elements, environment, types, templates, registry, budget, stack)

    {{:tuple, 0, elements}, spend!(budget)}
  end

  defp lower_template!(
         %{"tag" => "call", "module" => module, "function" => function, "arguments" => arguments},
         environment,
         types,
         templates,
         registry,
         budget,
         stack
       ) do
    {arguments, budget} =
      lower_many!(arguments, environment, types, templates, registry, budget, stack)

    {{:call, 0,
      {:remote, 0, {:atom, 0, String.to_atom(module)}, {:atom, 0, String.to_atom(function)}},
      arguments}, spend!(budget)}
  end

  defp lower_template!(
         %{
           "tag" => "trait_call",
           "trait" => trait,
           "arguments" => arguments,
           "method" => method,
           "values" => values
         },
         environment,
         types,
         templates,
         registry,
         budget,
         stack
       ) do
    arguments = Enum.map(arguments, &substitute_type!(TypeTerm.decode!(&1), types))
    evidence = Trait.resolve!(registry, trait, arguments, budget: budget)
    implementation = Map.fetch!(evidence.methods, method)

    {values, budget} =
      lower_many!(values, environment, types, templates, registry, spend!(budget), stack)

    case method_reference!(implementation) do
      {:remote, module, function} ->
        {remote_call(module, function, values), budget}

      {:template, id} ->
        if id in stack do
          fail("TRT007", "type-growing polymorphic template recursion is forbidden", "$.roots")
        end

        template = fetch_template!(templates, id)
        nested_environment = template["parameters"] |> Enum.zip(values) |> Map.new()

        lower_template!(
          template["body"],
          nested_environment,
          types,
          templates,
          registry,
          budget,
          [
            id | stack
          ]
        )
    end
  end

  defp lower_template!(
         %{
           "tag" => "derived_collect",
           "eliminator" => eliminator,
           "constructors" => constructors,
           "context_type" => context_type,
           "callback" => callback_name,
           "subject" => subject_name
         },
         environment,
         types,
         templates,
         registry,
         budget,
         stack
       ) do
    context = context_type |> TypeTerm.decode!() |> substitute_type!(types)
    mapper = Trait.resolve!(registry, "Mapper", [context], budget: budget)
    multi = Trait.resolve!(registry, "MultiMapper", [context], budget: budget)
    embedder = Trait.resolve!(registry, "ValueEmbedder", [context], budget: budget)
    callback = Map.fetch!(environment, callback_name)
    subject = Map.fetch!(environment, subject_name)

    {handlers, budget} =
      Enum.map_reduce(constructors, spend!(budget), fn constructor, remaining ->
        lower_collect_handler!(
          constructor,
          callback,
          mapper,
          multi,
          embedder,
          templates,
          types,
          registry,
          remaining,
          stack
        )
      end)

    {module, function} = split_reference!(eliminator)
    {remote_call(module, function, handlers ++ [subject]), budget}
  end

  defp lower_template!(
         %{"tag" => "call_template", "template" => id, "arguments" => arguments},
         environment,
         types,
         templates,
         registry,
         budget,
         stack
       ) do
    if id in stack do
      fail("TRT007", "type-growing polymorphic template recursion is forbidden", "$.roots")
    end

    template = fetch_template!(templates, id)

    {arguments, budget} =
      lower_many!(arguments, environment, types, templates, registry, budget, stack)

    nested_environment =
      template["parameters"]
      |> Enum.zip(arguments)
      |> Map.new()

    lower_template!(template["body"], nested_environment, types, templates, registry, budget, [
      id | stack
    ])
  end

  defp lower_template!(body, _environment, _types, _templates, _registry, _budget, _stack),
    do: fail("TRT006", "unsupported verified template body #{inspect(body)}", "$.templates")

  defp lower_many!(values, environment, types, templates, registry, budget, stack) do
    Enum.map_reduce(values, budget, fn value, remaining ->
      lower_template!(value, environment, types, templates, registry, remaining, stack)
    end)
  end

  defp registry!(interfaces) do
    ast = %{
      frontend_version: @categorical_version,
      origin: "catena://package/linker",
      traits: [],
      instances: [],
      templates: []
    }

    data = %{types: []}
    Categorical.prepare!(ast, data, interfaces).registry
  end

  defp template_index!(interfaces) do
    templates = Enum.flat_map(interfaces, &Map.get(&1, :templates, []))

    Enum.reduce(templates, %{}, fn template, index ->
      id = template["id"]

      if Map.has_key?(index, id),
        do: fail("TRT006", "ambiguous template #{id}", "$.interfaces"),
        else: Map.put(index, id, template)
    end)
  end

  defp fetch_template!(templates, id) do
    case Map.fetch(templates, id) do
      {:ok, template} -> template
      :error -> fail("TRT006", "unknown template #{id}", "$.roots")
    end
  end

  defp substitute_type!({:variable, "$type" <> index, _kind}, types) do
    Enum.fetch!(types, String.to_integer(index))
  end

  defp substitute_type!({:application, callee, argument}, types),
    do: {:application, substitute_type!(callee, types), substitute_type!(argument, types)}

  defp substitute_type!(type, _types), do: type

  defp method_reference!("template:" <> id) when byte_size(id) > 0, do: {:template, id}

  defp method_reference!(reference) when is_binary(reference) do
    {module, function} = split_reference!(reference)
    {:remote, module, function}
  end

  defp lower_collect_handler!(
         constructor,
         callback,
         mapper,
         multi,
         embedder,
         templates,
         types,
         registry,
         budget,
         stack
       ) do
    fields =
      if constructor["arity"] == 0 do
        []
      else
        Enum.map(0..(constructor["arity"] - 1), fn index ->
          {:var, 0, String.to_atom("Collect#{constructor["index"]}Field#{index}")}
        end)
      end

    targeted =
      constructor["targets"]
      |> Enum.with_index()
      |> Enum.reject(fn {target, _index} -> is_nil(target) end)

    {body, budget} =
      case targeted do
        [] ->
          {constructed, budget} =
            lower_constructor_call!(
              constructor["helper"],
              fields,
              templates,
              types,
              registry,
              budget,
              stack
            )

          {evidence_call!(embedder, "from_value", [constructed]), spend!(budget)}

        [{_target, field_index}] ->
          context = {:call, 0, callback, [Enum.at(fields, field_index)]}
          first_list = unary_fun("CollectedFirst", fn value -> list_expression([value]) end)
          accumulator = evidence_call!(mapper, "map", [first_list, context])

          {reconstruct, budget} =
            reconstruction_fun!(
              constructor,
              fields,
              templates,
              types,
              registry,
              budget,
              stack
            )

          {evidence_call!(mapper, "map", [reconstruct, accumulator]), spend!(budget)}

        [{_target, first_index} | rest] ->
          first_context = {:call, 0, callback, [Enum.at(fields, first_index)]}
          first_list = unary_fun("CollectedFirst", fn value -> list_expression([value]) end)
          accumulator = evidence_call!(mapper, "map", [first_list, first_context])

          {accumulator, budget} =
            Enum.reduce(rest, {accumulator, spend!(budget)}, fn {_target, field_index},
                                                                {current, remaining} ->
              next_context = {:call, 0, callback, [Enum.at(fields, field_index)]}

              {evidence_call!(multi, "map2", [append_fun(), current, next_context]),
               spend!(remaining)}
            end)

          {reconstruct, budget} =
            reconstruction_fun!(
              constructor,
              fields,
              templates,
              types,
              registry,
              budget,
              stack
            )

          {evidence_call!(mapper, "map", [reconstruct, accumulator]), spend!(budget)}
      end

    {curried_fun(fields, body), budget}
  end

  defp reconstruction_fun!(constructor, fields, templates, types, registry, budget, stack) do
    collected = {:var, 0, :CollectedValues}

    {arguments, _position} =
      Enum.map_reduce(Enum.zip(constructor["targets"], fields), 1, fn
        {nil, field}, position ->
          {field, position}

        {_target, _field}, position ->
          {remote_call("lists", "nth", [{:integer, 0, position}, collected]), position + 1}
      end)

    {constructed, budget} =
      lower_constructor_call!(
        constructor["helper"],
        arguments,
        templates,
        types,
        registry,
        budget,
        stack
      )

    {unary_fun("CollectedValues", fn _value -> constructed end), budget}
  end

  defp lower_constructor_call!(id, arguments, templates, types, registry, budget, stack) do
    template = fetch_template!(templates, id)
    environment = template["parameters"] |> Enum.zip(arguments) |> Map.new()

    lower_template!(template["body"], environment, types, templates, registry, budget, [
      id | stack
    ])
  end

  defp evidence_call!(evidence, method, arguments) do
    case evidence.methods |> Map.fetch!(method) |> method_reference!() do
      {:remote, module, function} ->
        remote_call(module, function, arguments)

      {:template, _id} ->
        fail("TRT006", "context operations must specialize to direct methods", "$.instances")
    end
  end

  defp split_reference!(reference) do
    pieces = String.split(reference, ".")

    case pieces do
      [_single] ->
        fail("TRT006", "instance method reference must be Module.function", "$.instances")

      ["Elixir" | _rest] ->
        {module, [function]} = Enum.split(pieces, length(pieces) - 1)
        {Enum.join(module, "."), function}

      [module | function] ->
        {module, Enum.join(function, ".")}
    end
  end

  defp remote_call(module, function, arguments) do
    {:call, 0,
     {:remote, 0, {:atom, 0, String.to_atom(module)}, {:atom, 0, String.to_atom(function)}},
     arguments}
  end

  defp unary_fun(name, body) do
    variable = {:var, 0, String.to_atom(name)}
    {:fun, 0, {:clauses, [{:clause, 0, [variable], [], [body.(variable)]}]}}
  end

  defp append_fun do
    collected = {:var, 0, :CollectedSoFar}
    item = {:var, 0, :CollectedNext}
    body = {:op, 0, :++, collected, list_expression([item])}
    inner = {:fun, 0, {:clauses, [{:clause, 0, [item], [], [body]}]}}
    {:fun, 0, {:clauses, [{:clause, 0, [collected], [], [inner]}]}}
  end

  defp curried_fun([], body), do: body

  defp curried_fun([field | rest], body) do
    inner = curried_fun(rest, body)
    {:fun, 0, {:clauses, [{:clause, 0, [field], [], [inner]}]}}
  end

  defp list_expression(elements),
    do: Enum.reduce(Enum.reverse(elements), {nil, 0}, &{:cons, 0, &1, &2})

  defp specialization_key(template, types, evidence) do
    payload = %{
      template: template,
      types: Enum.map(types, &TypeTerm.encode/1),
      instances: Enum.map(evidence, & &1.digest),
      compiler: Application.spec(:catena, :vsn) |> to_string(),
      specification: @categorical_version,
      standard: Categorical.Standard.interface!()["digest"]
    }

    :crypto.hash(:sha256, CanonicalJSON.encode(payload)) |> Base.encode16(case: :lower)
  end

  defp forms(module, functions, source) do
    module = String.to_atom(module)
    exports = Enum.map(functions, &{&1.name, length(&1.arguments)})

    definitions =
      Enum.map(functions, fn function ->
        clause = {:clause, 0, function.arguments, [], [function.body]}
        {:function, 0, function.name, length(function.arguments), [clause]}
      end)

    [
      {:attribute, 0, :file, {String.to_charlist(source), 1}},
      {:attribute, 0, :module, module},
      {:attribute, 0, :export, exports}
      | definitions
    ]
  end

  defp variable(index), do: {:var, 0, String.to_atom("Argument#{index}")}
  defp variables(0), do: []
  defp variables(count), do: Enum.map(0..(count - 1), &variable/1)

  defp spend!(remaining) when remaining > 0, do: remaining - 1
  defp spend!(_remaining), do: fail("TRT007", "specialization exceeded 20000 steps", "$.roots")
  defp ensure_budget!(remaining) when remaining > 0, do: :ok

  defp ensure_budget!(_remaining),
    do: fail("TRT007", "specialization exceeded 20000 steps", "$.roots")

  defp decoded_interface!(binary) do
    case Interface.decode(binary) do
      {:ok, interface} -> interface
      {:error, diagnostic} -> raise Catena.TypeError, diagnostic: diagnostic
    end
  end

  defp resolve(path, directory),
    do: if(Path.type(path) == :absolute, do: path, else: Path.join(directory, path))

  defp protect(function) do
    function.()
  rescue
    error in Catena.TypeError -> {:error, error.diagnostic}
    error in KeyError -> {:error, Diagnostic.new("LNK001", Exception.message(error))}
  end

  defp fail(id, message, path),
    do: raise(Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path))
end
