defmodule Catena.Package.Linker do
  @moduledoc "Deterministic Catena 0.4 package specialization and evidence-erasing BEAM linker."

  alias Catena.Categorical.TypeTerm
  alias Catena.Package.Manifest
  alias Catena.{CanonicalJSON, Categorical, Diagnostic, Interface}
  alias Catena.OTP.Compiler, as: OTPCompiler
  alias Catena.Type.Trait

  @budget 20_000

  @spec compile_manifest(Path.t(), keyword()) :: {:ok, map()} | {:error, Diagnostic.t()}
  def compile_manifest(path, options \\ []) do
    directory = Path.dirname(Path.expand(path))

    with {:ok, manifest} <- path |> File.read!() |> Manifest.decode(),
         {:ok, interfaces} <- load_interfaces(manifest.interfaces, directory),
         {:ok, interfaces, module_outputs} <-
           compile_modules(manifest.modules, directory, interfaces, options),
         {:ok, module, binary, metadata} <- link(manifest, interfaces, source: path) do
      output = resolve(manifest.output, directory)
      File.mkdir_p!(Path.dirname(output))
      File.write!(output, binary)

      {:ok,
       %{
         module: module,
         output: output,
         module_outputs: module_outputs,
         specialization_keys: metadata.specialization_keys,
         evidence_erased: true
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
             frontend_version: "0.4",
             specification: "0.4"
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
          File.mkdir_p!(Path.dirname(beam))
          File.mkdir_p!(Path.dirname(interface))
          File.write!(beam, binary)
          File.write!(interface, metadata.interface_binary)

          {:cont,
           {:ok, available ++ [decoded_interface!(metadata.interface_binary)],
            [%{beam: beam, interface: interface} | outputs]}}

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
      frontend_version: "0.4",
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
      specification: "0.4",
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
