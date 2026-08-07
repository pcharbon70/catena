defmodule Catena.Kernel.Backend do
  @moduledoc "Fixed-layout Erlang Abstract Format lowering for verified kernel 0.1.8 core."

  alias Catena.{Diagnostic, LanguageSelection}
  alias Catena.Kernel.{Interface, Verifier}
  alias Catena.OTP.Compiler, as: OTPCompiler

  @spec compile(map(), keyword()) ::
          {:ok, module(), binary(), map()} | {:error, Diagnostic.t()}
  def compile(core, _options \\ []) do
    with :ok <- verify(core),
         forms <- lower(core),
         selection <- %LanguageSelection{edition: "0.1", language_revision: "0.1.8", previews: []},
         {:ok, module, binary, warnings} <-
           OTPCompiler.compile(forms,
             source: core.origin,
             artifact_version: "0.1.8",
             frontend_version: "0.1.8",
             frontend: "kernel-sexpr-0.1.8",
             specification: "0.1.8",
             language_selection: selection
           ) do
      interface = Interface.build(core)

      {:ok, module, binary,
       %{
         core: core,
         forms: forms,
         warnings: warnings,
         diagnostics: core.diagnostics,
         selection: selection,
         artifact_version: "0.1.8",
         layout: :fixed,
         interface: interface,
         interface_binary: Interface.encode(interface)
       }}
    end
  end

  @spec lower(map()) :: [term()]
  def lower(core) do
    annotation = 1
    module = safe_atom(core.module)

    globals =
      Map.new(core.definitions, fn definition ->
        {definition.name,
         %{arity: definition.arity, effectful?: effect_control?(definition.expression)}}
      end)

    value_exports =
      core.definitions
      |> Enum.filter(&(&1.name in core.exports.values))
      |> Enum.map(&{safe_atom(&1.name), &1.arity})

    process_exports =
      core.processes
      |> Enum.filter(&(&1.name in core.exports.processes))
      |> Enum.map(&{safe_atom(&1.spawn_symbol), length(&1.parameters)})

    attributes = [
      {:attribute, annotation, :file, {String.to_charlist(core.origin), 1}},
      {:attribute, annotation, :module, module},
      {:attribute, annotation, :export, value_exports ++ process_exports}
    ]

    definitions = Enum.flat_map(core.definitions, &lower_definition(&1, globals))
    processes = Enum.flat_map(core.processes, &lower_process(&1, globals, module))
    attributes ++ definitions ++ processes
  end

  defp verify(core) do
    case Verifier.verify(core) do
      :ok ->
        :ok

      {:error, reason} ->
        {:error, Diagnostic.new("I001", "kernel-core verification failed: #{reason}")}
    end
  end

  defp lower_definition(definition, globals) do
    if effect_control?(definition.expression) do
      lower_effect_definition(definition, globals)
    else
      [lower_direct_definition(definition, globals)]
    end
  end

  defp lower_direct_definition(definition, globals) do
    annotation = annotation(definition.span)

    arguments =
      if definition.arity == 0 do
        []
      else
        Enum.map(1..definition.arity, &{:var, annotation, String.to_atom("__Catena_Arg#{&1}")})
      end

    value = lower_expression(definition.expression, %{}, globals, nil)

    body =
      Enum.reduce(arguments, value, fn argument, function ->
        {:call, annotation, function, [argument]}
      end)

    clause = {:clause, annotation, arguments, [], [body]}
    {:function, annotation, safe_atom(definition.name), definition.arity, [clause]}
  end

  defp lower_process(process, globals, module) do
    annotation = annotation(process.span)

    arguments =
      Enum.map(process.parameters, fn parameter ->
        {:var, annotation, variable_atom(parameter.name, parameter.span)}
      end)

    environment =
      process.parameters
      |> Enum.zip(arguments)
      |> Map.new(fn {parameter, {:var, _annotation, variable}} -> {parameter.name, variable} end)

    worker = safe_atom("__catena_process_#{process.name}")
    spawn = safe_atom(process.spawn_symbol)

    body =
      if effect_control?(process.body) do
        lower_cps(
          process.body,
          environment,
          globals,
          module,
          {:map, annotation, []},
          identity_fun(annotation)
        )
      else
        lower_expression(process.body, environment, globals, module)
      end

    worker_clause = {:clause, annotation, arguments, [], [body]}
    worker_function = {:function, annotation, worker, length(arguments), [worker_clause]}

    worker_call = {:call, annotation, {:atom, annotation, worker}, arguments}
    fun_clause = {:clause, annotation, [], [], [worker_call]}
    fun = {:fun, annotation, {:clauses, [fun_clause]}}

    spawn_call =
      {:call, annotation,
       {:remote, annotation, {:atom, annotation, :erlang}, {:atom, annotation, :spawn}}, [fun]}

    spawn_clause = {:clause, annotation, arguments, [], [spawn_call]}
    spawn_function = {:function, annotation, spawn, length(arguments), [spawn_clause]}
    [spawn_function, worker_function]
  end

  defp lower_expression(
         %{tag: :integer, value: value} = expression,
         _environment,
         _globals,
         _module
       ),
       do: {:integer, annotation(expression.span), value}

  defp lower_expression(
         %{tag: :boolean, value: value} = expression,
         _environment,
         _globals,
         _module
       ),
       do: {:atom, annotation(expression.span), value}

  defp lower_expression(%{tag: :unit} = expression, _environment, _globals, _module),
    do: {:atom, annotation(expression.span), :unit}

  defp lower_expression(%{tag: :variable, name: name} = expression, environment, globals, _module) do
    annotation = annotation(expression.span)

    case Map.fetch(environment, name) do
      {:ok, variable} ->
        {:var, annotation, variable}

      :error ->
        case Map.fetch!(globals, name).arity do
          0 -> {:call, annotation, {:atom, annotation, safe_atom(name)}, []}
          arity -> curried_global(name, arity, annotation)
        end
    end
  end

  defp lower_expression(%{tag: :function} = expression, environment, globals, module) do
    annotation = annotation(expression.span)
    variable = variable_atom(expression.parameter, expression.span)

    body =
      lower_expression(
        expression.body,
        Map.put(environment, expression.parameter, variable),
        globals,
        module
      )

    clause = {:clause, annotation, [{:var, annotation, variable}], [], [body]}
    {:fun, annotation, {:clauses, [clause]}}
  end

  defp lower_expression(
         %{tag: :call, callee: %{tag: :variable, name: name}, arguments: arguments} = expression,
         environment,
         globals,
         module
       ) do
    annotation = annotation(expression.span)

    case {Map.has_key?(environment, name), Map.get(globals, name)} do
      {false, %{arity: arity}} when arity == length(arguments) ->
        {:call, annotation, {:atom, annotation, safe_atom(name)},
         Enum.map(arguments, &lower_expression(&1, environment, globals, module))}

      _ ->
        lower_general_call(expression, environment, globals, module)
    end
  end

  defp lower_expression(%{tag: :call} = expression, environment, globals, module),
    do: lower_general_call(expression, environment, globals, module)

  defp lower_expression(%{tag: :let} = expression, environment, globals, module) do
    annotation = annotation(expression.span)
    variable = variable_atom(expression.name, expression.span)
    value = lower_expression(expression.value, environment, globals, module)
    match = {:match, annotation, {:var, annotation, variable}, value}

    body =
      lower_expression(
        expression.body,
        Map.put(environment, expression.name, variable),
        globals,
        module
      )

    {:block, annotation, [match, body]}
  end

  defp lower_expression(%{tag: :sequence} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    {:block, annotation,
     [
       lower_expression(expression.first, environment, globals, module),
       lower_expression(expression.second, environment, globals, module)
     ]}
  end

  defp lower_expression(%{tag: :tuple} = expression, environment, globals, module) do
    {:tuple, annotation(expression.span),
     Enum.map(expression.elements, &lower_expression(&1, environment, globals, module))}
  end

  defp lower_expression(%{tag: :annotate} = expression, environment, globals, module),
    do: lower_expression(expression.expression, environment, globals, module)

  defp lower_expression(%{tag: :unary} = expression, environment, globals, module) do
    {:op, annotation(expression.span), erlang_operator(expression.operator),
     lower_expression(expression.operand, environment, globals, module)}
  end

  defp lower_expression(%{tag: :binary} = expression, environment, globals, module) do
    {:op, annotation(expression.span), erlang_operator(expression.operator),
     lower_expression(expression.left, environment, globals, module),
     lower_expression(expression.right, environment, globals, module)}
  end

  defp lower_expression(%{tag: :record} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    fields =
      Enum.map(expression.fields, fn field ->
        {:map_field_assoc, annotation(field.span),
         {:atom, annotation(field.span), safe_atom(field.label)},
         lower_expression(field.expression, environment, globals, module)}
      end)

    {:map, annotation, fields}
  end

  defp lower_expression(%{tag: :select} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    remote_call(
      :maps,
      :get,
      [
        {:atom, annotation, safe_atom(expression.label)},
        lower_expression(expression.record, environment, globals, module)
      ],
      annotation
    )
  end

  defp lower_expression(%{tag: tag} = expression, environment, globals, module)
       when tag in [:update, :extend] do
    annotation = annotation(expression.span)
    field_tag = if tag == :update, do: :map_field_exact, else: :map_field_assoc

    {:map, annotation, lower_expression(expression.record, environment, globals, module),
     [
       {field_tag, annotation, {:atom, annotation, safe_atom(expression.label)},
        lower_expression(expression.value, environment, globals, module)}
     ]}
  end

  defp lower_expression(%{tag: :restrict} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    remote_call(
      :maps,
      :remove,
      [
        {:atom, annotation, safe_atom(expression.label)},
        lower_expression(expression.record, environment, globals, module)
      ],
      annotation
    )
  end

  defp lower_expression(%{tag: :inject} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    {:tuple, annotation,
     [
       {:atom, annotation, :catena_variant},
       {:atom, annotation, safe_atom(expression.label)},
       lower_expression(expression.payload, environment, globals, module)
     ]}
  end

  defp lower_expression(%{tag: :construct} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    constructor_value(
      expression.selected_constructor.name,
      Enum.map(expression.arguments, &lower_expression(&1, environment, globals, module)),
      annotation
    )
  end

  defp lower_expression(%{tag: :match} = expression, environment, globals, module) do
    annotation = annotation(expression.span)
    scrutinee = lower_expression(expression.scrutinee, environment, globals, module)
    clauses = lower_clauses(expression.clauses, environment, globals, module, false)
    {:case, annotation, scrutinee, clauses}
  end

  defp lower_expression(%{tag: :trait_call} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    {:call, annotation, {:atom, annotation, safe_atom(expression.selected_definition)},
     Enum.map(expression.arguments, &lower_expression(&1, environment, globals, module))}
  end

  defp lower_expression(%{tag: :spawn} = expression, environment, globals, module) do
    annotation = annotation(expression.span)
    entry = expression.selected_entry

    arguments =
      Enum.map(expression.arguments, &lower_expression(&1, environment, globals, module))

    if entry.module == module_name(module) do
      {:call, annotation, {:atom, annotation, safe_atom(entry.spawn_symbol)}, arguments}
    else
      remote_call(safe_atom(entry.module), safe_atom(entry.spawn_symbol), arguments, annotation)
    end
  end

  defp lower_expression(%{tag: :self} = expression, _environment, _globals, _module),
    do: remote_call(:erlang, :self, [], annotation(expression.span))

  defp lower_expression(%{tag: :send} = expression, environment, globals, module) do
    annotation = annotation(expression.span)

    send =
      {:op, annotation, :!, lower_expression(expression.left, environment, globals, module),
       lower_expression(expression.right, environment, globals, module)}

    {:block, annotation, [send, {:atom, annotation, :unit}]}
  end

  defp lower_expression(%{tag: :receive} = expression, environment, globals, module) do
    {:receive, annotation(expression.span),
     lower_clauses(expression.clauses, environment, globals, module, true)}
  end

  defp lower_expression(%{tag: :trap} = expression, environment, globals, module) do
    annotation = annotation(expression.span)
    reason = lower_expression(expression.expression, environment, globals, module)
    tuple = {:tuple, annotation, [{:atom, annotation, :catena_trap}, reason]}
    remote_call(:erlang, :error, [tuple], annotation)
  end

  defp lower_effect_definition(definition, globals) do
    annotation = annotation(definition.span)
    {parameters, body} = unwrap_functions(definition.expression, definition.arity, [])

    arguments =
      Enum.map(parameters, fn {name, span} ->
        {:var, annotation, variable_atom(name, span)}
      end)

    environment =
      parameters
      |> Enum.zip(arguments)
      |> Map.new(fn {{name, _span}, {:var, _annotation, variable}} -> {name, variable} end)

    handlers_variable = :__Catena_Kernel_Handlers
    continuation_variable = :__Catena_Kernel_Continuation
    handlers = {:var, annotation, handlers_variable}
    continuation = {:var, annotation, continuation_variable}

    worker_body = lower_cps(body, environment, globals, nil, handlers, continuation)

    worker_arguments =
      arguments ++
        [
          {:var, annotation, handlers_variable},
          {:var, annotation, continuation_variable}
        ]

    worker_clause = {:clause, annotation, worker_arguments, [], [worker_body]}
    worker_name = cps_worker_atom(definition.name)
    worker = {:function, annotation, worker_name, length(worker_arguments), [worker_clause]}

    wrapper_call =
      {:call, annotation, {:atom, annotation, worker_name},
       arguments ++ [{:map, annotation, []}, identity_fun(annotation)]}

    wrapper_clause = {:clause, annotation, arguments, [], [wrapper_call]}

    wrapper =
      {:function, annotation, safe_atom(definition.name), length(arguments), [wrapper_clause]}

    [wrapper, worker]
  end

  defp lower_cps(
         %{tag: :integer, value: value} = expression,
         _environment,
         _globals,
         _module,
         _handlers,
         k
       ),
       do:
         call_continuation(
           k,
           {:integer, annotation(expression.span), value},
           annotation(expression.span)
         )

  defp lower_cps(
         %{tag: :boolean, value: value} = expression,
         _environment,
         _globals,
         _module,
         _handlers,
         k
       ),
       do:
         call_continuation(
           k,
           {:atom, annotation(expression.span), value},
           annotation(expression.span)
         )

  defp lower_cps(%{tag: :unit} = expression, _environment, _globals, _module, _handlers, k),
    do:
      call_continuation(
        k,
        {:atom, annotation(expression.span), :unit},
        annotation(expression.span)
      )

  defp lower_cps(%{tag: :variable} = expression, environment, globals, _module, handlers, k) do
    annotation = annotation(expression.span)

    case Map.fetch(environment, expression.name) do
      {:ok, variable} ->
        call_continuation(k, {:var, annotation, variable}, annotation)

      :error ->
        global = Map.fetch!(globals, expression.name)

        cond do
          global.arity == 0 and global.effectful? ->
            {:call, annotation, {:atom, annotation, cps_worker_atom(expression.name)},
             [handlers, k]}

          global.arity == 0 ->
            value = {:call, annotation, {:atom, annotation, safe_atom(expression.name)}, []}
            call_continuation(k, value, annotation)

          true ->
            value = curried_cps_global(expression.name, global, handlers, annotation)
            call_continuation(k, value, annotation)
        end
    end
  end

  defp lower_cps(%{tag: :function} = expression, environment, globals, module, _handlers, k) do
    annotation = annotation(expression.span)
    argument_variable = variable_atom(expression.parameter, expression.span)
    handlers_variable = String.to_atom("__Catena_LambdaHandlers_#{expression.span.byte_start}")
    continuation_variable = String.to_atom("__Catena_LambdaK_#{expression.span.byte_start}")

    body =
      lower_cps(
        expression.body,
        Map.put(environment, expression.parameter, argument_variable),
        globals,
        module,
        {:var, annotation, handlers_variable},
        {:var, annotation, continuation_variable}
      )

    clause =
      {:clause, annotation,
       [
         {:var, annotation, argument_variable},
         {:var, annotation, handlers_variable},
         {:var, annotation, continuation_variable}
       ], [], [body]}

    call_continuation(k, {:fun, annotation, {:clauses, [clause]}}, annotation)
  end

  defp lower_cps(
         %{tag: :call, callee: %{tag: :variable, name: name}, arguments: arguments} = expression,
         environment,
         globals,
         module,
         handlers,
         k
       ) do
    case {Map.has_key?(environment, name), Map.get(globals, name)} do
      {false, %{arity: arity} = global} when arity == length(arguments) ->
        lower_values_cps(arguments, environment, globals, module, handlers, fn values ->
          annotation = annotation(expression.span)

          if global.effectful? do
            {:call, annotation, {:atom, annotation, cps_worker_atom(name)},
             values ++ [handlers, k]}
          else
            value = {:call, annotation, {:atom, annotation, safe_atom(name)}, values}
            call_continuation(k, value, annotation)
          end
        end)

      _ ->
        lower_general_cps_call(expression, environment, globals, module, handlers, k)
    end
  end

  defp lower_cps(%{tag: :call} = expression, environment, globals, module, handlers, k),
    do: lower_general_cps_call(expression, environment, globals, module, handlers, k)

  defp lower_cps(%{tag: :let} = expression, environment, globals, module, handlers, k) do
    variable = variable_atom(expression.name, expression.span)
    annotation = annotation(expression.span)

    continuation =
      continuation_fun(
        variable,
        lower_cps(
          expression.body,
          Map.put(environment, expression.name, variable),
          globals,
          module,
          handlers,
          k
        ),
        annotation
      )

    lower_cps(expression.value, environment, globals, module, handlers, continuation)
  end

  defp lower_cps(%{tag: :sequence} = expression, environment, globals, module, handlers, k) do
    ignored = String.to_atom("__Catena_Ignored_#{expression.span.byte_start}")

    continuation =
      continuation_fun(
        ignored,
        lower_cps(expression.second, environment, globals, module, handlers, k),
        annotation(expression.span)
      )

    lower_cps(expression.first, environment, globals, module, handlers, continuation)
  end

  defp lower_cps(%{tag: :tuple} = expression, environment, globals, module, handlers, k) do
    lower_values_cps(expression.elements, environment, globals, module, handlers, fn values ->
      value = {:tuple, annotation(expression.span), values}
      call_continuation(k, value, annotation(expression.span))
    end)
  end

  defp lower_cps(%{tag: :annotate} = expression, environment, globals, module, handlers, k),
    do: lower_cps(expression.expression, environment, globals, module, handlers, k)

  defp lower_cps(%{tag: :unary} = expression, environment, globals, module, handlers, k) do
    variable = cps_variable("Unary", expression.span, 0)
    annotation = annotation(expression.span)
    value = {:op, annotation, erlang_operator(expression.operator), {:var, annotation, variable}}
    continuation = continuation_fun(variable, call_continuation(k, value, annotation), annotation)
    lower_cps(expression.operand, environment, globals, module, handlers, continuation)
  end

  defp lower_cps(
         %{tag: :binary, operator: operator} = expression,
         environment,
         globals,
         module,
         handlers,
         k
       )
       when operator in [:and, :or] do
    annotation = annotation(expression.span)
    left = cps_variable("Lazy", expression.span, 0)
    decisive = if operator == :and, do: false, else: true

    clauses = [
      {:clause, annotation, [{:atom, annotation, decisive}], [],
       [call_continuation(k, {:atom, annotation, decisive}, annotation)]},
      {:clause, annotation, [{:atom, annotation, not decisive}], [],
       [lower_cps(expression.right, environment, globals, module, handlers, k)]}
    ]

    continuation =
      continuation_fun(left, {:case, annotation, {:var, annotation, left}, clauses}, annotation)

    lower_cps(expression.left, environment, globals, module, handlers, continuation)
  end

  defp lower_cps(%{tag: :binary} = expression, environment, globals, module, handlers, k) do
    annotation = annotation(expression.span)

    lower_values_cps(
      [expression.left, expression.right],
      environment,
      globals,
      module,
      handlers,
      fn [left, right] ->
        value = {:op, annotation, erlang_operator(expression.operator), left, right}
        call_continuation(k, value, annotation)
      end
    )
  end

  defp lower_cps(%{tag: :record} = expression, environment, globals, module, handlers, k) do
    expressions = Enum.map(expression.fields, & &1.expression)

    lower_values_cps(expressions, environment, globals, module, handlers, fn values ->
      fields =
        Enum.zip(expression.fields, values)
        |> Enum.map(fn {field, value} ->
          annotation = annotation(field.span)
          {:map_field_assoc, annotation, {:atom, annotation, safe_atom(field.label)}, value}
        end)

      call_continuation(
        k,
        {:map, annotation(expression.span), fields},
        annotation(expression.span)
      )
    end)
  end

  defp lower_cps(%{tag: :select} = expression, environment, globals, module, handlers, k) do
    lower_values_cps([expression.record], environment, globals, module, handlers, fn [record] ->
      annotation = annotation(expression.span)

      value =
        remote_call(
          :maps,
          :get,
          [{:atom, annotation, safe_atom(expression.label)}, record],
          annotation
        )

      call_continuation(k, value, annotation)
    end)
  end

  defp lower_cps(%{tag: tag} = expression, environment, globals, module, handlers, k)
       when tag in [:update, :extend] do
    lower_values_cps(
      [expression.record, expression.value],
      environment,
      globals,
      module,
      handlers,
      fn [record, value] ->
        annotation = annotation(expression.span)
        field_tag = if tag == :update, do: :map_field_exact, else: :map_field_assoc

        result =
          {:map, annotation, record,
           [{field_tag, annotation, {:atom, annotation, safe_atom(expression.label)}, value}]}

        call_continuation(k, result, annotation)
      end
    )
  end

  defp lower_cps(%{tag: :restrict} = expression, environment, globals, module, handlers, k) do
    lower_values_cps([expression.record], environment, globals, module, handlers, fn [record] ->
      annotation = annotation(expression.span)

      value =
        remote_call(
          :maps,
          :remove,
          [{:atom, annotation, safe_atom(expression.label)}, record],
          annotation
        )

      call_continuation(k, value, annotation)
    end)
  end

  defp lower_cps(%{tag: :inject} = expression, environment, globals, module, handlers, k) do
    lower_values_cps([expression.payload], environment, globals, module, handlers, fn [payload] ->
      annotation = annotation(expression.span)

      value =
        {:tuple, annotation,
         [
           {:atom, annotation, :catena_variant},
           {:atom, annotation, safe_atom(expression.label)},
           payload
         ]}

      call_continuation(k, value, annotation)
    end)
  end

  defp lower_cps(%{tag: :construct} = expression, environment, globals, module, handlers, k) do
    lower_values_cps(expression.arguments, environment, globals, module, handlers, fn fields ->
      annotation = annotation(expression.span)
      value = constructor_value(expression.selected_constructor.name, fields, annotation)
      call_continuation(k, value, annotation)
    end)
  end

  defp lower_cps(%{tag: :match} = expression, environment, globals, module, handlers, k) do
    scrutinee = cps_variable("Match", expression.span, 0)
    annotation = annotation(expression.span)

    clauses = lower_cps_clauses(expression.clauses, environment, globals, module, handlers, k)

    continuation =
      continuation_fun(
        scrutinee,
        {:case, annotation, {:var, annotation, scrutinee}, clauses},
        annotation
      )

    lower_cps(expression.scrutinee, environment, globals, module, handlers, continuation)
  end

  defp lower_cps(%{tag: :trait_call} = expression, environment, globals, module, handlers, k) do
    call =
      Map.merge(expression, %{
        tag: :call,
        callee: %{tag: :variable, name: expression.selected_definition}
      })

    lower_cps(call, environment, globals, module, handlers, k)
  end

  defp lower_cps(%{tag: :handle} = expression, environment, globals, module, handlers, k) do
    annotation = annotation(expression.span)
    handler = expression.selected_handler
    installed_variable = cps_variable("InstalledHandlers", expression.span, 0)
    installed_handlers = {:var, annotation, installed_variable}
    handler_fun = build_handler_fun(handler, environment, globals, module, handlers)

    installed_value =
      {:map, annotation, handlers,
       [
         {:map_field_assoc, annotation, {:atom, annotation, safe_atom(handler.effect)},
          handler_fun}
       ]}

    return_variable = cps_variable("HandledReturn", expression.span, 1)

    return_environment =
      Map.put(environment, handler.return.parameter, return_variable)

    return_body =
      lower_cps(
        handler.return.body,
        return_environment,
        globals,
        module,
        handlers,
        identity_fun(annotation)
      )

    return_k = continuation_fun(return_variable, return_body, annotation)

    inner =
      lower_cps(
        expression.expression,
        environment,
        globals,
        module,
        installed_handlers,
        return_k
      )

    {:block, annotation,
     [
       {:match, annotation, installed_handlers, installed_value},
       call_continuation(k, inner, annotation)
     ]}
  end

  defp lower_cps(%{tag: :request} = expression, environment, globals, module, handlers, k) do
    lower_values_cps(expression.arguments, environment, globals, module, handlers, fn values ->
      annotation = annotation(expression.span)
      handler_variable = cps_variable("SelectedHandler", expression.span, 0)
      reply = cps_variable("Reply", expression.span, 0)

      continuation =
        continuation_fun(
          reply,
          call_continuation(k, {:var, annotation, reply}, annotation),
          annotation
        )

      selected_call =
        {:call, annotation, {:var, annotation, handler_variable},
         [
           {:atom, annotation, safe_atom(expression.operation)},
           abstract_list(values, annotation),
           continuation
         ]}

      found =
        {:tuple, annotation, [{:atom, annotation, :ok}, {:var, annotation, handler_variable}]}

      missing_reason =
        :erl_parse.abstract({:unhandled_effect, expression.effect, expression.operation})

      missing_trap =
        {:tuple, annotation, [{:atom, annotation, :catena_trap}, missing_reason]}

      missing = remote_call(:erlang, :error, [missing_trap], annotation)

      selection =
        remote_call(
          :maps,
          :find,
          [{:atom, annotation, safe_atom(expression.effect)}, handlers],
          annotation
        )

      {:case, annotation, selection,
       [
         {:clause, annotation, [found], [], [selected_call]},
         {:clause, annotation, [{:atom, annotation, :error}], [], [missing]}
       ]}
    end)
  end

  defp lower_cps(%{tag: :resume} = expression, environment, globals, module, handlers, k) do
    lower_values_cps([expression.expression], environment, globals, module, handlers, fn [value] ->
      annotation = annotation(expression.span)
      resumption = {:var, annotation, Map.fetch!(environment, expression.resumption)}
      resumed = remote_call(Catena.Effect.Runtime, :resume, [resumption, value], annotation)
      call_continuation(k, resumed, annotation)
    end)
  end

  defp lower_cps(%{tag: :spawn} = expression, environment, globals, module, handlers, k) do
    lower_values_cps(expression.arguments, environment, globals, module, handlers, fn values ->
      annotation = annotation(expression.span)
      entry = expression.selected_entry

      value =
        if entry.module == module_name(module) do
          {:call, annotation, {:atom, annotation, safe_atom(entry.spawn_symbol)}, values}
        else
          remote_call(safe_atom(entry.module), safe_atom(entry.spawn_symbol), values, annotation)
        end

      call_continuation(k, value, annotation)
    end)
  end

  defp lower_cps(%{tag: :self} = expression, _environment, _globals, _module, _handlers, k) do
    annotation = annotation(expression.span)
    call_continuation(k, remote_call(:erlang, :self, [], annotation), annotation)
  end

  defp lower_cps(%{tag: :send} = expression, environment, globals, module, handlers, k) do
    lower_values_cps(
      [expression.left, expression.right],
      environment,
      globals,
      module,
      handlers,
      fn [target, message] ->
        annotation = annotation(expression.span)
        send = {:op, annotation, :!, target, message}
        {:block, annotation, [send, call_continuation(k, {:atom, annotation, :unit}, annotation)]}
      end
    )
  end

  defp lower_cps(%{tag: :receive} = expression, environment, globals, module, handlers, k) do
    {:receive, annotation(expression.span),
     lower_cps_clauses(expression.clauses, environment, globals, module, handlers, k)}
  end

  defp lower_cps(%{tag: :trap} = expression, environment, globals, module, handlers, _k) do
    reason = cps_variable("TrapReason", expression.span, 0)
    annotation = annotation(expression.span)
    tuple = {:tuple, annotation, [{:atom, annotation, :catena_trap}, {:var, annotation, reason}]}
    trap = remote_call(:erlang, :error, [tuple], annotation)
    continuation = continuation_fun(reason, trap, annotation)
    lower_cps(expression.expression, environment, globals, module, handlers, continuation)
  end

  defp lower_general_cps_call(expression, environment, globals, module, handlers, k) do
    callee = cps_variable("Callee", expression.span, 0)
    annotation = annotation(expression.span)

    continuation =
      continuation_fun(
        callee,
        lower_values_cps(
          expression.arguments,
          environment,
          globals,
          module,
          handlers,
          fn arguments ->
            apply_cps_values({:var, annotation, callee}, arguments, handlers, k, annotation)
          end
        ),
        annotation
      )

    lower_cps(expression.callee, environment, globals, module, handlers, continuation)
  end

  defp lower_values_cps(expressions, environment, globals, module, handlers, callback),
    do: do_lower_values_cps(expressions, environment, globals, module, handlers, [], callback)

  defp do_lower_values_cps([], _environment, _globals, _module, _handlers, values, callback),
    do: callback.(Enum.reverse(values))

  defp do_lower_values_cps(
         [expression | rest],
         environment,
         globals,
         module,
         handlers,
         values,
         callback
       ) do
    variable = cps_variable("Value", expression.span, length(values))
    annotation = annotation(expression.span)

    continuation =
      continuation_fun(
        variable,
        do_lower_values_cps(
          rest,
          environment,
          globals,
          module,
          handlers,
          [{:var, annotation, variable} | values],
          callback
        ),
        annotation
      )

    lower_cps(expression, environment, globals, module, handlers, continuation)
  end

  defp apply_cps_values(function, [], _handlers, k, annotation),
    do: call_continuation(k, function, annotation)

  defp apply_cps_values(function, [argument], handlers, k, annotation),
    do: {:call, annotation, function, [argument, handlers, k]}

  defp apply_cps_values(function, [argument | rest], handlers, k, annotation) do
    next = String.to_atom("__Catena_CpsApplied_#{length(rest)}_#{annotation}")

    continuation =
      continuation_fun(
        next,
        apply_cps_values({:var, annotation, next}, rest, handlers, k, annotation),
        annotation
      )

    {:call, annotation, function, [argument, handlers, continuation]}
  end

  defp lower_cps_clauses(clauses, environment, globals, module, handlers, k) do
    Enum.flat_map(clauses, fn clause ->
      clause.pattern
      |> expand_pattern()
      |> Enum.map(fn pattern ->
        {pattern, bindings} = lower_pattern(pattern, %{})
        environment = Map.merge(environment, bindings)
        annotation = annotation(clause.span)
        guards = if is_nil(clause.guard), do: [], else: [[lower_guard(clause.guard, environment)]]
        body = lower_cps(clause.body, environment, globals, module, handlers, k)
        {:clause, annotation, [pattern], guards, [body]}
      end)
    end)
  end

  defp build_handler_fun(handler, environment, globals, module, outer_handlers) do
    annotation = annotation(handler.span)
    operation_variable = cps_variable("Operation", handler.span, 0)
    arguments_variable = cps_variable("OperationArguments", handler.span, 1)
    continuation_variable = cps_variable("OperationContinuation", handler.span, 2)

    clauses =
      handler.operations
      |> Map.values()
      |> Enum.sort_by(& &1.operation)
      |> Enum.map(fn clause ->
        clause_annotation = annotation(clause.span)

        {parameter_patterns, clause_environment} =
          Enum.map_reduce(clause.parameters, environment, fn parameter, environment ->
            variable = variable_atom(parameter.name, parameter.span)
            {{:var, clause_annotation, variable}, Map.put(environment, parameter.name, variable)}
          end)

        resumption_variable = variable_atom(clause.resumption, clause.span)
        clause_environment = Map.put(clause_environment, clause.resumption, resumption_variable)

        token =
          remote_call(
            Catena.Effect.Runtime,
            :new_resumption,
            [{:var, clause_annotation, continuation_variable}],
            clause_annotation
          )

        body =
          lower_cps(
            clause.body,
            clause_environment,
            globals,
            module,
            outer_handlers,
            identity_fun(clause_annotation)
          )

        tuple_pattern =
          {:tuple, clause_annotation,
           [
             {:atom, clause_annotation, safe_atom(clause.operation)},
             abstract_list(parameter_patterns, clause_annotation)
           ]}

        block =
          {:block, clause_annotation,
           [
             {:match, clause_annotation, {:var, clause_annotation, resumption_variable}, token},
             body
           ]}

        {:clause, clause_annotation, [tuple_pattern], [], [block]}
      end)

    selected =
      {:tuple, annotation,
       [
         {:var, annotation, operation_variable},
         {:var, annotation, arguments_variable}
       ]}

    dispatch = {:case, annotation, selected, clauses}

    clause =
      {:clause, annotation,
       [
         {:var, annotation, operation_variable},
         {:var, annotation, arguments_variable},
         {:var, annotation, continuation_variable}
       ], [], [dispatch]}

    {:fun, annotation, {:clauses, [clause]}}
  end

  defp curried_cps_global(name, global, handlers, annotation) do
    variables = Enum.map(1..global.arity, &String.to_atom("__Catena_CpsCurry#{&1}_#{annotation}"))

    continuation_variables =
      Enum.map(1..global.arity, &String.to_atom("__Catena_CpsCurryK#{&1}_#{annotation}"))

    handler_variables =
      Enum.map(1..global.arity, &String.to_atom("__Catena_CpsCurryH#{&1}_#{annotation}"))

    final_handlers = {:var, annotation, List.last(handler_variables)}
    final_k = {:var, annotation, List.last(continuation_variables)}
    arguments = Enum.map(variables, &{:var, annotation, &1})

    body =
      if global.effectful? do
        {:call, annotation, {:atom, annotation, cps_worker_atom(name)},
         arguments ++ [final_handlers, final_k]}
      else
        value = {:call, annotation, {:atom, annotation, safe_atom(name)}, arguments}
        call_continuation(final_k, value, annotation)
      end

    variables
    |> Enum.zip(Enum.zip(handler_variables, continuation_variables))
    |> Enum.reverse()
    |> Enum.reduce(body, fn {variable, {handler_variable, continuation_variable}}, inner ->
      clause =
        {:clause, annotation,
         [
           {:var, annotation, variable},
           {:var, annotation, handler_variable},
           {:var, annotation, continuation_variable}
         ], [], [inner]}

      {:fun, annotation, {:clauses, [clause]}}
    end)
    |> then(fn fun ->
      _ = handlers
      fun
    end)
  end

  defp unwrap_functions(expression, 0, parameters), do: {Enum.reverse(parameters), expression}

  defp unwrap_functions(%{tag: :function} = expression, remaining, parameters) when remaining > 0,
    do:
      unwrap_functions(
        expression.body,
        remaining - 1,
        [{expression.parameter, expression.span} | parameters]
      )

  defp unwrap_functions(expression, _remaining, parameters),
    do: {Enum.reverse(parameters), expression}

  defp call_continuation(k, value, annotation), do: {:call, annotation, k, [value]}

  defp continuation_fun(variable, body, annotation) do
    clause = {:clause, annotation, [{:var, annotation, variable}], [], [body]}
    {:fun, annotation, {:clauses, [clause]}}
  end

  defp identity_fun(annotation) do
    variable = String.to_atom("__Catena_Identity_#{annotation}")
    continuation_fun(variable, {:var, annotation, variable}, annotation)
  end

  defp abstract_list(values, annotation),
    do: Enum.reduce(Enum.reverse(values), {nil, annotation}, &{:cons, annotation, &1, &2})

  defp cps_variable(prefix, span, index),
    do: String.to_atom("__Catena_#{prefix}_#{span.byte_start}_#{index}")

  defp cps_worker_atom(name), do: safe_atom("__catena_kernel_cps_#{name}")

  defp lower_general_call(expression, environment, globals, module) do
    annotation = annotation(expression.span)
    callee = lower_expression(expression.callee, environment, globals, module)

    Enum.reduce(expression.arguments, callee, fn argument, function ->
      {:call, annotation, function, [lower_expression(argument, environment, globals, module)]}
    end)
  end

  defp lower_clauses(clauses, environment, globals, module, receive?) do
    Enum.flat_map(clauses, fn clause ->
      clause.pattern
      |> expand_pattern()
      |> Enum.map(fn pattern ->
        {pattern, bindings} = lower_pattern(pattern, %{})
        environment = Map.merge(environment, bindings)
        annotation = annotation(clause.span)

        guards =
          if is_nil(clause.guard) do
            []
          else
            [[lower_guard(clause.guard, environment)]]
          end

        body = lower_expression(clause.body, environment, globals, module)
        patterns = if receive?, do: [pattern], else: [pattern]
        {:clause, annotation, patterns, guards, [body]}
      end)
    end)
  end

  defp lower_pattern(%{tag: :wildcard} = pattern, bindings),
    do: {{:var, annotation(pattern.span), :_}, bindings}

  defp lower_pattern(%{tag: :bind} = pattern, bindings) do
    variable = variable_atom(pattern.name, pattern.span)
    {{:var, annotation(pattern.span), variable}, Map.put(bindings, pattern.name, variable)}
  end

  defp lower_pattern(%{tag: :integer} = pattern, bindings),
    do: {{:integer, annotation(pattern.span), pattern.value}, bindings}

  defp lower_pattern(%{tag: :boolean} = pattern, bindings),
    do: {{:atom, annotation(pattern.span), pattern.value}, bindings}

  defp lower_pattern(%{tag: :tuple} = pattern, bindings) do
    {elements, bindings} = Enum.map_reduce(pattern.elements, bindings, &lower_pattern/2)
    {{:tuple, annotation(pattern.span), elements}, bindings}
  end

  defp lower_pattern(%{tag: :variant} = pattern, bindings) do
    {payload, bindings} = lower_pattern(pattern.pattern, bindings)
    annotation = annotation(pattern.span)

    {{:tuple, annotation,
      [
        {:atom, annotation, :catena_variant},
        {:atom, annotation, safe_atom(pattern.label)},
        payload
      ]}, bindings}
  end

  defp lower_pattern(%{tag: :constructor} = pattern, bindings) do
    {fields, bindings} = Enum.map_reduce(pattern.patterns, bindings, &lower_pattern/2)
    annotation = annotation(pattern.span)

    {{:tuple, annotation,
      [
        {:atom, annotation, :catena_constructor},
        {:atom, annotation, safe_atom(pattern.constructor)},
        {:tuple, annotation, fields}
      ]}, bindings}
  end

  defp lower_pattern(%{tag: :as} = pattern, bindings) do
    {inner, bindings} = lower_pattern(pattern.pattern, bindings)
    variable = variable_atom(pattern.name, pattern.span)
    match = {:match, annotation(pattern.span), {:var, annotation(pattern.span), variable}, inner}
    {match, Map.put(bindings, pattern.name, variable)}
  end

  defp lower_guard(%{tag: :integer} = expression, _environment),
    do: {:integer, annotation(expression.span), expression.value}

  defp lower_guard(%{tag: :boolean} = expression, _environment),
    do: {:atom, annotation(expression.span), expression.value}

  defp lower_guard(%{tag: :variable} = expression, environment),
    do: {:var, annotation(expression.span), Map.fetch!(environment, expression.name)}

  defp lower_guard(%{tag: :unary} = expression, environment),
    do:
      {:op, annotation(expression.span), erlang_operator(expression.operator),
       lower_guard(expression.operand, environment)}

  defp lower_guard(%{tag: :binary} = expression, environment),
    do:
      {:op, annotation(expression.span), erlang_operator(expression.operator),
       lower_guard(expression.left, environment), lower_guard(expression.right, environment)}

  defp lower_guard(%{tag: :tuple} = expression, environment),
    do:
      {:tuple, annotation(expression.span),
       Enum.map(expression.elements, &lower_guard(&1, environment))}

  defp lower_guard(%{tag: :annotate} = expression, environment),
    do: lower_guard(expression.expression, environment)

  defp expand_pattern(%{tag: :or, alternatives: alternatives}),
    do: Enum.flat_map(alternatives, &expand_pattern/1)

  defp expand_pattern(%{tag: :tuple, elements: elements} = pattern) do
    elements
    |> Enum.map(&expand_pattern/1)
    |> cartesian()
    |> Enum.map(&%{pattern | elements: &1})
  end

  defp expand_pattern(%{tag: :variant, pattern: inner} = pattern),
    do: Enum.map(expand_pattern(inner), &%{pattern | pattern: &1})

  defp expand_pattern(%{tag: :constructor, patterns: patterns} = pattern) do
    patterns
    |> Enum.map(&expand_pattern/1)
    |> cartesian()
    |> Enum.map(&%{pattern | patterns: &1})
  end

  defp expand_pattern(%{tag: :as, pattern: inner} = pattern),
    do: Enum.map(expand_pattern(inner), &%{pattern | pattern: &1})

  defp expand_pattern(pattern), do: [pattern]

  defp cartesian([]), do: [[]]

  defp cartesian([head | tail]) do
    for value <- head, rest <- cartesian(tail), do: [value | rest]
  end

  defp curried_global(name, 0, annotation),
    do: {:fun, annotation, {:function, safe_atom(name), 0}}

  defp curried_global(name, arity, annotation) do
    variables = Enum.map(1..arity, &String.to_atom("__Catena_Curry#{&1}"))

    body =
      {:call, annotation, {:atom, annotation, safe_atom(name)},
       Enum.map(variables, &{:var, annotation, &1})}

    Enum.reduce(Enum.reverse(variables), body, fn variable, inner ->
      clause = {:clause, annotation, [{:var, annotation, variable}], [], [inner]}
      {:fun, annotation, {:clauses, [clause]}}
    end)
  end

  defp remote_call(module, function, arguments, annotation) do
    {:call, annotation,
     {:remote, annotation, {:atom, annotation, module}, {:atom, annotation, function}}, arguments}
  end

  defp constructor_value(name, fields, annotation) do
    {:tuple, annotation,
     [
       {:atom, annotation, :catena_constructor},
       {:atom, annotation, safe_atom(name)},
       {:tuple, annotation, fields}
     ]}
  end

  defp erlang_operator(:not), do: :not
  defp erlang_operator(:negate), do: :-
  defp erlang_operator(:and), do: :andalso
  defp erlang_operator(:or), do: :orelse
  defp erlang_operator(:equal), do: :"=:="
  defp erlang_operator(:not_equal), do: :"=/="
  defp erlang_operator(:less), do: :<
  defp erlang_operator(:less_equal), do: :"=<"
  defp erlang_operator(:greater), do: :>
  defp erlang_operator(:greater_equal), do: :>=
  defp erlang_operator(:add), do: :+
  defp erlang_operator(:subtract), do: :-
  defp erlang_operator(:multiply), do: :*

  defp annotation(span), do: span.line_start
  defp variable_atom(name, span), do: String.to_atom("__Catena_#{name}_#{span.byte_start}")
  defp safe_atom(name), do: String.to_atom(name)
  defp module_name(nil), do: nil
  defp module_name(module) when is_atom(module), do: Atom.to_string(module)

  defp effect_control?(%{tag: tag}) when tag in [:request, :handle, :resume], do: true
  defp effect_control?(%Catena.SourceSpan{}), do: false

  defp effect_control?(%{} = value),
    do: value |> Map.values() |> Enum.any?(&effect_control?/1)

  defp effect_control?(values) when is_list(values), do: Enum.any?(values, &effect_control?/1)
  defp effect_control?(_value), do: false
end
