defmodule Catena.Kernel.Stepper do
  @moduledoc "Small-step CEK and actor-configuration reference semantics for kernel 0.1.8."

  alias Catena.ImplementationLimits

  @default_budget ImplementationLimits.configured(:kernel_reference_steps)

  @type configuration :: map()

  @spec initial(map(), String.t(), [term()]) :: {:ok, configuration()} | {:error, term()}
  def initial(core, name, arguments \\ []) do
    definitions = Map.new(core.definitions, &{&1.name, &1})

    case Map.fetch(definitions, name) do
      {:ok, definition} when length(arguments) == definition.arity ->
        process = %{
          id: 0,
          name: "$root",
          status: :running,
          control: {:expr, definition.expression, %{}},
          stack: Enum.map(arguments, &{:apply_value, &1}),
          mailbox: [],
          mailbox_type: nil,
          result: nil,
          trap: nil
        }

        {:ok,
         %{
           format: :kernel_configuration,
           core: core,
           definitions: definitions,
           processes: %{0 => process},
           next: 1,
           root: 0,
           trace: [],
           resumptions: MapSet.new(),
           next_resumption: 0,
           steps: 0
         }}

      {:ok, definition} ->
        {:error, {:wrong_arity, name, definition.arity, length(arguments)}}

      :error ->
        {:error, {:unknown_definition, name}}
    end
  end

  @spec runnable_pids(configuration()) :: [non_neg_integer()]
  def runnable_pids(configuration) do
    configuration.processes
    |> Enum.filter(fn {_pid, process} -> runnable?(process, configuration) end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sort()
  end

  @spec step(configuration(), non_neg_integer()) :: {:ok, configuration()} | {:error, term()}
  def step(configuration, pid) do
    with {:ok, process} <- Map.fetch(configuration.processes, pid),
         true <- runnable?(process, configuration) do
      configuration =
        case process.status do
          :waiting -> resume_receive(configuration, process)
          :running -> local_step(configuration, process)
        end

      {:ok, %{configuration | steps: configuration.steps + 1}}
    else
      :error -> {:error, {:unknown_process, pid}}
      false -> {:error, {:process_not_runnable, pid}}
    end
  end

  @spec run(map(), String.t(), [term()], keyword()) ::
          {:ok, term(), map()}
          | {:trap, term(), map()}
          | {:quiescent, map()}
          | {:budget_exhausted, map()}
          | {:error, term()}
  def run(core, name, arguments \\ [], options \\ []) do
    with {:ok, configuration} <- initial(core, name, arguments) do
      run_configuration(configuration, options)
    end
  end

  @spec run_configuration(configuration(), keyword()) ::
          {:ok, term(), map()}
          | {:trap, term(), map()}
          | {:quiescent, map()}
          | {:budget_exhausted, map()}
          | {:error, term()}
  def run_configuration(configuration, options \\ []) do
    budget = Keyword.get(options, :budget, @default_budget)
    schedule = Keyword.get(options, :schedule)
    do_run(configuration, budget, schedule)
  end

  @spec outcome(configuration()) :: map()
  def outcome(configuration) do
    root = Map.fetch!(configuration.processes, configuration.root)

    %{
      root_status: root.status,
      root_result: root.result,
      root_trap: root.trap,
      processes:
        configuration.processes
        |> Enum.sort_by(&elem(&1, 0))
        |> Enum.map(fn {pid, process} ->
          %{
            pid: pid,
            name: process.name,
            status: process.status,
            mailbox:
              Enum.map(process.mailbox, fn {sender, value} -> %{sender: sender, value: value} end),
            result: process.result,
            trap: process.trap
          }
        end),
      trace: configuration.trace,
      steps: configuration.steps
    }
  end

  defp do_run(configuration, budget, _schedule) when configuration.steps >= budget,
    do: {:budget_exhausted, outcome(configuration)}

  defp do_run(configuration, budget, schedule) do
    case runnable_pids(configuration) do
      [] ->
        terminal_outcome(configuration)

      runnable ->
        with {:ok, pid, remaining_schedule} <- choose_pid(runnable, schedule),
             {:ok, next} <- step(configuration, pid) do
          do_run(next, budget, remaining_schedule)
        end
    end
  end

  defp choose_pid([pid | _], nil), do: {:ok, pid, nil}
  defp choose_pid([pid | _], []), do: {:ok, pid, []}

  defp choose_pid(runnable, [pid | rest]) do
    if pid in runnable,
      do: {:ok, pid, rest},
      else: {:error, {:scheduled_process_not_runnable, pid, runnable}}
  end

  defp terminal_outcome(configuration) do
    root = Map.fetch!(configuration.processes, configuration.root)
    result = outcome(configuration)

    cond do
      Enum.any?(configuration.processes, fn {_pid, process} -> process.status == :waiting end) ->
        {:quiescent, result}

      root.status == :trapped ->
        {:trap, root.trap, result}

      root.status == :terminated ->
        {:ok, root.result, result}

      true ->
        {:error, :invalid_terminal_configuration}
    end
  end

  defp runnable?(%{status: :running}, _configuration), do: true

  defp runnable?(%{status: :waiting} = process, configuration),
    do: not is_nil(find_receive(process, configuration))

  defp runnable?(_process, _configuration), do: false

  defp local_step(configuration, %{control: {:expr, expression, environment}} = process) do
    case expression.tag do
      :integer ->
        put_control(configuration, process, {:value, expression.value})

      :boolean ->
        put_control(configuration, process, {:value, expression.value})

      :unit ->
        put_control(configuration, process, {:value, :unit})

      :variable ->
        evaluate_variable(configuration, process, expression, environment)

      :function ->
        closure = {:closure, expression.parameter, expression.body, environment}
        put_control(configuration, process, {:value, closure})

      :call ->
        push_expression(configuration, process, expression.callee, environment, [
          {:call_arguments, expression.arguments, environment}
        ])

      :let ->
        push_expression(configuration, process, expression.value, environment, [
          {:let, expression.name, expression.body, environment}
        ])

      :sequence ->
        push_expression(configuration, process, expression.first, environment, [
          {:sequence, expression.second, environment}
        ])

      :tuple ->
        begin_sequence(configuration, process, expression.elements, environment, :tuple)

      :annotate ->
        put_control(configuration, process, {:expr, expression.expression, environment})

      :unary ->
        push_expression(configuration, process, expression.operand, environment, [
          {:unary, expression.operator}
        ])

      :binary ->
        push_expression(configuration, process, expression.left, environment, [
          {:binary_left, expression.operator, expression.right, environment}
        ])

      :record ->
        begin_record(configuration, process, expression.fields, environment)

      :select ->
        push_expression(configuration, process, expression.record, environment, [
          {:select, String.to_atom(expression.label)}
        ])

      :update ->
        push_expression(configuration, process, expression.record, environment, [
          {:record_value, :update, String.to_atom(expression.label), expression.value,
           environment}
        ])

      :extend ->
        push_expression(configuration, process, expression.record, environment, [
          {:record_value, :extend, String.to_atom(expression.label), expression.value,
           environment}
        ])

      :restrict ->
        push_expression(configuration, process, expression.record, environment, [
          {:restrict, String.to_atom(expression.label)}
        ])

      :inject ->
        push_expression(configuration, process, expression.payload, environment, [
          {:inject, String.to_atom(expression.label)}
        ])

      :construct ->
        begin_construct(configuration, process, expression, environment)

      :match ->
        push_expression(configuration, process, expression.scrutinee, environment, [
          {:match, expression.clauses, environment}
        ])

      :trait_call ->
        call = %{
          tag: :call,
          callee: %{tag: :variable, name: expression.selected_definition},
          arguments: expression.arguments
        }

        put_control(configuration, process, {:expr, call, environment})

      :handle ->
        configuration =
          append_trace(configuration, %{
            label: :handle,
            pid: process.id,
            handler: expression.selected_handler.name,
            effect: expression.selected_handler.effect
          })

        push_expression(configuration, process, expression.expression, environment, [
          {:handler, expression.selected_handler, environment}
        ])

      :request ->
        begin_request(configuration, process, expression, environment)

      :resume ->
        push_expression(configuration, process, expression.expression, environment, [
          {:resume, expression.resumption, environment}
        ])

      :spawn ->
        begin_spawn(configuration, process, expression, environment)

      :self ->
        put_control(configuration, process, {:value, {:catena_process, process.id}})

      :send ->
        push_expression(configuration, process, expression.left, environment, [
          {:send_target, expression.right, environment}
        ])

      :receive ->
        attempt_receive(configuration, process, expression.clauses, environment)

      :trap ->
        push_expression(configuration, process, expression.expression, environment, [:trap])
    end
  end

  defp local_step(configuration, %{control: {:value, value}} = process),
    do: continue_value(configuration, process, value)

  defp evaluate_variable(configuration, process, expression, environment) do
    case Map.fetch(environment, expression.name) do
      {:ok, value} ->
        put_control(configuration, process, {:value, value})

      :error ->
        case Map.fetch(configuration.definitions, expression.name) do
          {:ok, definition} ->
            put_control(configuration, process, {:expr, definition.expression, %{}})

          :error ->
            trap_process(configuration, process, {:unbound_core_variable, expression.name})
        end
    end
  end

  defp continue_value(configuration, %{stack: []} = process, value) do
    terminate_process(configuration, process, value)
  end

  defp continue_value(configuration, %{stack: [frame | rest]} = process, value) do
    process = %{process | stack: rest}

    case frame do
      {:apply_value, argument} ->
        apply_closure(configuration, process, value, argument)

      {:call_arguments, [], _environment} ->
        put_control(configuration, process, {:value, value})

      {:call_arguments, [argument | remaining], environment} ->
        process = push_frames(process, [{:call_apply, value, remaining, environment}])
        put_control(configuration, process, {:expr, argument, environment})

      {:call_apply, function, remaining, environment} ->
        process = push_frames(process, [{:call_arguments, remaining, environment}])
        apply_closure(configuration, process, function, value)

      {:let, name, body, environment} ->
        put_control(configuration, process, {:expr, body, Map.put(environment, name, value)})

      {:sequence, second, environment} ->
        put_control(configuration, process, {:expr, second, environment})

      {:tuple, completed, [], _environment} ->
        put_control(
          configuration,
          process,
          {:value, List.to_tuple(Enum.reverse([value | completed]))}
        )

      {:tuple, completed, [next | remaining], environment} ->
        process = push_frames(process, [{:tuple, [value | completed], remaining, environment}])
        put_control(configuration, process, {:expr, next, environment})

      {:unary, operator} ->
        put_control(configuration, process, {:value, apply_unary(operator, value)})

      {:binary_left, :and, _right, _environment} when value == false ->
        put_control(configuration, process, {:value, false})

      {:binary_left, :or, _right, _environment} when value == true ->
        put_control(configuration, process, {:value, true})

      {:binary_left, operator, right, environment} ->
        process = push_frames(process, [{:binary_right, operator, value}])
        put_control(configuration, process, {:expr, right, environment})

      {:binary_right, operator, left} ->
        put_control(configuration, process, {:value, apply_binary(operator, left, value)})

      {:record, label, completed, [], _environment} ->
        put_control(
          configuration,
          process,
          {:value, Map.new(Enum.reverse([{label, value} | completed]))}
        )

      {:record, label, completed, [next | remaining], environment} ->
        process =
          push_frames(process, [
            {:record, String.to_atom(next.label), [{label, value} | completed], remaining,
             environment}
          ])

        put_control(configuration, process, {:expr, next.expression, environment})

      {:select, label} ->
        put_control(configuration, process, {:value, Map.fetch!(value, label)})

      {:record_value, operation, label, expression, environment} ->
        process = push_frames(process, [{:record_finish, operation, label, value}])
        put_control(configuration, process, {:expr, expression, environment})

      {:record_finish, _operation, label, record} ->
        put_control(configuration, process, {:value, Map.put(record, label, value)})

      {:restrict, label} ->
        put_control(configuration, process, {:value, Map.delete(value, label)})

      {:inject, label} ->
        put_control(configuration, process, {:value, {:catena_variant, label, value}})

      {:construct, constructor, completed, [], _environment} ->
        fields = completed |> then(&[value | &1]) |> Enum.reverse() |> List.to_tuple()
        put_control(configuration, process, {:value, {:catena_constructor, constructor, fields}})

      {:construct, constructor, completed, [next | remaining], environment} ->
        process =
          push_frames(process, [
            {:construct, constructor, [value | completed], remaining, environment}
          ])

        put_control(configuration, process, {:expr, next, environment})

      {:match, clauses, environment} ->
        select_match(configuration, process, value, clauses, environment)

      {:handler, handler, environment} ->
        return = handler.return

        configuration =
          append_trace(configuration, %{
            label: :effect_return,
            pid: process.id,
            handler: handler.name
          })

        put_control(
          configuration,
          process,
          {:expr, return.body, Map.put(environment, return.parameter, value)}
        )

      {:request, effect, operation, completed, [], _environment} ->
        perform_request(
          configuration,
          process,
          effect,
          operation,
          Enum.reverse([value | completed])
        )

      {:request, effect, operation, completed, [next | remaining], environment} ->
        process =
          push_frames(process, [
            {:request, effect, operation, [value | completed], remaining, environment}
          ])

        put_control(configuration, process, {:expr, next, environment})

      {:resume, name, environment} ->
        perform_resume(configuration, process, Map.fetch!(environment, name), value)

      {:resume_return, saved_stack} ->
        process = %{process | stack: saved_stack}
        put_control(configuration, process, {:value, value})

      {:spawn, entry, completed, [], _environment} ->
        perform_spawn(configuration, process, entry, Enum.reverse([value | completed]))

      {:spawn, entry, completed, [next | remaining], environment} ->
        process =
          push_frames(process, [{:spawn, entry, [value | completed], remaining, environment}])

        put_control(configuration, process, {:expr, next, environment})

      {:send_target, message, environment} ->
        process = push_frames(process, [{:send_message, value}])
        put_control(configuration, process, {:expr, message, environment})

      {:send_message, target} ->
        perform_send(configuration, process, target, value)

      :trap ->
        trap_process(configuration, process, value)
    end
  end

  defp begin_sequence(configuration, process, [], _environment, :tuple),
    do: put_control(configuration, process, {:value, {}})

  defp begin_sequence(configuration, process, [first | rest], environment, :tuple) do
    process = push_frames(process, [{:tuple, [], rest, environment}])
    put_control(configuration, process, {:expr, first, environment})
  end

  defp begin_record(configuration, process, [], _environment),
    do: put_control(configuration, process, {:value, %{}})

  defp begin_record(configuration, process, [first | rest], environment) do
    process =
      push_frames(process, [
        {:record, String.to_atom(first.label), [], rest, environment}
      ])

    put_control(configuration, process, {:expr, first.expression, environment})
  end

  defp begin_construct(configuration, process, expression, environment) do
    constructor = String.to_atom(expression.selected_constructor.name)

    case expression.arguments do
      [] ->
        put_control(
          configuration,
          process,
          {:value, {:catena_constructor, constructor, {}}}
        )

      [first | rest] ->
        process = push_frames(process, [{:construct, constructor, [], rest, environment}])
        put_control(configuration, process, {:expr, first, environment})
    end
  end

  defp begin_spawn(configuration, process, expression, environment) do
    case expression.arguments do
      [] ->
        perform_spawn(configuration, process, expression.selected_entry, [])

      [first | rest] ->
        process =
          push_frames(process, [{:spawn, expression.selected_entry, [], rest, environment}])

        put_control(configuration, process, {:expr, first, environment})
    end
  end

  defp begin_request(configuration, process, expression, environment) do
    case expression.arguments do
      [] ->
        perform_request(configuration, process, expression.effect, expression.operation, [])

      [first | rest] ->
        process =
          push_frames(process, [
            {:request, expression.effect, expression.operation, [], rest, environment}
          ])

        put_control(configuration, process, {:expr, first, environment})
    end
  end

  defp apply_closure(configuration, process, {:closure, parameter, body, environment}, argument) do
    put_control(configuration, process, {:expr, body, Map.put(environment, parameter, argument)})
  end

  defp apply_closure(configuration, process, value, _argument),
    do: trap_process(configuration, process, {:called_non_function, value})

  defp select_match(configuration, process, value, clauses, environment) do
    case select_clause(value, clauses, environment, configuration) do
      {:ok, clause, bindings} ->
        put_control(
          configuration,
          process,
          {:expr, clause.body, Map.merge(environment, bindings)}
        )

      :no_match ->
        trap_process(configuration, process, :verified_match_reached_no_clause)
    end
  end

  defp attempt_receive(configuration, process, clauses, environment) do
    process =
      Map.merge(process, %{receive_clauses: clauses, receive_environment: environment})

    case find_receive(process, configuration) do
      nil -> put_process(configuration, %{process | status: :waiting})
      match -> accept_receive(configuration, process, match)
    end
  end

  defp resume_receive(configuration, process) do
    case find_receive(process, configuration) do
      nil -> configuration
      match -> accept_receive(configuration, process, match)
    end
  end

  defp find_receive(process, configuration) do
    process.mailbox
    |> Enum.with_index()
    |> Enum.find_value(fn {{sender, message}, index} ->
      case select_clause(
             message,
             process.receive_clauses,
             process.receive_environment,
             configuration
           ) do
        {:ok, clause, bindings} ->
          %{index: index, sender: sender, message: message, clause: clause, bindings: bindings}

        :no_match ->
          nil
      end
    end)
  end

  defp accept_receive(configuration, process, match) do
    mailbox = List.delete_at(process.mailbox, match.index)
    environment = Map.merge(process.receive_environment, match.bindings)

    process =
      process
      |> Map.drop([:receive_clauses, :receive_environment])
      |> Map.merge(%{
        status: :running,
        mailbox: mailbox,
        control: {:expr, match.clause.body, environment}
      })

    configuration
    |> put_process(process)
    |> append_trace(%{
      label: :receive,
      pid: process.id,
      sender: match.sender,
      message: match.message
    })
  end

  defp select_clause(value, clauses, environment, configuration) do
    Enum.find_value(clauses, :no_match, fn clause ->
      case match_pattern(clause.pattern, value, %{}) do
        {:ok, bindings} ->
          branch = Map.merge(environment, bindings)

          if is_nil(clause.guard) or evaluate_guard(clause.guard, branch, configuration) == true,
            do: {:ok, clause, bindings},
            else: false

        :no_match ->
          false
      end
    end)
  end

  defp match_pattern(%{tag: :wildcard}, _value, bindings), do: {:ok, bindings}

  defp match_pattern(%{tag: :bind, name: name}, value, bindings),
    do: {:ok, Map.put(bindings, name, value)}

  defp match_pattern(%{tag: :integer, value: value}, value, bindings), do: {:ok, bindings}
  defp match_pattern(%{tag: :boolean, value: value}, value, bindings), do: {:ok, bindings}

  defp match_pattern(%{tag: tag}, _value, _bindings) when tag in [:integer, :boolean],
    do: :no_match

  defp match_pattern(%{tag: :tuple, elements: patterns}, value, bindings)
       when is_tuple(value) and tuple_size(value) == length(patterns),
       do: match_patterns(patterns, Tuple.to_list(value), bindings)

  defp match_pattern(
         %{tag: :variant, label: label, pattern: pattern},
         {:catena_variant, label_atom, value},
         bindings
       )
       when is_atom(label_atom) do
    if Atom.to_string(label_atom) == label,
      do: match_pattern(pattern, value, bindings),
      else: :no_match
  end

  defp match_pattern(
         %{tag: :constructor, constructor: constructor, patterns: patterns},
         {:catena_constructor, constructor_atom, fields},
         bindings
       )
       when is_tuple(fields) do
    if Atom.to_string(constructor_atom) == constructor and tuple_size(fields) == length(patterns),
      do: match_patterns(patterns, Tuple.to_list(fields), bindings),
      else: :no_match
  end

  defp match_pattern(%{tag: :as, pattern: pattern, name: name}, value, bindings) do
    with {:ok, bindings} <- match_pattern(pattern, value, bindings),
         do: {:ok, Map.put(bindings, name, value)}
  end

  defp match_pattern(%{tag: :or, alternatives: alternatives}, value, bindings) do
    Enum.find_value(alternatives, :no_match, fn alternative ->
      case match_pattern(alternative, value, bindings) do
        {:ok, _bindings} = result -> result
        :no_match -> false
      end
    end)
  end

  defp match_pattern(_pattern, _value, _bindings), do: :no_match

  defp match_patterns(patterns, values, bindings) do
    Enum.zip(patterns, values)
    |> Enum.reduce_while({:ok, bindings}, fn {pattern, value}, {:ok, bindings} ->
      case match_pattern(pattern, value, bindings) do
        {:ok, bindings} -> {:cont, {:ok, bindings}}
        :no_match -> {:halt, :no_match}
      end
    end)
  end

  defp evaluate_guard(expression, environment, configuration) do
    case expression.tag do
      :integer ->
        expression.value

      :boolean ->
        expression.value

      :unit ->
        :unit

      :variable ->
        Map.fetch!(environment, expression.name)

      :unary ->
        apply_unary(
          expression.operator,
          evaluate_guard(expression.operand, environment, configuration)
        )

      :binary ->
        evaluate_guard_binary(expression, environment, configuration)

      :tuple ->
        expression.elements
        |> Enum.map(&evaluate_guard(&1, environment, configuration))
        |> List.to_tuple()

      :annotate ->
        evaluate_guard(expression.expression, environment, configuration)
    end
  end

  defp evaluate_guard_binary(%{operator: :and} = expression, environment, configuration),
    do:
      evaluate_guard(expression.left, environment, configuration) and
        evaluate_guard(expression.right, environment, configuration)

  defp evaluate_guard_binary(%{operator: :or} = expression, environment, configuration),
    do:
      evaluate_guard(expression.left, environment, configuration) or
        evaluate_guard(expression.right, environment, configuration)

  defp evaluate_guard_binary(expression, environment, configuration) do
    left = evaluate_guard(expression.left, environment, configuration)
    right = evaluate_guard(expression.right, environment, configuration)
    apply_binary(expression.operator, left, right)
  end

  defp perform_spawn(configuration, parent, entry, arguments) do
    case Enum.find(configuration.core.processes, &(&1.name == entry.name and not entry.imported?)) do
      nil ->
        trap_process(configuration, parent, {:unavailable_imported_process, entry.identity})

      process_entry ->
        pid = configuration.next

        environment =
          process_entry.parameters
          |> Enum.map(& &1.name)
          |> Enum.zip(arguments)
          |> Map.new()

        child = %{
          id: pid,
          name: process_entry.name,
          status: :running,
          control: {:expr, process_entry.body, environment},
          stack: [],
          mailbox: [],
          mailbox_type: process_entry.mailbox,
          result: nil,
          trap: nil
        }

        configuration
        |> Map.put(:next, pid + 1)
        |> Map.update!(:processes, &Map.put(&1, pid, child))
        |> put_process(%{parent | control: {:value, {:catena_process, pid}}})
        |> append_trace(%{label: :spawn, pid: parent.id, child: pid, entry: entry.identity})
    end
  end

  defp perform_request(configuration, process, effect, operation, arguments) do
    case split_handler(process.stack, effect, []) do
      {:ok, captured, {:handler, handler, handler_environment} = marker, outer_stack} ->
        clause = Map.fetch!(handler.operations, operation)
        id = configuration.next_resumption

        resumption = %{
          kind: :kernel_resumption,
          id: id,
          process: process.id,
          captured: captured,
          handler: marker
        }

        environment =
          clause.parameters
          |> Enum.map(& &1.name)
          |> Enum.zip(arguments)
          |> Enum.reduce(handler_environment, fn {name, value}, environment ->
            Map.put(environment, name, value)
          end)
          |> Map.put(clause.resumption, resumption)

        process = %{process | stack: outer_stack, control: {:expr, clause.body, environment}}

        configuration
        |> Map.put(:next_resumption, id + 1)
        |> put_process(process)
        |> append_trace(%{
          label: :request,
          pid: process.id,
          effect: effect,
          operation: operation,
          arguments: arguments
        })

      :error ->
        trap_process(configuration, process, {:unhandled_effect, effect, operation})
    end
  end

  defp perform_resume(configuration, process, %{kind: :kernel_resumption} = resumption, value) do
    cond do
      resumption.process != process.id ->
        trap_process(configuration, process, :resumption_process_mismatch)

      MapSet.member?(configuration.resumptions, resumption.id) ->
        trap_process(configuration, process, :consumed_resumption)

      true ->
        saved_stack = process.stack

        process = %{
          process
          | control: {:value, value},
            stack: resumption.captured ++ [resumption.handler, {:resume_return, saved_stack}]
        }

        configuration
        |> Map.update!(:resumptions, &MapSet.put(&1, resumption.id))
        |> put_process(process)
        |> append_trace(%{label: :resume, pid: process.id, resumption: resumption.id})
    end
  end

  defp perform_resume(configuration, process, _resumption, _value),
    do: trap_process(configuration, process, :invalid_resumption)

  defp split_handler([], _effect, _captured), do: :error

  defp split_handler(
         [{:handler, %{effect: effect}, _environment} = marker | rest],
         effect,
         captured
       ),
       do: {:ok, Enum.reverse(captured), marker, rest}

  defp split_handler([frame | rest], effect, captured),
    do: split_handler(rest, effect, [frame | captured])

  defp perform_send(configuration, sender, {:catena_process, target_pid}, message) do
    configuration =
      case Map.fetch(configuration.processes, target_pid) do
        {:ok, %{status: status} = target} when status in [:running, :waiting] ->
          if target_pid == sender.id do
            updated = %{
              sender
              | mailbox: sender.mailbox ++ [{sender.id, message}],
                control: {:value, :unit}
            }

            put_process(configuration, updated)
          else
            configuration
            |> put_process(%{target | mailbox: target.mailbox ++ [{sender.id, message}]})
            |> put_process(%{sender | control: {:value, :unit}})
          end

        _ ->
          put_process(configuration, %{sender | control: {:value, :unit}})
      end

    configuration
    |> append_trace(%{label: :send, pid: sender.id, target: target_pid, message: message})
  end

  defp perform_send(configuration, process, target, _message),
    do: trap_process(configuration, process, {:invalid_process_handle, target})

  defp terminate_process(configuration, process, value) do
    process = %{
      process
      | status: :terminated,
        result: value,
        control: nil,
        stack: [],
        mailbox: []
    }

    configuration
    |> put_process(process)
    |> append_trace(%{label: :return, pid: process.id, value: value})
  end

  defp trap_process(configuration, process, reason) do
    process = %{process | status: :trapped, trap: reason, control: nil, stack: [], mailbox: []}

    configuration
    |> put_process(process)
    |> append_trace(%{label: :trap, pid: process.id, reason: reason})
  end

  defp apply_unary(:not, value), do: not value
  defp apply_unary(:negate, value), do: -value

  defp apply_binary(:and, left, right), do: left and right
  defp apply_binary(:or, left, right), do: left or right
  defp apply_binary(:equal, left, right), do: left === right
  defp apply_binary(:not_equal, left, right), do: left !== right
  defp apply_binary(:less, left, right), do: left < right
  defp apply_binary(:less_equal, left, right), do: left <= right
  defp apply_binary(:greater, left, right), do: left > right
  defp apply_binary(:greater_equal, left, right), do: left >= right
  defp apply_binary(:add, left, right), do: left + right
  defp apply_binary(:subtract, left, right), do: left - right
  defp apply_binary(:multiply, left, right), do: left * right

  defp push_expression(configuration, process, expression, environment, frames) do
    process = push_frames(process, frames)
    put_control(configuration, process, {:expr, expression, environment})
  end

  defp push_frames(process, frames), do: %{process | stack: frames ++ process.stack}

  defp put_control(configuration, process, control),
    do: put_process(configuration, %{process | control: control})

  defp put_process(configuration, process),
    do: %{configuration | processes: Map.put(configuration.processes, process.id, process)}

  defp append_trace(configuration, event),
    do: %{configuration | trace: configuration.trace ++ [event]}
end
