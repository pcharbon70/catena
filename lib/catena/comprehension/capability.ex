defmodule Catena.Comprehension.Capability do
  @moduledoc "Closed-capability comprehension checking at exact 0.1.50; public grammar remains unadopted."
  alias Catena.{Comprehension, Diagnostic}
  alias Catena.Kernel.{CapabilityKernel, Checker, Parser}

  def check(%Comprehension{} = spec, bindings, options \\ []) do
    try do
      contexts = Enum.map(spec.context, &context/1)

      qualifiers =
        Enum.map(spec.qualifiers, fn {kind, fields} -> {kind, Keyword.delete(fields, :uses)} end)

      pure = %{
        spec
        | uses: [],
          qualifiers: qualifiers,
          context: Enum.map(contexts, fn {n, t, e, _} -> {n, t, e} end)
      }

      with {:ok, _selection} <- CapabilityKernel.selection(options),
           {:ok, source, advisories} <- Comprehension.elaborate(pure),
           {:ok, parsed} <- Parser.parse(source),
           :ok <- probes(spec, contexts, parsed, bindings),
           :ok <- aggregate(spec),
           {:ok, parsed} <- enclose(source, parsed, Keyword.get(options, :handlers, [])),
           {:ok, annotated} <-
             annotate(parsed, spec, contexts, Keyword.get(options, :handlers, [])),
           {:ok, module} <- CapabilityKernel.prepare(annotated, bindings),
           {:ok, core} <- Checker.check(module) do
        {:ok, core, advisories}
      end
    rescue
      _ -> error("malformed capability comprehension metadata", "$.comprehension")
    end
  end

  defp context({name, type, expression}), do: {name, type, expression, []}
  defp context({name, type, expression, uses}), do: {name, type, expression, row(uses)}

  defp row(entries) when is_list(entries) do
    if Enum.all?(entries, &is_binary/1),
      do: Enum.sort(Enum.uniq(entries)),
      else: raise(ArgumentError)
  end

  defp effects(entries),
    do:
      Enum.map(row(entries), fn
        "Process" -> :process
        name -> {:effect, name}
      end)

  defp aggregate(spec) do
    actual =
      row(
        spec.yield_uses ++
          Enum.flat_map(spec.qualifiers, fn {_, fields} -> Keyword.get(fields, :uses, []) end)
      )

    if actual == row(spec.uses),
      do: :ok,
      else: error("aggregate row differs from fragment union", "$.uses")
  end

  defp probes(spec, contexts, parsed, bindings) do
    context_names = Enum.map(contexts, &elem(&1, 0))
    prefix = Macro.underscore(spec.module)

    helpers =
      Enum.reject(parsed.definitions, fn definition ->
        definition.name in ["main", prefix <> "_reverse" | context_names] or
          String.starts_with?(definition.name, prefix <> "_go")
      end)

    result =
      Enum.reduce_while(contexts, {:ok, helpers}, fn {name, type, expression, uses},
                                                     {:ok, prior} ->
        case probe(parsed, prior, expression, type, [], uses, bindings, "$.context.#{name}") do
          :ok ->
            definition = Enum.find(parsed.definitions, &(&1.name == name))
            {:cont, {:ok, prior ++ [%{definition | uses: effects(uses)}]}}

          failure ->
            {:halt, failure}
        end
      end)

    with {:ok, globals} <- result do
      result =
        spec.qualifiers
        |> Enum.with_index()
        |> Enum.reduce_while({:ok, []}, fn {{kind, fields}, i}, {:ok, scope} ->
          {expression, type} =
            case kind do
              k when k in [:generator, :case_generator] ->
                {fields[:source], "(List #{fields[:element_type]})"}

              :filter ->
                {fields[:expr], "Bool"}

              :let ->
                {fields[:expr], fields[:value_type]}
            end

          case probe(
                 parsed,
                 globals,
                 expression,
                 type,
                 scope,
                 Keyword.get(fields, :uses, []),
                 bindings,
                 "$.qualifiers[#{i}]"
               ) do
            :ok -> {:cont, {:ok, scope ++ Keyword.get(fields, :binds, [])}}
            failure -> {:halt, failure}
          end
        end)

      with {:ok, scope} <- result,
           do:
             probe(
               parsed,
               globals,
               spec.yield,
               spec.result_element_type,
               scope,
               spec.yield_uses,
               bindings,
               "$.yield"
             )
    end
  end

  defp probe(parsed, globals, expression, result, scope, uses, bindings, path) do
    types = Enum.map(scope, &elem(&1, 1))
    signature = function_type(types, result, uses)

    body =
      Enum.reduce(Enum.reverse(scope), expression, fn {name, type}, body ->
        "(fn (#{name} #{type}) #{body})"
      end)

    evaluated = if scope == [], do: uses, else: []

    source = """
    (module FragmentProbe (edition 0.1) (revision 0.1.8) (origin "test://fragment-probe")
      (export value c047_fragment_probe)
      (def c047_fragment_probe (signature #{signature} (uses #{Enum.join(evaluated, " ")})) #{body}))
    """

    with {:ok, decoded} <- Parser.parse(source),
         probe <- hd(decoded.definitions),
         candidate <- %{
           parsed
           | definitions: globals ++ [probe],
             processes: [],
             imports: [],
             instances: [],
             exports: %{parsed.exports | values: [probe.name], processes: []}
         },
         {:ok, module} <- CapabilityKernel.prepare(candidate, bindings),
         {:ok, _core} <- Checker.check(module) do
      :ok
    else
      {:error, diagnostic} -> {:error, %{diagnostic | path: path}}
    end
  end

  defp function_type([], result, _uses), do: result

  defp function_type([type], result, uses),
    do: "(Fn #{type} (effects #{Enum.join(uses, " ")}) #{result})"

  defp function_type([type | rest], result, uses),
    do: "(Fn #{type} (effects) #{function_type(rest, result, uses)})"

  defp enclose(_source, parsed, []) do
    {:ok, parsed}
  end

  defp enclose(source, parsed, handlers) do
    main = Enum.find(parsed.definitions, &(&1.name == "main"))
    span = main.expression.span
    expression = binary_part(source, span.byte_start, span.byte_end - span.byte_start)

    wrapped =
      Enum.reduce(Enum.reverse(handlers), expression, fn handler, body ->
        "(handle #{handler} #{body})"
      end)

    source =
      binary_part(source, 0, span.byte_start) <>
        wrapped <> binary_part(source, span.byte_end, byte_size(source) - span.byte_end)

    Parser.parse(source)
  end

  defp annotate(parsed, spec, contexts, handlers) do
    prefix = Macro.underscore(spec.module)

    rows =
      spec.qualifiers
      |> Enum.with_index()
      |> Enum.filter(fn {{kind, _}, _} -> kind in [:generator, :case_generator] end)
      |> Enum.with_index(1)
      |> Map.new(fn {{_generator, index}, depth} ->
        suffix = Enum.drop(spec.qualifiers, index + 1)

        uses =
          spec.yield_uses ++
            Enum.flat_map(suffix, fn {_, fields} -> Keyword.get(fields, :uses, []) end)

        {"#{prefix}_go#{depth}", effects(uses)}
      end)

    context_rows = Map.new(contexts, fn {name, _, _, uses} -> {name, effects(uses)} end)

    main_uses =
      Enum.reduce(handlers, effects(spec.uses), fn name, remaining ->
        handler = Enum.find(parsed.handlers, &(&1.name == name)) || raise ArgumentError
        List.delete(remaining, {:effect, handler.effect})
      end)

    definitions =
      Enum.map(parsed.definitions, fn definition ->
        cond do
          Map.has_key?(rows, definition.name) ->
            %{
              definition
              | signature: final_arrow(definition.signature, rows[definition.name]),
                uses: []
            }

          Map.has_key?(context_rows, definition.name) ->
            %{definition | uses: context_rows[definition.name]}

          definition.name == "main" ->
            signature =
              case handlers do
                [] -> definition.signature
                [outer | _] -> Enum.find(parsed.handlers, &(&1.name == outer)).output
              end

            %{definition | signature: signature, uses: main_uses}

          true ->
            definition
        end
      end)

    {:ok, %{parsed | definitions: definitions}}
  end

  defp final_arrow({:function, parameter, _, {:function, _, _, _} = result}, effects),
    do: {:function, parameter, [], final_arrow(result, effects)}

  defp final_arrow({:function, parameter, _, result}, effects),
    do: {:function, parameter, effects, result}

  defp error(message, path), do: {:error, Diagnostic.new("T002", message, path: path)}
end
