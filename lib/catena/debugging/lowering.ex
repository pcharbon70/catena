defmodule Catena.Debugging.Lowering do
  @moduledoc false
  alias Catena.Calling.Descriptor

  @ast ~w(function clause var atom integer float char string nil cons tuple map map_field_assoc map_field_exact bin bin_element op call remote fun named_fun match block case if receive try catch lc bc generate b_generate record record_field record_index)a

  def build(forms, origins, source_digest, profile, module) do
    functions =
      Map.new(
        for {:function, _, name, 0, [{:clause, _, [], [], [body]}]} <- forms,
            eligible?(body),
            do: {name, body}
      )

    state = %{
      next: 1,
      entries: %{},
      origins: origins,
      source_digest: source_digest,
      profile: profile,
      functions: functions
    }

    file = "catena-debug://" <> module

    {forms, state} =
      Enum.map_reduce(forms, state, fn
        {:attribute, a, :file, _}, state ->
          {{:attribute, a, :file, {String.to_charlist(file), 1}}, state}

        {:attribute, _, :catena_calling_origins, _}, state ->
          {nil, state}

        {:function, _, name, arity, _} = form, state ->
          walk(form, state, [{:function, Atom.to_string(name), arity}], [], [], 0)

        {:attribute, _, key, value}, state ->
          {{:attribute, 1, key, value}, state}

        form, state ->
          {form, state}
      end)

    forms = Enum.reject(forms, &is_nil/1)
    runtime_forms = Enum.reject(forms, &match?({:attribute, _, :file, _}, &1))
    file = file <> "/" <> Descriptor.digest(runtime_forms)

    forms =
      Enum.map(forms, fn
        {:attribute, _, :file, _} -> {:attribute, 1, :file, {String.to_charlist(file), 1}}
        form -> form
      end)

    {forms, state.entries, file}
  end

  defp walk({:call, annotation, {:atom, _, name}, []} = node, state, chain, inline, path, depth) do
    if state.profile.inline_depth > depth and Map.has_key?(state.functions, name) do
      origin = Map.get(state.origins, :erl_anno.line(annotation))

      walk(
        state.functions[name],
        state,
        chain,
        inline ++ [%{kind: :inline, call_site: origin, target: Atom.to_string(name)}],
        path ++ [:inline],
        depth + 1
      )
    else
      ast(node, state, chain, inline, path, depth)
    end
  end

  defp walk(node, state, chain, inline, path, depth) when is_tuple(node),
    do: ast(node, state, chain, inline, path, depth)

  defp walk(nodes, state, chain, inline, path, depth) when is_list(nodes) do
    nodes
    |> Enum.with_index()
    |> Enum.map_reduce(state, fn {node, index}, state ->
      walk(node, state, chain, inline, path ++ [index], depth)
    end)
  end

  defp walk(node, state, _, _, _, _), do: {node, state}

  defp ast(node, state, chain, inline, path, depth) do
    elements = Tuple.to_list(node)

    case elements do
      [tag, annotation | rest]
      when tag in @ast and (is_integer(annotation) or is_list(annotation) or is_tuple(annotation)) ->
        if state.next > state.profile.max_nodes, do: throw(:debug_node_limit)
        origin = Map.get(state.origins, :erl_anno.line(annotation))
        generated = chain ++ [%{kind: :generated, operation: tag}]
        details = inline ++ generated

        record = %{
          id:
            Descriptor.digest({:lowered_node, state.source_digest, path, chain, inline, origin}),
          function: hd(chain),
          source_id:
            origin &&
              Catena.Kernel.Node.origin_id(state.source_digest, origin.span, origin.locator),
          primary: origin,
          chain: Enum.take(details, state.profile.max_chain),
          omitted: max(length(details) - state.profile.max_chain, 0)
        }

        line = state.next
        state = %{state | next: line + 1, entries: Map.put(state.entries, line, record)}
        next_chain = if tag in [:fun, :named_fun, :receive, :try], do: generated, else: chain
        {rest, state} = walk(rest, state, next_chain, inline, path ++ [tag], depth)
        {List.to_tuple([tag, line | rest]), state}

      _ ->
        {elements, state} = walk(elements, state, chain, inline, path, depth)
        {List.to_tuple(elements), state}
    end
  end

  # Only closed, zero-argument expression bodies are substituted. Binding and
  # control forms stay in their original scope; recursion is depth-bounded.
  defp eligible?({tag, _, _}) when tag in [:atom, :integer, :float, :char, :string], do: true
  defp eligible?({nil, _}), do: true
  defp eligible?({:call, _, {:atom, _, _}, arguments}), do: Enum.all?(arguments, &eligible?/1)

  defp eligible?({:call, _, {:remote, _, {:atom, _, _}, {:atom, _, _}}, arguments}),
    do: Enum.all?(arguments, &eligible?/1)

  defp eligible?({:op, _, _, value}), do: eligible?(value)
  defp eligible?({:op, _, _, a, b}), do: eligible?(a) and eligible?(b)
  defp eligible?({:tuple, _, values}), do: Enum.all?(values, &eligible?/1)
  defp eligible?(_), do: false
end
