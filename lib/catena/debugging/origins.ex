defmodule Catena.Debugging.Origins do
  @moduledoc false
  @key {__MODULE__, :context}

  def capture(path, source, json_paths, fun) do
    previous = Process.get(@key)

    Process.put(@key, %{
      path: path,
      source: source,
      json_paths: json_paths,
      entries: %{},
      reverse: %{},
      next: 1
    })

    try do
      forms = fun.()
      {forms, Process.get(@key).entries}
    after
      if previous, do: Process.put(@key, previous), else: Process.delete(@key)
    end
  end

  def annotation(node, fallback) do
    case Process.get(@key) do
      nil ->
        fallback

      context ->
        span =
          case node do
            %Catena.SourceSpan{} = span -> span
            %{span: %Catena.SourceSpan{} = span} -> span
            %{path: path} -> Map.get(context.json_paths, path)
            _ -> nil
          end

        path = if is_map(node), do: Map.get(node, :path), else: nil

        source = %{
          path: context.path,
          locator: path,
          span: span && Catena.SourceSpan.to_map(span)
        }

        case Map.fetch(context.reverse, source) do
          {:ok, id} ->
            id

          :error ->
            Process.put(@key, %{
              context
              | next: context.next + 1,
                entries: Map.put(context.entries, context.next, source),
                reverse: Map.put(context.reverse, source, context.next)
            })

            context.next
        end
    end
  end
end
