defmodule Catena.Trust.Inventory do
  @moduledoc "Development boundary inventory. Syntax evidence is not a security proof or runtime sandbox."
  @local ~w(apply spawn spawn_link spawn_monitor spawn_opt open_port binary_to_term load_nif noenv_forms)a

  def elixir_calls(source) when is_binary(source) and byte_size(source) <= 2_000_000 do
    with {:ok, ast} <- Code.string_to_quoted(source, columns: false) do
      {_ast, calls} =
        Macro.prewalk(ast, [], fn node, acc ->
          case target(node) do
            nil ->
              {node, acc}

            target ->
              {node, [%{"target" => target, "expression" => digest(Macro.to_string(node))} | acc]}
          end
        end)

      {:ok, Enum.sort_by(calls, &{&1["target"], &1["expression"]})}
    else
      _ -> {:error, :invalid_trust_source}
    end
  end

  def elixir_calls(_), do: {:error, :trust_source_limit}

  defp target({{:., _, [module, function]}, _, args}) when is_atom(function) and is_list(args),
    do:
      Macro.to_string(module) <>
        "." <> Atom.to_string(function) <> "/" <> Integer.to_string(length(args))

  defp target({{:., _, [_]}, _, args}) when is_list(args),
    do: "callback/" <> Integer.to_string(length(args))

  defp target({name, _, args}) when name in @local and is_list(args),
    do: Atom.to_string(name) <> "/" <> Integer.to_string(length(args))

  defp target({name, _, args}) when name in [:alias, :import, :require, :use] and is_list(args),
    do: Atom.to_string(name)

  defp target({:{}, _, [:remote | _]}), do: "generated-remote"
  defp target(_), do: nil

  def digest(value), do: :crypto.hash(:sha256, value) |> Base.encode16(case: :lower)

  def compare(expected, actual) when is_map(expected) and is_map(actual) do
    paths = (Map.keys(expected) ++ Map.keys(actual)) |> Enum.uniq() |> Enum.sort()

    differences =
      Enum.flat_map(paths, fn path ->
        cond do
          not Map.has_key?(expected, path) -> [%{path: path, reason: :unclassified_source}]
          not Map.has_key?(actual, path) -> [%{path: path, reason: :missing_source}]
          expected[path] != actual[path] -> [%{path: path, reason: :changed_boundary_calls}]
          true -> []
        end
      end)

    if differences == [], do: :ok, else: {:error, differences}
  end

  def source_paths(root) do
    [
      "lib/**/*",
      "scripts/**/*",
      "src/**/*",
      "c_src/**/*",
      "config/**/*",
      "priv/**/*.py",
      "mix.exs"
    ]
    |> Enum.flat_map(&Path.wildcard(Path.join(root, &1)))
    |> Enum.filter(&File.regular?/1)
    |> Enum.map(&Path.relative_to(&1, root))
    |> Enum.uniq()
    |> Enum.sort()
  end

  def scan(root) do
    Enum.reduce_while(source_paths(root), {:ok, %{}}, fn path, {:ok, acc} ->
      source = File.read!(Path.join(root, path))

      result =
        cond do
          String.ends_with?(path, ".py") -> python_calls(root, path)
          Path.extname(path) in [".ex", ".exs"] -> elixir_calls(source)
          true -> {:error, :unclassified_trust_source_format}
        end

      case result do
        {:ok, calls} -> {:cont, {:ok, Map.put(acc, path, summarize(calls))}}
        error -> {:halt, error}
      end
    end)
  rescue
    _ -> {:error, :unreadable_trust_source}
  end

  def summarize(calls) do
    %{
      "count" => length(calls),
      "digest" => digest(Catena.CanonicalJSON.encode(calls)),
      "targets" => Enum.frequencies_by(calls, & &1["target"])
    }
  end

  defp python_calls(root, path) do
    case System.cmd(
           "python3",
           ["-I", Path.join(root, "scripts/scan_python_trust.py"), Path.join(root, path)],
           stderr_to_stdout: true
         ) do
      {output, 0} -> JSON.decode(output)
      _ -> {:error, :invalid_python_trust_source}
    end
  end
end
