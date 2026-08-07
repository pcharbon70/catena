defmodule Catena.Report do
  @moduledoc false

  alias Catena.Type
  alias Catena.Type.Scheme

  def module(core) do
    %{
      version: core.version,
      edition: core.edition,
      language_revision: core.language_revision,
      previews: core.previews,
      diagnostics: Enum.map(Map.get(core, :diagnostics, []), &diagnostic/1),
      module: core.module,
      profile: Atom.to_string(core.profile),
      types:
        Enum.map(Map.get(core, :data, %{types: []}).types, fn type ->
          %{
            id: type.id,
            name: type.name,
            visibility: Atom.to_string(type.visibility),
            inhabitation: Atom.to_string(type.inhabitation),
            variance: Enum.map(type.variance, &Atom.to_string/1),
            positive: type.positive?,
            regular: type.regular?
          }
        end),
      definitions:
        Enum.map(core.definitions, fn definition ->
          %{name: definition.name, scheme: scheme(definition.scheme)}
        end)
    }
  end

  def kernel_module(core) do
    %{
      version: core.version,
      edition: core.edition,
      language_revision: core.language_revision,
      previews: core.previews,
      diagnostics: Enum.map(core.diagnostics, &diagnostic/1),
      module: core.module,
      profile: Atom.to_string(core.profile),
      definitions:
        Enum.map(core.definitions, fn definition ->
          %{
            name: definition.name,
            type: Catena.Kernel.Type.encode(definition.signature),
            uses: kernel_effects(definition.uses)
          }
        end),
      processes:
        Enum.map(core.processes, fn process ->
          %{
            name: process.name,
            mailbox: Catena.Kernel.Type.encode(process.mailbox),
            arity: length(process.parameters),
            public: process.name in core.exports.processes
          }
        end)
    }
  end

  def diagnostic(diagnostic) do
    %{
      id: diagnostic.id,
      message: diagnostic.message,
      path: diagnostic.path,
      span: span(diagnostic.span),
      severity: Atom.to_string(diagnostic.severity),
      details: diagnostic.details,
      fixes: diagnostic.fixes
    }
  end

  defp span(nil), do: nil
  defp span(span), do: Catena.SourceSpan.to_map(span)

  defp scheme(%Scheme{variables: variables, type: type}) do
    %{quantified: length(variables), type: printable(Type.normalize(type))}
  end

  defp printable(value) when is_tuple(value),
    do: value |> Tuple.to_list() |> Enum.map(&printable/1)

  defp printable(value) when is_list(value), do: Enum.map(value, &printable/1)
  defp printable(value) when is_atom(value), do: Atom.to_string(value)
  defp printable(value), do: value

  defp kernel_effects(effects) do
    Enum.map(effects, fn
      :process -> "Process"
      {:effect, name} -> name
    end)
  end
end
