defmodule Catena.ConformanceInfo do
  @moduledoc "Deterministic machine-readable bootstrap conformance profile."

  alias Catena.{ImplementationLimits, LanguageVersion}

  @spec document() :: map()
  def document do
    %{
      "format" => "catena-conformance-info",
      "version" => 1,
      "implementation" => %{
        "name" => "Catena Elixir bootstrap compiler",
        "release" => Application.spec(:catena, :vsn) |> to_string(),
        "target" => "BEAM through OTP 29 Erlang Abstract Format"
      },
      "language" => %{
        "edition" => "0.1",
        "current_revision" => LanguageVersion.latest(),
        "supported_revisions" => LanguageVersion.all()
      },
      "implementation_defined_choices" => [],
      "vendor_extensions" => [],
      "permissions" => permissions(),
      "recommendations" => recommendations(),
      "bounded_presentations" => bounded_presentations(),
      "limits" => Map.new(ImplementationLimits.all(), &limit_entry/1),
      "runtime_capacities" => %{
        "mailbox" => %{
          "capacity" => "deployment-defined",
          "storage" => "runtime-configured",
          "semantic_constraints" => [
            "resource pressure does not authorize message reordering",
            "resource pressure does not authorize retargeting",
            "resource pressure does not authorize silent live-target message loss"
          ],
          "policy_owner" => "G068/G129"
        }
      }
    }
  end

  defp limit_entry(limit) do
    {Atom.to_string(limit.id),
     %{
       "classification" => Atom.to_string(limit.classification),
       "unit" => Atom.to_string(limit.unit),
       "portable_minimum" => limit.portable_minimum,
       "configured" => limit.configured,
       "applies_to" => limit.applies_to,
       "exhaustion" => stringify(limit.exhaustion)
     }}
  end

  defp stringify(value) when is_map(value),
    do: Map.new(value, fn {key, item} -> {Atom.to_string(key), stringify(item)} end)

  defp stringify(value) when is_atom(value), do: Atom.to_string(value)
  defp stringify(value), do: value

  defp permissions do
    [
      %{"id" => "interface-consumption", "disposition" => "enabled"},
      %{"id" => "adt-layout", "disposition" => "compact-default-uniform-selectable"},
      %{"id" => "gadt-coverage-equalities", "disposition" => "enabled"},
      %{"id" => "claim-summaries", "disposition" => "emitted"}
    ]
  end

  defp recommendations do
    [
      %{"id" => "secondary-diagnostic-spans", "disposition" => "partial", "owner" => "P117"},
      %{"id" => "clause-condition-wording", "disposition" => "deviation", "owner" => "P117"},
      %{"id" => "shared-pattern-matrices", "disposition" => "deviation", "owner" => "G138"},
      %{"id" => "original-source-locations", "disposition" => "kernel-only", "owner" => "P117"},
      %{
        "id" => "stale-preview-removal-edit",
        "disposition" => "not-implemented",
        "owner" => "P125"
      }
    ]
  end

  defp bounded_presentations do
    [
      %{"id" => "fresh-type-variable-spelling", "bound" => "alpha-equivalence"},
      %{"id" => "diagnostic-technical-detail-order", "bound" => "stable identity and repair"}
    ]
  end
end
