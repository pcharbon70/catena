defmodule Catena.LanguageVersion do
  @moduledoc """
  Canonical prototype language-slice versions.

  Catena's current language line is `0.1`. Each completed semantic slice uses
  the next patch component. Compiler-package releases are versioned
  independently in `mix.exs`.
  """

  @versions [
    type_system: "0.1.1",
    data_and_patterns: "0.1.2",
    clause_conditions: "0.1.3",
    traits_and_categories: "0.1.4",
    effects_and_handlers: "0.1.5",
    specifications_and_governance: "0.1.6"
  ]
  @ordered Keyword.values(@versions)
  @interfaces tl(@ordered)
  @retired ~w(0.1 0.2 0.3 0.4 0.5 0.6)
  @core_semver ~r/^(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)\.(0|[1-9][0-9]*)$/

  @type feature ::
          :type_system
          | :data_and_patterns
          | :clause_conditions
          | :traits_and_categories
          | :effects_and_handlers
          | :specifications_and_governance

  @spec all() :: [String.t()]
  def all, do: @ordered

  @spec interface_versions() :: [String.t()]
  def interface_versions, do: @interfaces

  @spec retired() :: [String.t()]
  def retired, do: @retired

  @spec latest() :: String.t()
  def latest, do: List.last(@ordered)

  @spec introduced(feature()) :: String.t()
  def introduced(feature), do: Keyword.fetch!(@versions, feature)

  @spec from(feature()) :: [String.t()]
  def from(feature) do
    introduced = introduced(feature)
    Enum.drop_while(@ordered, &(&1 != introduced))
  end

  @spec before(feature()) :: [String.t()]
  def before(feature) do
    introduced = introduced(feature)
    Enum.take_while(@ordered, &(&1 != introduced))
  end

  @spec internal_representation(String.t()) :: String.t()
  def internal_representation("0.1.1"), do: "0.1.2"
  def internal_representation(version) when version in @ordered, do: version

  @spec valid_core_semver?(term()) :: boolean()
  def valid_core_semver?(version) when is_binary(version),
    do: Regex.match?(@core_semver, version)

  def valid_core_semver?(_version), do: false
end
