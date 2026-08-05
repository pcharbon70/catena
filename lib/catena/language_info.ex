defmodule Catena.LanguageInfo do
  @moduledoc "Public deterministic discovery document for Catena language versions."

  alias Catena.{LanguageLifecycle, LanguageSelection, LanguageVersion}

  @format_version LanguageVersion.introduced(:editions_and_feature_lifecycle)

  @spec document() :: map()
  def document do
    features = LanguageLifecycle.features()
    changes = LanguageLifecycle.changes()
    change_ids = MapSet.new(changes, & &1["id"])

    unless LanguageLifecycle.valid_registry?(features) and
             LanguageLifecycle.valid_changes?(changes) and
             Enum.all?(features, &MapSet.member?(change_ids, &1["change"])) do
      raise "invalid built-in Catena language registry"
    end

    %{
      "format" => "catena-language-info",
      "version" => @format_version,
      "current" => LanguageVersion.current_selection() |> LanguageSelection.to_map(),
      "editions" => LanguageVersion.editions(),
      "features" => features,
      "changes" => changes
    }
  end
end
