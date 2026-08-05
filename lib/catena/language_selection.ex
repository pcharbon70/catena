defmodule Catena.LanguageSelection do
  @moduledoc "A resolved package-local Catena language selection."

  @enforce_keys [:edition, :language_revision, :previews]
  defstruct [:edition, :language_revision, :previews]

  @type t :: %__MODULE__{
          edition: String.t(),
          language_revision: String.t(),
          previews: [String.t()]
        }

  @spec to_map(t()) :: map()
  def to_map(%__MODULE__{} = selection) do
    %{
      "edition" => selection.edition,
      "language_revision" => selection.language_revision,
      "previews" => selection.previews
    }
  end
end
