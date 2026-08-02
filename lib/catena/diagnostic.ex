defmodule Catena.Diagnostic do
  @moduledoc "A stable machine-readable compiler diagnostic."

  @enforce_keys [:id, :message]
  defstruct [:id, :message, :path, details: %{}]

  @type t :: %__MODULE__{
          id: String.t(),
          message: String.t(),
          path: String.t() | nil,
          details: map()
        }

  @spec new(String.t(), String.t(), keyword()) :: t()
  def new(id, message, options \\ []) do
    %__MODULE__{
      id: id,
      message: message,
      path: Keyword.get(options, :path),
      details: Map.new(Keyword.get(options, :details, %{}))
    }
  end
end
