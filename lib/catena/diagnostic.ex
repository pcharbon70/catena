defmodule Catena.Diagnostic do
  @moduledoc "A stable machine-readable compiler diagnostic."

  @enforce_keys [:id, :message]
  defstruct [:id, :message, :path, :span, severity: :error, details: %{}, fixes: []]

  @type t :: %__MODULE__{
          id: String.t(),
          message: String.t(),
          path: String.t() | nil,
          span: Catena.SourceSpan.t() | nil,
          severity: :error | :warning,
          details: map(),
          fixes: [map()]
        }

  @spec new(String.t(), String.t(), keyword()) :: t()
  def new(id, message, options \\ []) do
    %__MODULE__{
      id: id,
      message: message,
      path: Keyword.get(options, :path),
      span: Keyword.get(options, :span),
      severity: Keyword.get(options, :severity, :error),
      details: Map.new(Keyword.get(options, :details, %{})),
      fixes: Keyword.get(options, :fixes, [])
    }
  end
end
