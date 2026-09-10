defmodule Catena.Diagnostic do
  @moduledoc "A stable machine-readable compiler diagnostic."

  @enforce_keys [:id, :message]
  defstruct [
    :id,
    :message,
    :path,
    :span,
    severity: :error,
    details: %{},
    fixes: [],
    related: [],
    explanation: nil,
    provenance: []
  ]

  @type t :: %__MODULE__{
          id: String.t(),
          message: String.t(),
          path: String.t() | nil,
          span: Catena.SourceSpan.t() | nil,
          severity: :error | :warning,
          details: map(),
          fixes: [map()],
          related: [map()],
          explanation: map() | nil,
          provenance: [map()]
        }

  @spec new(String.t(), String.t(), keyword()) :: t()
  def new(id, message, options \\ []) do
    Catena.Runtime.Secret.redact_diagnostic(%__MODULE__{
      id: id,
      message: message,
      path: Keyword.get(options, :path),
      span: Keyword.get(options, :span),
      severity: Keyword.get(options, :severity, :error),
      details: Map.new(Keyword.get(options, :details, %{})),
      fixes: Keyword.get(options, :fixes, []),
      related: Keyword.get(options, :related, []),
      explanation: Keyword.get(options, :explanation),
      provenance: Keyword.get(options, :provenance, [])
    })
  end
end
