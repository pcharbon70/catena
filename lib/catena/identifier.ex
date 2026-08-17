defmodule Catena.Identifier do
  @moduledoc "A validated Catena 0.1.10 identifier segment."

  alias Catena.{Diagnostic, QualifiedName}

  @enforce_keys [
    :source,
    :canonical,
    :escaped,
    :span,
    :scripts,
    :skeleton,
    :selection
  ]
  defstruct @enforce_keys

  @type t :: %__MODULE__{
          source: String.t(),
          canonical: String.t(),
          escaped: boolean(),
          span: Catena.SourceSpan.t(),
          scripts: [String.t()],
          skeleton: String.t(),
          selection: Catena.LanguageSelection.t()
        }

  @spec parse(binary(), keyword()) :: {:ok, t()} | {:error, Diagnostic.t()}
  def parse(source, options \\ []) when is_binary(source) and is_list(options) do
    case QualifiedName.parse(source, options) do
      {:ok, %{segments: [identifier]}} ->
        {:ok, identifier}

      {:ok, qualified} ->
        {:error,
         Diagnostic.new("IDN006", "an identifier must contain exactly one segment",
           span: qualified.span,
           details: %{segment_count: length(qualified.segments)}
         )}

      {:error, %{id: "IDN006"} = diagnostic} when source == "" ->
        {:error, %{diagnostic | id: "IDN001", message: "an identifier must not be empty"}}

      {:error, _diagnostic} = error ->
        error
    end
  end
end
