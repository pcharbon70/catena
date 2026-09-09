defmodule Catena.Kernel.Node do
  @moduledoc false

  @enforce_keys [:kind, :value, :span]
  defstruct @enforce_keys

  @type kind :: :atom | :string | :list
  @type t :: %__MODULE__{kind: kind(), value: String.t() | [t()], span: Catena.SourceSpan.t()}

  @doc "Content-bound source-node identity; never an executable value."
  def origin_id(source_digest, span, locator) do
    Catena.Calling.Descriptor.digest({:source_node, source_digest, span, locator})
  end
end
