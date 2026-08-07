defmodule Catena.Kernel.Node do
  @moduledoc false

  @enforce_keys [:kind, :value, :span]
  defstruct @enforce_keys

  @type kind :: :atom | :string | :list
  @type t :: %__MODULE__{kind: kind(), value: String.t() | [t()], span: Catena.SourceSpan.t()}
end
