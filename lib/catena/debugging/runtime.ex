defmodule Catena.Debugging.Runtime do
  @moduledoc false
  # An out-of-module identity keeps only the exported foreign-entry boundary
  # live while its argument executes. Recursive workers remain tail calls.
  def identity(value), do: value
end
