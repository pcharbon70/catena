defmodule Catena.Standard.Numeric.Primitive do
  @moduledoc "Revision 0.1.67 primitive arithmetic preserves result types and classifies overflow as a trap."
  alias Catena.Standard.Numeric.Binary64
  def reason, do: {:catena_variant, :arithmetic, {:catena_variant, :overflow, :unit}}

  def apply(op, a, b) when is_float(a) and is_float(b) do
    case Binary64.operate(op, a, b) do
      {:ok, value} -> value
      {:error, :overflow} -> :erlang.error({:catena_trap, reason()})
    end
  end

  def apply(:add, a, b) when is_integer(a) and is_integer(b), do: a + b
  def apply(:subtract, a, b) when is_integer(a) and is_integer(b), do: a - b
  def apply(:multiply, a, b) when is_integer(a) and is_integer(b), do: a * b
end
