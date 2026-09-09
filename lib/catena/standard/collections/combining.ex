defmodule Catena.Standard.Collections.Combining do
  @moduledoc "Closed associative Int combination evidence; no sampled law certification."
  def new(operation) when operation in [:integer_sum, :integer_minimum, :integer_maximum],
    do:
      {:ok,
       %{
         version: "0.1.65",
         operation: operation,
         schema: :integer,
         associative: true,
         commutative: true
       }}

  def new(_), do: {:error, :unsupported_combining_law}

  def verify(evidence) do
    case new(evidence.operation) do
      {:ok, ^evidence} -> :ok
      _ -> {:error, :invalid_combining_evidence}
    end
  rescue
    _ -> {:error, :invalid_combining_evidence}
  end

  def apply(%{operation: :integer_sum}, a, b), do: a + b
  def apply(%{operation: :integer_minimum}, a, b), do: min(a, b)
  def apply(%{operation: :integer_maximum}, a, b), do: max(a, b)
end
