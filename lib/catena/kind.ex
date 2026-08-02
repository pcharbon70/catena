defmodule Catena.Kind do
  @moduledoc "Kind parsing and checking for the bounded Catena 0.4 constructor hierarchy."

  alias Catena.Diagnostic

  @type t :: :type | {:arrow, t(), t()}

  @spec parse!(String.t(), String.t() | nil) :: t()
  def parse!(value, path \\ nil)

  def parse!(value, path) when is_binary(value) do
    tokens = Regex.scan(~r/Type|->/, value) |> List.flatten()

    normalized =
      value
      |> String.replace(~r/\s+/, "")

    if Enum.join(tokens, "") != normalized do
      fail("TRT002", "unsupported kind #{inspect(value)}", path)
    end

    case parse_tokens(tokens) do
      {:ok, kind, []} -> kind
      _ -> fail("TRT002", "malformed kind #{inspect(value)}", path)
    end
  end

  def parse!(value, path),
    do: fail("TRT002", "kind must be a string, got #{inspect(value)}", path)

  @spec encode(t()) :: String.t()
  def encode(:type), do: "Type"
  def encode({:arrow, left, right}), do: encode_operand(left) <> " -> " <> encode(right)

  @spec arity(t()) :: non_neg_integer()
  def arity(:type), do: 0
  def arity({:arrow, _left, right}), do: 1 + arity(right)

  @spec apply!(t(), t(), String.t() | nil) :: t()
  def apply!({:arrow, expected, result}, actual, _path) when expected == actual, do: result

  def apply!({:arrow, expected, _result}, actual, path),
    do:
      fail(
        "TRT002",
        "constructor expects #{encode(expected)} but received #{encode(actual)}",
        path
      )

  def apply!(kind, _actual, path),
    do: fail("TRT002", "cannot apply a constructor of kind #{encode(kind)}", path)

  defp parse_tokens(["Type" | rest]) do
    case rest do
      ["->" | tail] ->
        with {:ok, right, remaining} <- parse_tokens(tail),
             do: {:ok, {:arrow, :type, right}, remaining}

      _ ->
        {:ok, :type, rest}
    end
  end

  defp parse_tokens(_tokens), do: :error

  defp encode_operand(:type), do: "Type"
  defp encode_operand(kind), do: "(" <> encode(kind) <> ")"

  defp fail(id, message, path) do
    raise Catena.TypeError, diagnostic: Diagnostic.new(id, message, path: path)
  end
end
