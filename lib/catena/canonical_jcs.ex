defmodule Catena.CanonicalJCS do
  @moduledoc "RFC 8785 canonical JSON with Catena 0.1.6's integer-only signed profile."

  alias Catena.{Diagnostic, LanguageVersion}

  @governance_version LanguageVersion.introduced(:specifications_and_governance)

  @safe_integer 9_007_199_254_740_991

  @spec decode(binary(), keyword()) :: {:ok, term()} | {:error, Diagnostic.t()}
  def decode(binary, options \\ []) when is_binary(binary) do
    object_push = fn key, value, entries ->
      if Enum.any?(entries, fn {existing, _value} -> existing == key end) do
        throw({:jcs_error, "duplicate object name #{inspect(key)}"})
      end

      [{key, value} | entries]
    end

    object_finish = fn entries, old -> {Map.new(entries), old} end

    reject_float = fn token ->
      throw({:jcs_error, "floating-point value #{inspect(token)} is forbidden"})
    end

    safe_integer = fn token ->
      if token == "-0" do
        throw({:jcs_error, "negative zero is forbidden"})
      end

      value = String.to_integer(token)

      if abs(value) <= @safe_integer,
        do: value,
        else: throw({:jcs_error, "integer is outside the interoperable safe range"})
    end

    {value, _accumulator, rest} =
      :json.decode(binary, :ok, %{
        object_push: object_push,
        object_finish: object_finish,
        float: reject_float,
        integer: safe_integer,
        null: nil
      })

    if rest != <<>> do
      {:error, Diagnostic.new("EVD001", "trailing bytes after canonical JSON", path: "$")}
    else
      if negative_zero?(binary) do
        {:error, Diagnostic.new("EVD001", "negative zero is forbidden", path: "$")}
      else
        canonical_result(value, binary, options)
      end
    end
  rescue
    error ->
      {:error,
       Diagnostic.new("EVD001", "invalid canonical JSON: #{Exception.message(error)}", path: "$")}
  catch
    {:jcs_error, message} -> {:error, Diagnostic.new("EVD001", message, path: "$")}
  end

  defp canonical_result(value, binary, options) do
    canonical = encode(value)

    if Keyword.get(options, :canonical, false) and binary != canonical do
      {:error,
       Diagnostic.new("EVD001", "signed JSON payload is not in canonical form", path: "$")}
    else
      {:ok, value}
    end
  end

  @spec encode(term()) :: binary()
  def encode(value) when is_map(value) do
    value
    |> Enum.map(fn
      {key, item} when is_binary(key) -> {key, item}
      {key, item} when is_atom(key) -> {Atom.to_string(key), item}
      {key, _item} -> raise ArgumentError, "JCS object name must be a string, got #{inspect(key)}"
    end)
    |> ensure_unique_keys!()
    |> Enum.sort_by(fn {key, _item} -> utf16!(key) end)
    |> Enum.map_join(",", fn {key, item} -> encode_string(key) <> ":" <> encode(item) end)
    |> then(&("{" <> &1 <> "}"))
  end

  def encode(value) when is_list(value),
    do: "[" <> Enum.map_join(value, ",", &encode/1) <> "]"

  def encode(value) when is_binary(value), do: encode_string(value)
  def encode(value) when is_boolean(value) or is_nil(value), do: JSON.encode!(value)

  def encode(value) when is_integer(value) and abs(value) <= @safe_integer,
    do: Integer.to_string(value)

  def encode(value) when is_integer(value),
    do: raise(ArgumentError, "JCS integer #{value} is outside Catena's safe range")

  def encode(value) when is_float(value),
    do: raise(ArgumentError, "floating-point values are forbidden in Catena signed JSON")

  def encode(value), do: raise(ArgumentError, "unsupported JCS value #{inspect(value)}")

  @spec digest(term()) :: String.t()
  def digest(value),
    do: :crypto.hash(:sha256, encode(value)) |> Base.encode16(case: :lower)

  @spec payload(String.t(), term()) :: binary()
  def payload(kind, value)
      when kind in ~w(root delegation evidence approval transition manifest) do
    "catena:#{kind}:#{@governance_version}\n" <> encode(value)
  end

  defp ensure_unique_keys!(entries) do
    keys = Enum.map(entries, &elem(&1, 0))

    if length(keys) == length(Enum.uniq(keys)),
      do: entries,
      else: raise(ArgumentError, "duplicate object name after key conversion")
  end

  defp utf16!(value) do
    case :unicode.characters_to_binary(value, :utf8, {:utf16, :big}) do
      binary when is_binary(binary) -> binary
      _ -> raise ArgumentError, "invalid Unicode string"
    end
  end

  defp encode_string(value) do
    _ = utf16!(value)
    JSON.encode!(value)
  end

  defp negative_zero?(binary), do: scan_negative_zero(binary, false, false)

  defp scan_negative_zero(<<>>, _in_string, _escaped), do: false

  defp scan_negative_zero(<<byte, rest::binary>>, true, escaped) do
    cond do
      escaped -> scan_negative_zero(rest, true, false)
      byte == ?\\ -> scan_negative_zero(rest, true, true)
      byte == ?" -> scan_negative_zero(rest, false, false)
      true -> scan_negative_zero(rest, true, false)
    end
  end

  defp scan_negative_zero(<<?", rest::binary>>, false, false),
    do: scan_negative_zero(rest, true, false)

  defp scan_negative_zero(<<?-, ?0, rest::binary>>, false, false) do
    case rest do
      <<>> -> true
      <<next, _::binary>> when next in [32, 9, 10, 13, ?,, ?], ?}] -> true
      _ -> scan_negative_zero(rest, false, false)
    end
  end

  defp scan_negative_zero(<<_byte, rest::binary>>, false, false),
    do: scan_negative_zero(rest, false, false)
end
