defmodule Catena.Diagnostic.Contract do
  @moduledoc "Bounded validation and source-level presentation for structured diagnostics."

  alias Catena.{Diagnostic, SourceSpan, Type}

  @version "0.1.91"
  @maximum_related 8
  @maximum_provenance 32
  @maximum_type_bytes 512
  @applicability ~w(machine_applicable maybe_incorrect unspecified)a

  def profile do
    %{
      version: @version,
      maximum_related_locations: @maximum_related,
      maximum_constraint_causes: @maximum_provenance,
      maximum_presented_type_bytes: @maximum_type_bytes,
      edit_applicability: @applicability,
      parse_diagnostics: :held_for_p109,
      generated_origins: :exact_digest_bound,
      coordinates: :unicode_scalar_half_open
    }
  end

  def present_type(type) do
    normalized = type |> Type.normalize() |> inspect(limit: :infinity, printable_limit: :infinity)
    digest = :crypto.hash(:sha256, normalized) |> Base.encode16(case: :lower)

    if byte_size(normalized) <= @maximum_type_bytes do
      %{text: normalized, truncated: false, digest: digest}
    else
      prefix = binary_part(normalized, 0, @maximum_type_bytes - 3)
      %{text: prefix <> "...", truncated: true, digest: digest}
    end
  end

  def validate(diagnostic, source \\ nil)

  def validate(%Diagnostic{} = diagnostic, source) do
    with true <- valid_identity?(diagnostic),
         true <- length(diagnostic.related) <= @maximum_related,
         true <- Enum.all?(diagnostic.related, &valid_location?(&1, source)),
         true <- valid_provenance?(diagnostic.provenance),
         true <- valid_generated_origin?(diagnostic.explanation, source),
         true <- valid_fixes?(diagnostic.fixes, source) do
      {:ok, diagnostic}
    else
      false -> {:error, :invalid_diagnostic_contract}
    end
  end

  def validate(_, _), do: {:error, :invalid_diagnostic_contract}

  defp valid_identity?(diagnostic),
    do:
      is_binary(diagnostic.id) and diagnostic.id != "" and is_binary(diagnostic.message) and
        diagnostic.severity in [:error, :warning]

  defp valid_location?(%{label: label, span: %SourceSpan{} = span}, source),
    do: is_binary(label) and valid_span?(span, source)

  defp valid_location?(_, _), do: false

  defp valid_span?(span, nil), do: span.byte_start <= span.byte_end

  defp valid_span?(span, source) when is_binary(source) do
    match?({:ok, ^span}, SourceSpan.from_bytes(source, span.byte_start, span.byte_end))
  end

  defp valid_provenance?(steps) when is_list(steps) and length(steps) <= @maximum_provenance do
    ids = Enum.map(steps, &Map.get(&1, :id))

    ids == Enum.uniq(ids) and
      Enum.all?(steps, fn step ->
        is_binary(step[:id]) and step[:id] != "" and is_atom(step[:relation]) and
          (is_nil(step[:path]) or is_binary(step[:path]))
      end)
  end

  defp valid_provenance?(_), do: false

  defp valid_generated_origin?(%{generated_origin: origin}, source) do
    is_map(origin) and is_binary(origin[:node]) and digest?(origin[:digest]) and
      match?(%SourceSpan{}, origin[:span]) and valid_span?(origin[:span], source)
  end

  defp valid_generated_origin?(_, _), do: true

  defp valid_fixes?([], _source), do: true
  defp valid_fixes?(_fixes, nil), do: false

  defp valid_fixes?(fixes, source) when is_list(fixes) and is_binary(source) do
    digest = :crypto.hash(:sha256, source) |> Base.encode16(case: :lower)

    with true <-
           Enum.all?(fixes, fn fix ->
             fix[:applicability] in @applicability and fix[:preimage_digest] == digest and
               is_binary(fix[:replacement]) and String.valid?(fix[:replacement]) and
               match?({:ok, _}, SourceSpan.from_bytes(source, fix[:byte_start], fix[:byte_end]))
           end) do
      ranges = fixes |> Enum.map(&{&1[:byte_start], &1[:byte_end]}) |> Enum.sort()
      Enum.chunk_every(ranges, 2, 1, :discard) |> Enum.all?(fn [{_, a}, {b, _}] -> a <= b end)
    end
  rescue
    _ -> false
  end

  defp valid_fixes?(_, _), do: false
  defp digest?(value), do: is_binary(value) and Regex.match?(~r/^[0-9a-f]{64}$/, value)
end
